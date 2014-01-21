(ns ^:shared wits.behavior
  (:require [clojure.string :as string]
            [io.pedestal.app :as app]
            [io.pedestal.app.messages :as msg]
            [io.pedestal.app.dataflow :as d]))

(def odds [6 5 4 3 2 3 4 5])

;; Transforms
(defn swap-transform [_ message]
  (:value message))

(defn- number [string-or-int]
  (let [n (int string-or-int)]
    (when (and string-or-int (not (zero? n)))
      n)))

(defn add-answer-transform [_ message]
  (number (:answer message)))

(defn add-bid-transform [_ message]
  (number (:bid message)))

;; Effects
(defn publish-answer [{:keys [answer name]}]
  (when (and answer name)
    [{msg/type :swap msg/topic [:answers name] :value answer}]))

;; Derives
(defn copy-value [_ value] value)

(defn insert-in-middle [item coll]
  (let [[head tail] (partition (/ (count coll) 2) coll)]
    (concat head [item] tail)))

(defn balance [coll]
  (let [coll (if (even? (count coll))
               (insert-in-middle nil coll)
               coll)]
    (case (count coll)
      7 coll
      5 (concat [nil] coll [nil])
      3 (concat [nil nil] coll [nil nil])
      1 (concat [nil nil nil] coll [nil nil nil]))))

(defn remove-duplicates [coll]
  (reduce (fn [v e]
            (conj v (when (not (some #{e} v)) e))) [] coll))

(defn group-by-val [hash]
  (reduce (fn [h [k v]]
            (assoc h v (conj (get h v []) k)))
          {}
          hash))

(defn prepend [coll val]
  (->> (list* coll)
       (cons val)
       vec))

(defn rank-answers* [answers]
  (-> answers
      vals
      set
      sort
      vec
      balance
      (prepend nil)))

(defn rank-answers [_ answers]
  (->> answers
       rank-answers*
       (zipmap (range))))

(defn bids-at [bids space]
  (into {} (for [[bidder spaces] bids
                 [s b] spaces
                 :when (= space s)]
             [bidder b])))

(defn player-can-bid [vals]
  (< (count (filter identity vals)) 2))

(defn biddable [bids space val?]
  (if (get bids space)
    :remove
    (if (and (player-can-bid (vals bids)) val?)
      :add
      :none)))

(defn create-board [_ {:keys [answers spaces bids]}]
  (let [grouped-answers (group-by-val answers)]
    (->> spaces
         (into (sorted-map))
         (map (fn [[s v]]
                {:value v
                 :answerers (get grouped-answers v)
                 :bids (bids-at bids s)
                 :biddable (biddable (:mine bids) s v)
                 :odds (get odds s)}))
         (zipmap (range))
         (into (sorted-map)))))

;; Emitters
; login
(defn init-login [_]
  [{:login
    {:name
     {:transforms
      {:login [{msg/topic [:login :name] msg/type :swap (msg/param :value) {}}]}}}}])

(defn- login-name-deltas [value]
  [[:node-create [:login :name] :map]
   [:value [:login :name] value]
   [:transform-disable [:login :name] :login]
   [:node-create [:login :my-answer] :map]
   [:transform-enable [:login :my-answer] :add-answer
    [{msg/topic [:answers :mine] (msg/param :answer) {}}]]])

(defn- my-answer-deltas [value]
  [[:node-create [:login :my-answer] :map]
   [:value [:login :my-answer] value]])

(defn login-emitter [inputs]
  (reduce (fn [a [input-path new-value]]
            (concat a (case input-path
                        [:login :name] (login-name-deltas new-value)
                        [:answers :mine] (my-answer-deltas new-value)
                        [])))
          []
          (merge (d/added-inputs inputs) (d/updated-inputs inputs))))

(defn emit-value [path value]
  [[:node-create path]
   [:value path value]])

(defn remove-node [path]
  [[:value path nil] [:node-destroy path]])

(defn emit-map [path value]
  (mapcat (fn [[p v]]
            (let [pp (conj path p)]
              (if (nil? v)
                (remove-node pp)
                (emit-value pp v)))) value))

(defn emit-biddability [path value]
  (let [p (last path)
        add [{msg/topic [:bids :mine p] (msg/param :bid) {}}]
        remove [{msg/topic [:bids :mine p]}]]
    (case value
      :add [[:transform-disable path :remove-bid remove]
            [:transform-enable path :add-bid add]]
      :remove [[:transform-enable path :remove-bid remove]
               [:transform-disable path :add-bid add]]
      :none [[:transform-disable path :remove-bid remove]
             [:transform-disable path :add-bid add]])))

(defn board-deltas [path value]
  (if (map? value)
    (emit-map path value)
    (emit-value path value)))

(defn board-emitter [inputs]
  (let [ins (merge (d/added-inputs inputs) (d/updated-inputs inputs) (d/removed-inputs inputs))]
    (mapcat (fn [[path value]]
              (case (last path)
                :biddable (emit-biddability (vec (butlast path)) value)
                (board-deltas path value))) ins)))

;; Continue
; wait screen
(defn init-wait [_]
  (let [start-game {msg/type :swap msg/topic [:active-game] :value true}]
    [{:wait
     {:start
      {:transforms
       {:start-game [{msg/topic msg/effect :payload start-game}
                     start-game]}}}}]))

(defn start-game [inputs]
  (let [active (d/old-and-new inputs [:active-game])
        login (d/old-and-new inputs [:login :name])]
    (when (or (and (:new login) (not (:old active)) (:new active))
              (and (:new active) (not (:old login)) (:new login)))
      [^:input {msg/topic msg/app-model msg/type :set-focus :name :game}])))

(defn wait-for-other-answers [inputs]
  (let [answer (d/old-and-new inputs [:answers :mine])
        login (d/old-and-new inputs [:login :name])]
    (when (or (and (:new login) (not (:old answer)) (:new answer))
              (and (:new answer) (not (:old login)) (:new login)))
      [^:input {msg/topic msg/app-model msg/type :set-focus :name :wait}])))


; main game
(defn init-game [app]
  (let [answers (keys (get-in app [:new-model :board]))
        finish-game {msg/type :swap msg/topic [:active-game] :value false}]
    (concat [[:node-create [:wits :finish]]
             [:transform-enable [:wits :finish] :finish-game [{msg/type msg/effect :payload finish-game} finish-game]]
             [:node-create [:board] :vector]]

            (mapcat (fn [p] [[:node-create [:board p]]
                             [:node-create [:board p :bids] :map]]) answers))))

(defn finish-game [inputs]
  (let [active (d/old-and-new inputs [:active-game])]
    (when (and (:old active) (not (:new active)))
      [^:input {msg/topic msg/app-model msg/type :set-focus :name :score}])))


; scoreboard
(defn init-scoreboard [])

;; Data Model Paths:
;; [:system :question] - original question
;; [:system :answer]   - correct answer
;; [:player :name]      - Nickname for user
;; [:answers :mine]
;; [:answers :*]
;; [:bids :mine]
;; [:bids :*]

;; App Model Paths:
;; [:login :name]        - Displays user's login
;; [:login :my-answer]   - Provide an answer
;; [:wits :answers]      - All answers, by player
;; [:wits :bids]         - All bids, by player
;; [:scoreboard :scores] - All scores, by player

(def example-app
  {:version 2
   :debug true
   :transform [[:swap       [:**]            swap-transform]
               [:add-answer [:answers :mine] add-answer-transform]
               [:add-bid    [:bids :**]      add-bid-transform]
               [:remove-bid [:bids :**]      (constantly nil)]
               [:debug      [:pedestal :**]  swap-transform]]
   :continue #{[#{[:active-game]} start-game]
               [#{[:active-game]} finish-game]
               [#{[:login :name] [:answers :mine]} wait-for-other-answers]}
   :derive #{[#{[:login :name]} [:player :name] copy-value :single-val]
             [#{[:answers]} [:ranked-answers] rank-answers :single-val]
             [{[:ranked-answers] :spaces [:answers] :answers [:bids] :bids}
              [:board] create-board :map]}
   :effect #{[{[:answers :mine] :answer [:player :name] :name} publish-answer :map]}
   :emit [{:init init-login}
          [#{[:login :*] [:answers :mine]} login-emitter]
          {:init init-wait}
          {:in #{[:player :*] [:answers :*]} :fn (app/default-emitter [:wait]) :mode :always}
          {:init init-game}
          {:in #{[:board :* :*]} :fn board-emitter :mode :always}
          {:in #{[:player :*]} :fn (app/default-emitter [:wits]) :mode :always}
          {:init init-scoreboard}
          {:in #{[:scores :*]} :fn (app/default-emitter [:scoreboard]) :mode :always}]
   :focus {:login  [[:login]]
           :wait   [[:wait]]
           :game   [[:wits] [:board] [:pedestal]]
           :score  [[:scoreboard]]
           :default :login}})
