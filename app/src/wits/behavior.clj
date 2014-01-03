(ns ^:shared wits.behavior
    (:require [clojure.string :as string]
              [io.pedestal.app :as app]
              [io.pedestal.app.messages :as msg]
              [io.pedestal.app.dataflow :as dataflow]))

(defn swap-transform [_ message]
  (:value message))

(defn publish-answer [{:keys [answer name]}]
  (when (and answer name)
    [{msg/type :swap msg/topic [:other-answers name] :value answer}]))

(defn- answer [message]
  (int (:answer message)))

(defn add-my-answer-transform [_ message]
  (answer message))

(defn add-bid-transform [_ message]
  (int (:bid message)))

(defn group-by-val [hash]
  (into (sorted-map) (reduce (fn [h [k v]]
                         (assoc h v (conj (get h v []) k)))
                       {}
                       hash)))

(defn merge-answers [_ {:keys [me others login-name]}]
  (assoc others login-name me))

(defn nonzero-value? [[k v]]
  (when (and v (not (zero? v)))
    [k v]))

(defn remove-zeros [nested-hash]
  (reduce (fn [h [k ps]]
            (assoc h k (into {} (filter nonzero-value? ps)))) {} nested-hash))

(defn merge-bids [_ {:keys [mine others login-name]}]
  (let [m (reduce (fn [h [k v]] (assoc h k {login-name v})) {} mine)]
    (remove-zeros (merge-with conj others m))))

(defn sort-answers [_ {:keys [answers]}]
  (group-by-val answers))

(defn start-game [inputs]
  (let [active (dataflow/old-and-new inputs [:active-game])
        login (dataflow/old-and-new inputs [:login :name])]
    (when (or (and (:new login) (not (:old active)) (:new active))
              (and (:new active) (not (:old login)) (:new login)))
      [^:input {msg/topic msg/app-model msg/type :set-focus :name :game}])))

(defn accept-answer [inputs]
  (let [answer (dataflow/old-and-new inputs [:my-answer])]
    (when (and (:new answer) (not (:old answer)))
      [^:input {msg/topic msg/app-model msg/type :set-focus :name :wait}])))

(defn provide-answer [inputs]
  (let [login (dataflow/old-and-new inputs [:login :name])]
    (when (and (:new login) (not (:old login)))
      [^:input {msg/topic msg/app-model msg/type :set-focus :name :answer}])))


(defn init-login [_]
  [[:transform-enable [:login :name]
    :login [{msg/topic [:login :name] msg/type :swap (msg/param :value) {}}]]])

(defn init-wait [_]
  (let [start-game {msg/type :swap msg/topic [:active-game] :value true}]
    [{:wait
     {:start
      {:transforms
       {:start-game [{msg/topic msg/effect :payload start-game}
                     start-game]}}}}]))

(defn init-answer [_]
  [[:transform-enable [:my-answer]
    :add-answer [{msg/topic [:my-answer] (msg/param :answer) {} }]]])

(defn init-game [app]
  (let [answers (keys (get-in app [:new-model :sorted-answers]))]
    [{:wits
      {:my-bids
       (into {} (map (fn [p]
                       [p {:transforms
                           {:add-bid [{msg/topic [:my-bids p] (msg/param :bid) {}}]
                            :remove-bid [{msg/topic [:my-bids p]}]}}])
                     answers))}}]))


;; Data Model Paths:
;; [:login :name] - Nickname for chat user
;; [:players] - All participants
;; [:my-answer]
;; [:other-answers]
;; [:my-bid]
;; [:other-bids]

;; App Model Paths:
;; [:login :name]   - Displays chat user
;; [:wait :*]       - Waiting for all players
;; [:wits :answers] - All answers
;; [:wits :bids]    - All bids


(def example-app
  {:version 2
   :debug true
   :transform [[:swap       [:**]           swap-transform]
               [:add-answer [:my-answer]    add-my-answer-transform]
               [:add-bid    [:my-bids :*]   add-bid-transform]
               [:remove-bid [:my-bids :*]   (constantly nil)]
               [:debug      [:pedestal :**] swap-transform]]
   :continue #{[#{[:active-game]} start-game]
               [#{[:login :name]} provide-answer]
               [#{[:my-answer]}   accept-answer]}
   :derive #{[{[:my-answer] :me [:other-answers] :others [:login :name] :login-name}
              [:answers]
              merge-answers :map]
             [{[:my-bids] :mine [:other-bids] :others [:login :name] :login-name}
              [:bids]
              merge-bids :map]
             [{[:answers] :answers} [:sorted-answers] sort-answers :map]}
   :effect #{[{[:my-answer] :answer [:login :name] :name} publish-answer :map]}
   :emit [{:init init-login}
          [#{[:login :*]} (app/default-emitter [])]
          {:init init-answer}
          [#{[:my-answer]} (app/default-emitter [])]
          {:init init-wait}
          {:in #{[:players :*] [:login :*] [:answers :*]} :fn (app/default-emitter [:wait]) :mode :always}
          {:init init-game}
          [#{[:login :*] [:bids :*] [:my-bids :*] [:sorted-answers :*]} (app/default-emitter [:wits])] ]
   :focus {:login  [[:login]]
           :answer [[:my-answer]]
           :wait   [[:wait]]
           :game   [[:wits] [:pedestal]]
           :default :login}})
