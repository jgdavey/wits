(ns wits.rendering
  (:require [domina :as dom]
            [domina.css :as css]
            [domina.events :as dom-events]
            [io.pedestal.app.render.push :as render]
            [io.pedestal.app.render.events :as events]
            [io.pedestal.app.render.push.templates :as templates]
            [io.pedestal.app.render.push.handlers :as h]
            [io.pedestal.app.render.push.handlers.automatic :as d]
            [io.pedestal.app.messages :as msg]
            [clojure.string :as str])
  (:require-macros [wits.html-templates :as html-templates]))

(def templates (html-templates/wits-templates))

(defn render-template-with-id [template-name id]
  (fn [renderer [_ path] transmitter]
    (let [id (render/new-id! renderer path id)
          html (template-name templates)]
      (dom/append! (dom/by-id id) (html)))))

(defn render-template [template-name initial-value-fn]
  (fn [renderer [_ path :as delta] input-queue]
    (let [parent (render/get-parent-id renderer path)
          id (render/new-id! renderer path)
          html (templates/add-template renderer path (template-name templates))]
      (dom/append! (dom/by-id parent) (html (assoc (initial-value-fn delta) :id id))))))

(defn render-static-template [template-name initial-value-fn]
  (fn [renderer [_ path :as delta] input-queue]
    (let [parent (render/get-parent-id renderer path)
          id (render/new-id! renderer path)
          html (template-name templates)]
      (dom/append! (dom/by-id parent) (html (assoc (initial-value-fn delta) :id id))))))

(defn render-page [template-name]
  (fn [renderer [_ path] transmitter]
    (let [parent (render/get-parent-id renderer path)
          id (render/new-id! renderer path)
          html (templates/add-template renderer path (template-name templates))]
      (dom/append! (dom/by-id parent) (html {:id id})))))

(defn register-dynamic-template-with-id
  ([template-name id]
    (register-dynamic-template-with-id template-name id {:class "hide"}))
  ([template-name id opts]
    (fn [renderer [_ path] transmitter]
      (let [id (render/new-id! renderer path id)
            html (templates/add-template renderer path (template-name templates))]
        (dom/append! (dom/by-id id) (html opts))))))


;; Render Login

(defn add-login-template [renderer [_ path :as delta] input-queue]
  (let [parent (render/get-parent-id renderer path)
        id (render/new-id! renderer path "login")]
    (dom/append! (dom/by-id parent) (str "<div id='" id "'></div>"))))

(defn add-submit-login-handler [renderer [_ path transform-name messages :as delta] input-queue]
  ((render-static-template :login-form (constantly {})) renderer delta input-queue)
  (events/collect-and-send :click "login-button" input-queue transform-name messages
                           {"login-name" :value}))

(defn remove-submit-login-event [_ _ _]
  (events/remove-click-event "login-button")
  (dom/destroy! (dom/by-id "login-form")))

(defn render-login-value [renderer [_ path _ new-value] transmitter]
  (templates/update-t renderer path {:name new-value :class (if new-value "" "hide")}))

(defn add-answer-handler [renderer [_ path name messages] input-queue]
  (events/collect-and-send :click "answer-button" input-queue name messages
                           {"answer" :answer}))

(defn remove-answer-handler [_ _ _]
  (events/remove-click-event "answer-button"))

;; Render Board

(defn render-board [renderer [_ path] _]
  (render/new-id! renderer path "slots"))

(defn render-bid [renderer [_ path] _]
  (let [parent (render/get-parent-id renderer (templates/parent path))
        id (render/new-id! renderer path)
        html (templates/add-template renderer path (:bid templates))
        ul (dom/single-node (css/sel (dom/by-id parent) ".bids"))
        li (html {:id id
                  :bidder (last path)
                  :class (when (= :mine (last path)) "me")})]
    (dom/append! ul li)))

(defn render-value [renderer [_ path _ new-value] input-queue]
  (let [key (last path)]
    (templates/update-t renderer (templates/parent path) {key (str new-value)})))

(defn update-bid [renderer [_ path _ new-value] input-queue]
  (templates/update-t renderer path {:bid (str new-value)}))

(defn render-joined-strings [renderer [_ path old new-value] input-queue]
  (render-value renderer [nil path old (str/join "," new-value)] input-queue))

(defn bid-actions [renderer path]
  (let [slot-node (dom/by-id (render/get-id renderer path))]
    {:add-bid (dom/single-node (css/sel slot-node ".add-bid"))
     :remove-bid (dom/single-node (css/sel slot-node ".remove-bid"))}))

(defn enable-bid-action [renderer [_ path transform-name messages] d]
  (let [action-nodes (bid-actions renderer path)
        node (get action-nodes transform-name)]
    (dom/remove-class! node "hide")
    (case transform-name
      :add-bid (events/send-on :submit node d transform-name
                    (fn [] (let [amount-node (dom/single-node (css/sel node "input"))
                                 amount (.-value amount-node)]
                             (set! (.-value amount-node) "")
                             (msg/fill transform-name messages {:bid amount}))))
      :remove-bid (events/send-on-click node d transform-name messages)
    )))

(defn update-bid [renderer [_ path _ new-value] input-queue]
  (templates/update-t renderer path {:bid (str new-value)}))

(defn update-answer [renderer [_ path _ new-value] input-queue]
  (templates/update-t renderer path {:answer (str new-value)}))

(defn register-path-id [id]
  (fn [renderer [_ path] _]
    (render/new-id! renderer path id)))

(defn disable-bid-action [renderer [_ path transform-name messages] d]
  (let [action-nodes (bid-actions renderer path)
        node (get action-nodes transform-name)]
    (dom/add-class! node "hide")
    (dom-events/unlisten! node)))

(defn render-config []
  [
   ;; Game
   [:node-create  [:wits] (render-page :wits-page)]
   [:node-destroy [:wits] h/default-destroy]
   [:node-create [:board] render-board]
   [:node-destroy [:board] h/default-destroy]
   [:node-create [:board :*]
    (render-template :slot (fn [[_ path]] {}))]
   [:node-destroy [:board :*] h/default-destroy]
   [:value [:board :* :odds] render-value]
   [:value [:board :* :value] render-value]
   [:value [:board :* :answerers] render-joined-strings]
   [:node-create [:board :* :bids :*] render-bid]
   [:node-destroy [:board :* :bids :*] h/default-destroy]
   [:value [:board :* :bids :*] update-bid]
   [:transform-enable [:board :*] enable-bid-action]
   [:transform-disable [:board :*] disable-bid-action]

   ;; Login
   [:node-create  [:login] add-login-template]
   [:node-destroy [:login] h/default-destroy]
   [:node-create  [:login :name] (render-template :login-value (fn [_] {:class "hide"}))]
   [:transform-enable [:login :name] add-submit-login-handler]
   [:transform-disable [:login :name] remove-submit-login-event]
   [:value [:login :name] render-login-value]
   [:node-create  [:login :my-answer] (render-static-template :answer-form (constantly {}))]
   [:node-destroy  [:login :my-answer] h/default-destroy]
   [:transform-enable [:login :my-answer] add-answer-handler]
   [:transform-disable [:login :my-answer] remove-answer-handler]
   [:value [:login :question] (render-static-template :question (constantly {}))]

   ;; Wait
   [:node-create [:wait] (render-page :wait-page)]
   [:node-destroy [:wait] h/default-destroy]
   [:node-create [:wait :answers] (register-path-id "answers")]
   [:node-create [:wait :answers :*] (render-template :wait-answer
                                                      (fn [[_ path]] (let [n (last path)
                                                                           mine? (= :mine n)]
                                                                       {:player (if mine? "Me" n)
                                                                        :class (when mine? "me")})))]
   [:value [:wait :answers :*] update-answer]
   ])
