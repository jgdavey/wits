(ns wits.simulated.services
  (:require [io.pedestal.app.protocols :as p]
            [io.pedestal.app.messages :as msg]
            [io.pedestal.app.util.platform :as platform]))

(def answers (atom {"player-abc" 0 "player-xyz" 0}))

(defn provide-answer [key input-queue]
  (let [answer (rand-int 25)
        message {msg/type :swap msg/topic [:other-answers key] :value answer}]
    (update-answer message)
    (p/put-message input-queue message)))

(defn update-answer [message]
  (let [[_ key] (msg/topic message)
        val (:value message)]
    (swap! answers update-in [key] (constantly val))))

(defn provide-bid [player-name input-queue]
  (let [target (rand-nth (vals @answers))]
    (p/put-message input-queue {msg/topic [:other-bids target player-name]
                                msg/type :swap
                                :value (rand-int 3)})))

(defn receive-messages [input-queue]
  (platform/create-timeout 3000 #(provide-bid "player-abc" input-queue))
  (platform/create-timeout 2000 #(provide-bid "player-xyz" input-queue)))

(defn start-game-simulation [input-queue]
  (receive-messages input-queue))

(defrecord MockServices [app]
  p/Activity
  (start [this]
    (platform/create-timeout 3000 #(provide-answer "player-abc" (:input app)))
    (platform/create-timeout 2000 #(provide-answer "player-xyz" (:input app))))
  (stop [this]))

(defn services-fn [message input-queue]
  (cond (and (= (msg/topic message) [:active-game]) (:value message))
        (start-game-simulation input-queue)
        (= (first (msg/topic message)) :other-answers)
        (update-answer message)
        :else
        (.log js/console (str "Sending message to server: " message))))
