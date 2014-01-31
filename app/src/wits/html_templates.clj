(ns wits.html-templates
  (:use [io.pedestal.app.templates :only [tfn dtfn tnodes]]))

(defmacro wits-templates
  []
  {:login-form  (tfn  (tnodes "login.html" "login-form"))
   :login-value (dtfn (tnodes "login.html" "login-value") #{:id})
   :question    (tfn  (tnodes "login.html" "question"))
   :answer-form (tfn  (tnodes "login.html" "answer-form"))
   :wait-page   (dtfn (tnodes "wait.html" "wait" [[:#answers]]) #{:id})
   :wait-answer (dtfn (tnodes "wait.html" "wait-answer") #{:id})
   :wits-page   (dtfn (tnodes "wits.html" "wits" [[:#slots]]) #{:id})
   :slot        (dtfn (tnodes "wits.html" "slot" [[:.bids]]) #{:id})
   :bid         (dtfn (tnodes "wits.html" "bid") #{:id :class :bidder})} )
