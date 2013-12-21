(ns wits.html-templates
  (:use [io.pedestal.app.templates :only [tfn dtfn tnodes]]))

(defmacro wits-templates
  []
  {:wits-page (dtfn (tnodes "wits.html" "hello") #{:id})})
