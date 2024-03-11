(ns electric-starter-app.main
  (:require [contrib.str :refer [empty->nil]]
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-ui4 :as ui4]
            [missionary.core :as m]
            #?(:clj [electric-starter-app.llm :as llm])))


;; Saving this file will automatically recompile and update in your browser

(def tagid 123)

(e/defn Main [ring-request]
  (e/client
   (binding [dom/node js/document.body]
     (let [!val (atom nil) val (e/watch !val)]
       (dom/input
        (dom/props {:placeholder "Type a message" :maxlength 100})
        (dom/on "keydown" (e/fn [e]
                            (when (= "Enter" (.-key e))
                              (when-some [v (empty->nil (.. e -target -value))]
                                (set! (.-value dom/node) "")
                                (reset! !val (e/server (llm/ask-ant v))))))))
       (dom/h3 (dom/text (str "hello: " val)))))))
