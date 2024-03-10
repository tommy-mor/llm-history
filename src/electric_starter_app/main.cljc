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
       (comment (dom/input
                 (dom/props {:placeholder "Type a message" :maxlength 100})
                 (dom/on "keydown" (e/fn [e]
                                     (when (= "Enter" (.-key e))
                                       (when-some [v (empty->nil (.. e -target -value))]
                                         (set! (.-value dom/node) "")
                                         (reset! !val (e/server (llm/ask-ant v)))))))))
       (ui4/button (e/fn [] (reset! !val (e/server (llm/vote-top tagid))))
                   (dom/text "vote top"))
       (ui4/button (e/fn [] (reset! !val (e/server (llm/vote-pair tagid))))
                   (dom/text "vote pair"))
       (ui4/button (e/fn [] (reset! !val (e/server (llm/interpolate tagid))))
                   (dom/text "interpolate"))
       (ui4/button (e/fn [] (reset! !val (e/server (llm/extrapolate tagid))))
                   (dom/text "extrapolate"))
       (ui4/button (e/fn [] (reset! !val (e/server (llm/inverse tagid))))
                   (dom/text "inverse"))))))
