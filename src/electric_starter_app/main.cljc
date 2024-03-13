(ns electric-starter-app.main
  (:require [contrib.str :refer [empty->nil]]
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-ui4 :as ui4]
            [missionary.core :as m]
            #?(:clj [electric-starter-app.llm :as llm])))


;; Saving this file will automatically recompile and update in your browser

(e/defn Response [v]
  (let [r (e/server (new (m/reductions llm/collect (llm/ask-ant-stream v))))]
    (e/client
     (dom/h4 (dom/text r)))))

#_(e/defn Response [v]
  (let [r (e/server (m/? (m/reduce conj (m/ap
                                         (let [x (m/?> (m/seed ["Hello" "World" "!"]))]
                                           (m/? (m/sleep 10))
                                           x)))))]
    (e/client
     (dom/h4 (dom/text r)))))

(e/defn Main [ring-request]
  (e/client
   (binding [dom/node js/document.body]
     (let [!responses (atom []) responses (e/watch !responses)]
       (dom/input
        (dom/props {:placeholder "Type a message" :maxlength 100})
        (dom/on "keydown" (e/fn [e]
                            (when (= "Enter" (.-key e))
                              (when-some [v (empty->nil (.. e -target -value))]
                                (set! (.-value dom/node) "")
                                (swap! !responses conj v))))))
       (dom/h3 (dom/text (str responses)))
       (e/for-by identity [v responses]
                 (Response. v))))))
