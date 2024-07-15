(ns electric-starter-app.main
  (:require [contrib.str :refer [empty->nil]]
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-ui4 :as ui4]
            [missionary.core :as m]
            #?(:clj
               [electric-starter-app.llm :as llm])
            #?(:clj
               [duratom.core :as duratom])))

#?(:clj (def graph (duratom :local-file
                            :file-path "file.edn"
                            :init {})))

#?(:clj (defn create-item [title description]
          (let [uuid (random-uuid)]
            (swap! graph assoc uuid {:id uuid :title title :description description})
            uuid)))

#?(:clj (def roots (duratom :local-file
                            :file-path "roots.edn"
                            :init [])))

(comment
  (e/server (create-item "test title" "test description")))

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

(e/defn HistoryBlock [id]
  (e/client (dom/div (dom/props {:style {:background "pink"
                                         :width "300px"}})
                     (let [data (e/server (get (e/watch graph) id))]
                       (dom/h3 (dom/text (:title data)))
                       (dom/text (:description data))
                       (dom/button
                        (dom/on "click"
                                (e/fn [_] (e/server
                                           (swap! graph dissoc id)
                                           (swap! roots (fn [f] (filter #(not= % id) f))))))
                        (dom/text "delete"))))))

(e/defn Main [ring-request]
  (e/client
   (binding [dom/node js/document.body]
     (let [!responses (atom []) responses (e/watch !responses)]
       (dom/pre (dom/text (pr-str (e/server (e/watch graph)))))
       (dom/pre (dom/text (pr-str (e/server (e/watch roots)))))
       (dom/input
        (dom/props {:placeholder "Type a message" :maxlength 100})
        (dom/on "keydown" (e/fn [e]
                            (when (= "Enter" (.-key e))
                              (when-some [v (empty->nil (.. e -target -value))]
                                (set! (.-value dom/node) "")
                                (e/server (swap! roots conj (create-item v ""))))))))
       (e/for-by identity [root (e/server (e/watch roots))]
                 (e/client (HistoryBlock. root )))))))
