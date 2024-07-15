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

#?(:clj (defn create-item [title]
          (let [uuid (random-uuid)]
            (swap! graph assoc uuid {:id uuid :title title
                                     :causes [] :effects []})
            uuid)))

#?(:clj (def roots (duratom :local-file
                            :file-path "roots.edn"
                            :init [])))

(comment
  (e/server (create-item "american civil war")))

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

(comment
  (reset! graph {})
  (reset! roots []))

(e/defn HistoryBlock [id]
  (e/client
   
   (let [data (e/server (get (e/watch graph) id))]
     (dom/div
      (dom/props {:style {:display "flex"}})
      (dom/div (dom/props {:style {:background "pink"
                                   :width "300px"}})
               (dom/text (:title data))
               #_(dom/button
                (dom/on "click"
                        (e/fn [_] (e/server
                                   (swap! graph dissoc id)
                                   (swap! roots (fn [f] (filter #(not= % id) f))))))
                (dom/text "delete")))
      (dom/div
       (let [!show (atom false) show (e/watch !show)]
         (if show
           (let [r (e/server (new (m/reductions llm/collect
                                                (llm/ask-ant-stream (str "what are the three main events that led to following historical event. Separate them by '---': \""
                                                                         (:title data))))))]
             (e/for-by identity [x (:causes data)]
                       (HistoryBlock. x))
             (e/for-by identity [x (clojure.string/split r #"---")]
                       (let [!instantiated (atom false) instantiated (e/watch !instantiated)]
                         (when (not instantiated)
                           (dom/div (dom/props {:style {:background "pink" :margin "10px"}})
                                    (dom/text x)
                                    (dom/button
                                     (dom/on "click" (e/fn [_]
                                                       (let [new-id (e/server (create-item x))]
                                                         (reset! !instantiated new-id)
                                                         (e/server
                                                          (swap! graph assoc-in [id :causes]
                                                                 (conj (get-in @graph [id :causes]) new-id))))))
                                     (dom/text "save")))))))
           (dom/button
            (dom/on "click"
                    (e/fn [_] (swap! !show not)))
            (dom/text "causes>")))))))))

(e/defn Main [ring-request]
  (e/client
   (binding [dom/node js/document.body]
     (let [!responses (atom []) responses (e/watch !responses)]
       #_(dom/pre (dom/text (e/server (clojure.pprint/write (e/watch graph) :stream nil))))
       #_(dom/pre (dom/text (pr-str (e/server (e/watch roots)))))
       (dom/input
        (dom/props {:placeholder "Type a message" :maxlength 100})
        (dom/on "keydown" (e/fn [e]
                            (when (= "Enter" (.-key e))
                              (when-some [v (empty->nil (.. e -target -value))]
                                (set! (.-value dom/node) "")
                                (e/server (swap! roots conj (create-item v))))))))
       (e/for-by identity [root (e/server (e/watch roots))]
                 (e/client (HistoryBlock. root )))

       (dom/div (dom/props {:id "cyto"
                            :style {:width "1000px" :height "1000px"}}))
       (e/client
        (let [cy (js/cytoscape (clj->js {:container (js/document.getElementById "cyto")
                                         :elements (e/server {:nodes (->> (vals (e/watch graph))
                                                                          (map (fn [x]
                                                                                 (def x x)
                                                                                 {:id (str (:id x))
                                                                                  :data {:id (str (:id x))
                                                                                         :label (:title x)}})))
                                                              :edges
                                                              (->> (vals (e/watch graph))
                                                                   (map #(for [y (:causes %)]
                                                                           {:id (str (random-uuid))
                                                                            :data {:source (:id %) :target y :id (str (random-uuid))}}))
                                                                   flatten)})
                                         :style [{:selector "node"
                                                  :style {:label "data(label)"
                                                          :text-valign "center"
                                                          :text-halign "center"}}]
                                         :layout {:name (do
                                                          (println "here")
                                                          "cose")}}))]
          (println cy)))))))
