(ns electric-starter-app.main
  (:require [contrib.str :refer [empty->nil]]
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-ui4 :as ui4]
            [missionary.core :as m]
            #?(:clj
               [electric-starter-app.llm :as llm])
            #?(:clj
               [duratom.core :as duratom])
            #?(:clj
               [dorothy.core :as dot])))

#?(:clj (def graph (duratom/duratom :local-file
                                    :file-path "file.edn"
                                    :init {})))

#?(:clj (defn is-same-event [e1 e2]
          (def e1 e1)
          (def e2 e2)
          
          (.startsWith (.toLowerCase (llm/ask-ant (str "are these two events the same event?"
                                                       e1
                                                       "\" and \""
                                                       e2
                                                       "\"? respond with exactly one word"))) "y")))

(comment
  (is-same-event "the deployment of russian troops to crimea in late feburary 2014"
                "the deployment of russian troops to crimea in late feburary 2014")
  (is-same-event "1. The Euromaidan protests in Ukraine (November 2013 - February 2014) that led to the ousting of pro-Russian President Viktor Yanukovych"
                 "the deployment of russian troops to ukraine in late feburary 2022"))


#?(:clj (defn create-item [title parent]
          (let [uuid (random-uuid)
                embedding (llm/get-embedding title)]
            (def title title)
            (if (not-empty @graph)
              
              
              (do (def potential-duplicate (apply min-key (fn [x] (llm/embedding-distance embedding (:embedding x)))
                                                  (vals @graph)))
                  
                  (def is-same (is-same-event title (:title potential-duplicate)))

                  (if is-same
                    (do (when parent
                          (swap! graph assoc-in [parent :causes]
                                 (conj (get-in @graph [parent :causes]) (:id potential-duplicate))))
                        false)
                    (do
                      (swap! graph assoc uuid {:id uuid :title title
                                               :causes [] :effects []
                                               :embedding embedding})
                      (when parent
                        (swap! graph assoc-in [parent :causes]
                               (conj (get-in @graph [parent :causes]) uuid)))
                      uuid)))
              
              (do
                (swap! graph assoc uuid {:id uuid :title title
                                         :causes [] :effects []
                                         :embedding embedding})
                (when parent
                  (swap! graph assoc-in [parent :causes]
                         (conj (get-in @graph [parent :causes]) uuid)))
                uuid)))))

#?(:clj (def roots (duratom/duratom :local-file
                                    :file-path "roots.edn"
                                    :init [])))

(comment
  (e/server (create-item "american civil war" nil)))

(comment
  (reset! graph {})
  (reset! roots []))

(e/defn HistoryBlock [id seen !displayed]
  (e/client
   (let [data (e/server (get (e/watch graph) id)) !selectedchild (atom nil) selectedchild (e/watch !selectedchild)]
     
     (dom/div
      (dom/props {:style {:display "flex" :background "rgba(255, 100, 100, .1)" :margin-top "30px" :flex-direction "column" :cursor "pointer" :width "fit-content"}})
      (dom/on "click" (e/fn [_] (reset! !selectedchild nil)))
      (dom/div (dom/props {:style {:width "300px" :margin "10px"}})
               (dom/text (:title data))))
     
     (let [!show (atom false) show (e/watch !show)]
       (if selectedchild
         (HistoryBlock. selectedchild (conj seen id) !displayed)
         (do (dom/div
              (dom/props {:style {:display "flex" }})
              (e/for-by identity [cause (distinct (filter #(and (not= % id) (not (contains? seen %))) (:causes data)))]
                        (dom/div (dom/props {:style {:background "pink" :margin "10px" :dislpay "flex" :cursor "pointer"}})
                                 (dom/text (e/server (get-in @graph [cause :title])))
                                 (dom/on "click" (e/fn [_] (reset! !selectedchild cause))))) )
             (if show
               (let [r (e/server (new (m/reductions llm/collect
                                                    (llm/ask-ant-stream (str "what are the three main events that led to following historical event. Separate them by '---': \""
                                                                             (:title data))))))]
                 (dom/div
                  (dom/props {:style {:display "flex" }})
                  (e/for-by identity [x (clojure.string/split r #"---")]
                            (let [!instantiated (atom false) instantiated (e/watch !instantiated)]
                              (when (not instantiated)
                                (dom/div (dom/props {:style {:background "pink" :margin "10px" :dislpay "flex"}})
                                         (dom/text x)
                                         (dom/button
                                          (dom/on "click" (e/fn [_]
                                                            (let [new-id (e/server (create-item x id))]
                                                              (println "new-id" new-id)
                                                              (when new-id
                                                                (reset! !selectedchild new-id)))))
                                          (dom/text "save"))))))))
               (dom/button
                (dom/on "click"
                        (e/fn [_] (swap! !show not)))
                (dom/text "ask llm for causes")))))))))

(defn split-words-wrap [text]
  "split by words, insert newlines every 6 words, then rejoin"
  (def text text)
  (clojure.string/join "\n"
                       (map #(clojure.string/join " " %)
                            (partition 6 6
                                       nil
                                       (clojure.string/split text #" ")
                                       ))))

(e/defn Main [ring-request]
  (e/client
   (binding [dom/node js/document.body]
     (let [!displayed (atom #{}) !admin (atom false) admin (e/watch !admin)]
       (dom/input
        (dom/props {:placeholder "Type a message" :maxlength 100})
        (dom/on "keydown" (e/fn [e]
                            (reset! !admin (= (.. e -target -value) "secretadminpowers"))
                            (when (= "Enter" (.-key e))
                              (when-some [v (empty->nil (.. e -target -value))]
                                (set! (.-value dom/node) "")
                                (e/server (swap! roots conj (create-item v nil))))))))
       (e/for-by identity [root (e/server (e/watch roots))]
                 (when admin
                   (dom/div
                    (dom/button (dom/text "delete")
                                (dom/on "click"
                                        (e/fn [_] (e/server
                                                   (reset! roots (filter #(not= % root) @roots))
                                                   (swap! graph dissoc root)))) ) ) )
                 (let [!show (atom false) show (e/watch !show) ]
                   (if show
                     (dom/div
                      (dom/button (dom/text "hide causes")
                                  (dom/on "click"
                                          (e/fn [_] (swap! !show not) ) ) )
                      (HistoryBlock. root #{root} !displayed))
                     (dom/div
                      (dom/props {:style {:background "rgba(100, 100, 255, .1)" :margin-top "30px"}})
                      (dom/div
                       (dom/div (dom/text (:title (e/server (get (e/watch graph) root))))
                                (dom/button
                                 (dom/on "click"
                                         (e/fn [_] (swap! !show not)))
                                 (dom/text "show causes")) )) )
                     
                     
                     )))
       (dom/img
        (dom/props {:src 
                    (str "data:image/png;base64, "
                         (e/server
                          (llm/encode (dot/render (dot/dot (dot/digraph
                                                            {}
                                                            (concat
                                                             (->> (e/watch graph) vals (map (fn [node]
                                                                                              [(str (:id node))
                                                                                               {:label (split-words-wrap (:title node))
                                                                                                :shape "box"}])))
                                                             (->> (e/watch graph) vals (map (fn [node]
                                                                                              (for [x (:causes node)]
                                                                                                [(str (:id node)) (str x)]))))
                                                             )))
                                                  {:format :png}))))
                    :style {:width "100%"}}))))))
