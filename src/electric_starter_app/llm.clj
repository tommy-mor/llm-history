(ns electric-starter-app.llm
  (:require [clj-http.client :as client]
            [cheshire.core :as json]
            [missionary.core :as m]))


(def oai-key (-> "secrets.edn" slurp read-string :openai :api-key))
(def ant-key (-> "secrets.edn" slurp read-string :anthropic :api-key ))

(defn ask-oai [q]
  (-> (client/post "https://api.openai.com/v1/chat/completions"
                   {:headers {"Content-Type" "application/json"
                              "Authorization" (str "Bearer " oai-key)}
                    :content-type :json
                    :form-params {:model "gpt-3.5-turbo"
                                  :messages [{:role "user" :content q}]
                                  :temperature 0.7
                                  :stream true}})
      :body (json/parse-string true) :choices first :message :content))

(defn read-until-newlines [rdr]
  (let [sb (StringBuilder.)]
    (loop [c (.read rdr)]
      (if (or (neg? c) (and (not (= 0 (.length sb)))
                            (= \newline (char c))))
        (str sb)
        (do (.append sb (char c))
            (recur (.read rdr)))))))

(defn ask-ant [q]
  (-> (client/post "https://api.anthropic.com/v1/messages"
                   {:headers {"Content-Type" "application/json"
                              "x-api-key" ant-key
                              "anthropic-version" "2023-06-01"}
                    :content-type :json
                    :form-params {:model "claude-3-sonnet-20240229"
                                  :messages [{:role "user" :content q}]
                                  :max_tokens 100}})
      :body (json/parse-string true) :content first :text))

(defn new-tag []
  (let [st (->> (-> (client/get "http://localhost:3000/api/tag"
                                {:headers {"Content-type" "application/json"
                                           "Cookie" "ring-session=8cd75040-6f87-41f6-b16c-ba9e63314849"}
                                 :content-type :json})
                    :body (json/parse-string true) :public)
                (map :title)
                (filter #(clojure.string/starts-with? % "log"))
                last)
        st (inc (Integer/parseInt (last (clojure.string/split st #"log" 2))))]
    (-> (client/post "http://localhost:3000/api/tag"
                     {:headers {"Content-type" "application/json"
                                "Cookie" "ring-session=8cd75040-6f87-41f6-b16c-ba9e63314849"}
                      :content-type :json
                      :form-params {:title (str "log" st) :description ""}})
        :body (json/parse-string true) :id)))

  

  (new-tag)
  
  (comment
    (def r (ask-ant "hello"))
    (slurp r)
    (m/? (m/reduce conj (m/ap (let [[_ msg] (m/?> (m/eduction (partition-all 2) r))]
                                (when msg
                                  (json/parse-string (second (clojure.string/split msg #":" 2) true)))))))
    (defn read-messages [flow]
      (let [[_ msg] (m/? (m/reduce conj (m/eduction (partition-all 2) flow)))]
        (json/parse-string (second (clojure.string/split msg #":" 2)) true)))
    (defn llm-stream [q] (read-messages (ask-ant q)))
    (read-messages r)
    (read-until-newlines r)
    (take 100 s)
    (m/?> s))
