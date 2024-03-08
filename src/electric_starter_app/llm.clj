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
  (let [body (:body (client/post "https://api.anthropic.com/v1/messages"
                                 {:headers {"Content-Type" "application/json"
                                            "x-api-key" ant-key
                                            "anthropic-version" "2023-06-01"}
                                  :as :stream
                                  :content-type :json
                                  :form-params {:model "claude-3-sonnet-20240229"
                                                :messages [{:role "user" :content q}]
                                                :max_tokens 100
                                                :stream true}}))]
    (fn [n t]
      (n)
      (reify
        clojure.lang.IDeref
        (deref [_] (let [resp (read-until-newlines body)]
                     (prn resp)
                     (if (= "" resp)
                       (t)
                       
                       (do
                         (n)
                         resp))))))))

(defn read-messages [flow]
  (let [[_ msg] (m/? (m/reduce conj (m/eduction (partition-all 2) flow)))]
    (json/parse-string (second (clojure.string/split msg #":" 2)) true)))

(defn llm-stream [q] (read-messages (ask-ant q)))

(comment
  (def r (ask-ant "hello"))
  (slurp r)
  (m/? (m/reduce conj (m/ap (let [[_ msg] (m/?> (m/eduction (partition-all 2) r))]
                              (when msg
                                (json/parse-string (second (clojure.string/split msg #":" 2) true)))))))
  (read-messages r)
  (read-until-newlines r)
  (take 100 s)
  (m/?> s)
  
  )

(defn plus [x]
  (if x (+ x x)
      100))


