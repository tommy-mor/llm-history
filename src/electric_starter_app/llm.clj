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

(defn ask-ant [q]
  (-> (client/post "https://api.anthropic.com/v1/messages"
                   {:headers {"Content-Type" "application/json"
                              "x-api-key" ant-key
                              "anthropic-version" "2023-06-01"}
                    :as :stream
                    :content-type :json
                    :form-params {:model "claude-3-sonnet-20240229"
                                  :messages [{:role "user" :content q}]
                                  :max_tokens 100
                                  :stream true}})
      :body))

(defn read-until-newlines [rdr]
  (cons (let [sb (StringBuilder.)]
          (loop [c (.read rdr)]
            (if (or (neg? c) (and (not (= 0 (.length sb)))
                                  (= \newline (char c))))
              (str sb)
              (do (when (not= \newline (char c)) (.append sb (char c)))
                  (recur (.read rdr))))))
        (lazy-seq (read-until-newlines rdr))))

(def r (ask-ant "what is the capital of france?"))

(defn read-messages [rdr]
  (let [[_ msg] (take 2 (read-until-newlines rdr))]
    (json/parse-string (second (clojure.string/split msg #":" 2)) true)))

(read-messages r)
(defn plus [x]
  (if x (+ x x)
      100))


