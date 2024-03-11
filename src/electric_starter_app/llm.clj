(ns electric-starter-app.llm
  (:require [clj-http.client :as client]
            [cheshire.core :as json]
            [missionary.core :as m]
            [selmer.parser :as sel]))


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

(defn ask-ant-stream [q]
  (fn [n t]
    (n)
    
    (let [body (-> (client/post "https://api.anthropic.com/v1/messages"
                                {:headers {"Content-Type" "application/json"
                                           "x-api-key" ant-key
                                           "anthropic-version" "2023-06-01"}
                                 :as :stream
                                 :content-type :json
                                 :form-params {:model "claude-3-sonnet-20240229"
                                               :messages [{:role "user" :content q}]
                                               :max_tokens 100
                                               :stream true}})
                   :body)]
      
      (reify clojure.lang.IDeref
        (deref [_] (let [_ (read-until-newlines body)
                         bit (read-until-newlines body)
                         bit (json/parse-string (second (clojure.string/split bit #":" 2)) true)]
                     (def bit "data: {\"type\": \"message start\"}")
                     (if (= (:type bit) "message_stop")
                       (do (t) bit)
                       (do (n) bit))))))))

(m/? (m/reduce conj (ask-ant-stream "hello")))

(comment
  (def r (ask-ant-stream "hello"))
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
