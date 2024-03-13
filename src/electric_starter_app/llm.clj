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
  (m/ap
   (let [body (m/? (m/via m/blk
                          (-> (client/post "https://api.anthropic.com/v1/messages"
                                           {:headers      {"Content-Type"      "application/json"
                                                           "x-api-key"         ant-key
                                                           "anthropic-version" "2023-06-01"}
                                            :as           :stream
                                            :content-type :json
                                            :form-params  {:model      "claude-3-sonnet-20240229"
                                                           :messages   [{:role "user" :content q}]
                                                           :max_tokens 100
                                                           :stream     true}})
                              :body)))]
     (loop []
       (m/? (m/via m/blk (read-until-newlines body)))
       (let [bit (m/? (m/via m/blk (read-until-newlines body)))
             bit (json/parse-string (second (clojure.string/split bit #":" 2)) true)]
         (if (= (:type bit) "message_stop")
           bit (m/amb bit (recur))))))))

(defn collect
  ([sofar next]
   (case (:type next)
     "content_block_start" (str sofar (-> next :content_block :text))
     "content_block_delta" (str sofar (-> next :delta :text))
     sofar))
  
  ([] ""))

(defn test []
  (m/ap
   (let [x (m/?> (m/seed ["Hello" "World" "!"]))]
     (m/? (m/sleep 1000))
     x)))

(comment
  (def r (ask-ant-stream "hello"))
  (m/? (m/reduce collect (m/ap
                          (m/?> (ask-ant-stream "hello")))))
  
  (m/? (m/reduce conj (m/ap
                       (let [x (m/?> (m/seed ["Hello" "World" "!"]))]
                         (m/? (m/sleep 1000))
                         x))))
  
  (m/? (m/reduce conj (m/ap (let [x (m/?> (m/seed [1 2 3 4]))]
                              (m/? (m/sleep 10))
                              x))))

  (m/? (m/reduce conj (m/ap
                       (let [x (m/?> (m/seed ["Hello" "World" "!"]))]
                         (Thread/sleep 1000)
                         x))))
  
  (m/? (m/reduce conj (m/ap
                       (println (m/?> (ask-ant-stream "what is the  meaning of life"))))))
  
  (m/? (m/reduce conj (m/ap
                       (println (m/?> (ask-ant-stream "what is the  meaning of life"))))))
  
  (def it ((m/reduce #(prn %2) nil (ask-ant-stream "what is the meaning of life"))
           (partial prn :ok)
           (partial prn :ex)))
  
  
  (m/? (m/reduce clct (ask-ant-stream "hello")))
  (m/? (m/reduce conj (ask-ant-stream "hello"))))




