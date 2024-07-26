(ns electric-starter-app.llm
  (:require [clj-http.client :as client]
            [cheshire.core :as json]
            [missionary.core :as m]
            [selmer.parser :as sel])
  (:import java.util.Base64))

(def oai-key (-> "secrets.edn" slurp read-string :openai :api-key))
(def ant-key (-> "secrets.edn" slurp read-string :anthropic :api-key ))

(defn encode [s]
  (.encodeToString (Base64/getEncoder) s))

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
                    :form-params {:model "claude-3-haiku-20240307"
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
                                            :form-params  {:model      (or "claude-3-haiku-20240307" "claude-3-5-sonnet-20240620")
                                                           :messages   [{:role "user" :content q} {:role "assistant" :content "---"}]
                                                           :max_tokens 4096
                                                           :stream     true}})
                              :body)))]
     (loop []
       (m/? (m/via m/blk (read-until-newlines body)))
       (let [bit (m/? (m/via m/blk (read-until-newlines body)))
             bit (json/parse-string (second (clojure.string/split bit #":" 2)) true)]
         (if (= (:type bit) "message_stop")
           bit (m/amb bit (recur))))))))

(defn get-embedding [text]
  (-> (client/post "https://api.openai.com/v1/embeddings"
                   {:headers {"Content-Type" "application/json"
                              "Authorization" (str "Bearer " oai-key)}
                    :content-type :json
                    :form-params {:model "text-embedding-3-small"
                                  :input text}})
      :body (json/parse-string true) :data first :embedding))

(defn embedding-distance [x1 x2]
  (Math/sqrt (reduce + (map (fn [a b] (Math/pow (- a b) 2)) x1 x2))))

(defn is-duplicate [all new]
  (apply min-key (fn [x] (embedding-distance new x)) all))



(comment
  (embedding-distance (get-embedding "the civil war")
                      (get-embedding "world war 2"))
  (embedding-distance (get-embedding "circle")
                      (get-embedding "world war 2"))
  (is-duplicate [(get-embedding "the civil war")
                 (get-embedding "world war 2")]
                (get-embedding "world war 2"))
  
  )

(defn collect
  ([sofar next]
   (case (:type next)
     "content_block_start" (str sofar (-> next :content_block :text))
     "content_block_delta" (str sofar (-> next :delta :text))
     sofar))
  
  ([] ""))

(comment
  (def r (ask-ant-stream "hello"))
  (m/? (m/reduce collect (m/ap
                          (m/?> (ask-ant-stream "hello"))))))
