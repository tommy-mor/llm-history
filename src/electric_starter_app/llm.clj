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

(def cookie "ring-session=c548a617-065c-4469-80e5-1d04ebaf3485")

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
                                           "Cookie" cookie}
                                 :content-type :json})
                    :body (json/parse-string true) :public)
                (map :title)
                (filter #(clojure.string/starts-with? % "log"))
                last)
        st (inc (Integer/parseInt (last (clojure.string/split st #"log" 2))))]
    (-> (client/post "http://localhost:3000/api/tag"
                     {:headers {"Content-type" "application/json"
                                "Cookie" cookie}
                      :content-type :json
                      :form-params {:title (str "log" st) :description ""}})
        :body (json/parse-string true) :id)))

(def tagid 110)

(defn get-ranking [tagid]
  (-> (client/get (str "http://localhost:3000/api/tag/page?id=" tagid)
                  {:headers {"Content-type" "application/json"
                             "Cookie" cookie}
                   :content-type :json})
      :body (json/parse-string true) :sorted ))


(defn vote-top [tagid]
  (let [[fst snd] (take 2 (get-ranking tagid))]
    (def fst fst)
    (def snd snd)

    (def resp (ask-ant (sel/render "here are two thoughts, A\n: \"{{a}}\" \n\n B:\n \"{{b}}\"\n\nwhich one do you like better. Respond only with a single letter, or multiple letters if you like it a lot more."
                                   {:a (:title fst) :b (:title snd)})))

    (def magnitude (case resp
                     "A" 20
                     "B" 80
                     _ (do
                         (println "unusual resp" resp)
                         50)))
    (client/post "http://localhost:3000/api/vote"
                 {:headers {"Content-type" "application/json"
                            "Cookie" cookie}
                  :content-type :json
                  :form-params
                  {:tag_id tagid
                   :left_item_id (:id fst)
                   :right_item_id (:id snd)
                   :magnitude magnitude
                   :attribute 0}})))

(defn interpolate [tagid]
  (let [[fst snd] (take 2 (get-ranking tagid))]
    (def fst fst)
    (def snd snd)

    "template to vote between pairs"
    "submit vote"
    ))

(defn extrapolate [tagid]
  (let [[fst snd] (take 2 (get-ranking tagid))]
    (def fst fst)
    (def snd snd)

    "template to vote between pairs"
    "submit vote"
    ))

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
