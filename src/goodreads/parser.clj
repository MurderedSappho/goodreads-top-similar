(ns goodreads.parser
  (:gen-class)
  (:require
    [clojure.string :as string]))

;; I prefer to use java.net.URL,but in this case it's too unsporting
(defn parse-url [url]
  (let [[_ host path param] (re-matches #".*://([^/]+)\/(.*?(?=\?|$))(?:.(.*))?" url)
        paths (string/split path #"/")
        param-pairs (if (some? param)
                      (->> (string/split param #"&")
                           (map #(string/split % #"="))
                           (into {}))
                      {})]
    {:host host
     :paths paths
     :params param-pairs}))

(defn new-pattern [pattern]
  (let [host (re-find #"(?<=host\().+?(?=\))" pattern)
        paths (-> (re-find #"(?<=path\().+?(?=\))" pattern)
                  (string/split #"/"))
        params (->> (re-seq #"(?<=queryparam\().+?(?=\))" pattern)
                    (map #(string/split % #"="))
                    (map (fn [[k v]] {(->> (rest v)
                                           (apply str))
                                      k}))
                    (into {}))]
    {:host host
     :paths paths
     :params params
     }))

(defn recognize [pattern url]
  (let [{url-host :host url-paths :paths url-params :params} (parse-url url)
        {pattern-host :host pattern-paths :paths pattern-params :params} pattern
        same-host? (= url-host pattern-host)
        valid-paths? (= (count url-paths) (count pattern-paths))
        params (->> (map (fn [[k _]] [(keyword k) (get-in url-params [k])]) pattern-params)
                    (filter (fn [[_ v]] (some? v))))
        valid-params? (= (count params) (count pattern-params))]
    (when (every? true? [same-host? valid-params? valid-paths?])
      (let [paths (->> (zipmap pattern-paths url-paths)
                       (filter (fn [[k v]](string/starts-with? k "?")))
                       (map (fn [[k v]][(->> (rest k)
                                             (apply str)
                                             (keyword)) v])))]
        (into (vec params) paths)))))
