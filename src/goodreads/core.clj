(ns goodreads.core
  (:gen-class)
  (:require [clojure.tools.cli :as cli]
            [manifold.deferred :as d]
            [aleph.http :as http]
            [clojure.zip :as zip]
            [clojure.xml :as xml]))

(def items-on-page 100)
(def user-books-api-path "https://www.goodreads.com/review/list/")
(def book-info-api-path "https://www.goodreads.com/book/show/")

(declare
  handle-xml-response
  different-keys?
  xml->json
  fetch-goodreads
  fetch-user-books
  fetch-books
  build-recommentations
  book->str)

(defn handle-xml-response [{body :body}]
  (let [parsed-response (->> body
                             (xml/parse)
                             (zip/xml-zip)
                             xml->json)
        [{result :GoodreadsResponse}] parsed-response]
    { :response result }))

(defn different-keys? [content]
  (when content
    (let [dkeys (->> (map :tag content)
                     (distinct )
                     (filter identity )
                     count)
          n (count content)]
      (= dkeys n))))

(defn xml->json [element]
  (cond
    (nil? element) nil
    (string? element) element
    (sequential? element) (if (> (count element) 1)
                            (if (different-keys? element)
                              (reduce into {} (map (partial xml->json ) element))
                              (map xml->json element))
                            (xml->json  (first element)))
    (and (map? element) (empty? element)) {}
    (map? element) (if (:attrs element)
                     {(:tag element) (xml->json (:content element))
                      (keyword (str (name (:tag element)) "Attrs")) (:attrs element)}
                     {(:tag element) (xml->json  (:content element))})
    :else nil))

(defn fetch-goodreads [url]
  (d/chain
    (http/get url)
    handle-xml-response))

(defn fetch-user-books [user-id page per-page read-state token]
  (d/chain
    (fetch-goodreads
      (format "%s%s.xml?key=%s&v=2&page=%s&per_page=%s&shelf=%s"
              user-books-api-path user-id token page per-page read-state))
    #(let [{{reviews :reviews {:keys [start end total]} :reviewsAttrs} :response} %
           books (map (fn[{{{id :id} :book}:review}] id) reviews)
           last-page (int (Math/ceil (/ (read-string total) per-page)))
           pages-to-fetch (range 2 (inc last-page))]
       {:books books
        :pages-to-fetch pages-to-fetch})
    #(let [{:keys [books pages-to-fetch]} %
           query-more? (and (= 1 page) (not (empty? pages-to-fetch)))]
       (if query-more?
         (apply d/zip
                (conj (mapv (fn [p](fetch-user-books user-id p per-page read-state token)) pages-to-fetch) (future books)))
         books))))

(defn fetch-books [book-ids token]
  (apply d/zip
         (mapv #(d/chain
                  (fetch-goodreads (format "%s%s.xml?key=%s" book-info-api-path % token))
                  (fn [response](let [similar-books (->> (get-in response [:response :book :similar_books])
                                                         (map (fn [{{title :title
                                                                     link :link
                                                                     authors :authors
                                                                     rating :average_rating} :book}]
                                                                {:title title
                                                                 :authors authors
                                                                 :rating rating
                                                                 :link link})))]
                                  similar-books))) book-ids)))



(defn build-recommentations [{:keys [token user-id number-books]}]
  (d/let-flow [read-books (d/chain
                                (fetch-user-books user-id 1 items-on-page "read" token)
                                flatten)
               reading-books (d/chain
                                   (fetch-user-books user-id 1 items-on-page "reading" token)
                                   flatten)
               similar-books (d/chain (fetch-books read-books token)
                                      flatten
                                      #(->> (filter (fn[{id :id}] (not (some? (some #{id} reading-books)))) %)
                                            (distinct)
                                            (sort-by :rating)
                                            (reverse)
                                            (take number-books)
                                            vec))]
              similar-books))

(def cli-options [["-t"
                   "--timeout-ms"
                   "Wait before finished"
                   :default 10000
                   :parse-fn #(Integer/parseInt %)]
                  ["-n"
                   "--number-books"
                   "How many books do you want to recommend"
                   :default 10
                   :parse-fn #(Integer/parseInt %)]
                  ["-u"
                   "--user-id"
                   "User id to get similar books"
                   :default 27405051
                   :parse-fn #(Integer/parseInt %)]
                  ["-h" "--help"]])

(defn book->str [{:keys [title link authors]}]
  (format "\"%s\" by %s\nMore: %s"
          title
          (->> authors
               (map :name)
               (clojure.string/join ", "))
          link))

(defn -main [& args]
  (let [{:keys [options errors summary]} (cli/parse-opts args cli-options)]
    (cond
      (contains? options :help) (do (println summary) (System/exit 0))
      (some? errors) (do (println errors) (System/exit 1))
      (empty? args) (do (println "Please, specify user's token") (System/exit 1))
      :else (let [config {:token (first args)
                          :number-books (:number-books options)
                          :user-id (:user-id options)}
                  books (-> (build-recommentations config)
                            ;; Exception handling could be better, sorry I have not enough time ):
                            (d/catch Exception (fn[& ex] (println "Error has been occured, try again later" ex)))
                            (d/timeout! (:timeout-ms options) ::timeout)
                            deref)]
                  (println (type books))
              (cond
                (= ::timeout books) (println "Not enough time :(")
                (empty? books) (println "Nothing found, leave me alone :(")
                :else (doseq [[i book] (map-indexed vector books)]
                          (println (str "#" (inc i)))
                          (println (book->str book))
                          (println)))))))
