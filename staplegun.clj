;; #! /opt/homebrew/bin/bb

(ns staplegun
  (:require
   [org.httpkit.server :as httpkit.server]
   [clojure.core.match :refer [match]]
   [clojure.java.browse :as browse]
   [clojure.string :as str]
   [hiccup.core :as h]
   [babashka.deps :as deps]
   [babashka.pods :as pods]
   [babashka.tasks :refer [shell]]
   [clojure.set :as set])
  (:import [java.net URLDecoder URLEncoder]))

(println "starting up...")

(deps/add-deps '{:deps {com.github.seancorfield/honeysql {:mvn/version "2.2.861"}}})
(require '[honey.sql :as hdb])

(pods/load-pod 'org.babashka/go-sqlite3 "0.0.1")
(require '[pod.babashka.go-sqlite3 :as sqlite])

(def db "staple.db")

(defn execute! [query] (when query (sqlite/execute! db query)))

(defn format-results [history-results]
  (mapv #(cond-> % (:created_at %) (set/rename-keys {:created_at :created-at}))
        history-results))

(defn query [sql] (format-results (sqlite/query db sql)))

(defn last-clip-db []
  (->> {:select [:content] :from :history :order-by [[:created-at :desc]] :limit 1}
       hdb/format
       query
       first
       :content))

(defonce *last-clip (atom nil))

(defn insert-clip! [clip]
  (when (and clip (not= @*last-clip clip))
    (reset! *last-clip clip)
    (execute! ["insert into history (content, created_at) VALUES (?, ?)" clip (quot (System/currentTimeMillis) 1000)])))

(defn all-clips []
  (query (hdb/format {:select [:*]
                      :from :history
                      :order-by [[:created-at :desc]]})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; web view
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn escape-html
  "Change special characters into HTML character entities."
  [text]
  (.. ^String (str text)
    (replace "&"  "&amp;")
    (replace "<"  "&lt;")
    (replace ">"  "&gt;")
    (replace "\"" "&quot;")
    (replace "'" "&#39;")))

(defn clipboard-line-item [{:keys [content created-at]}]
  [:div {:style {:margin "5px"}}
   #_[:span created-at]
   [:pre {:style {:overflow-x "scroll"
                  :font-family "monospace"
                  :background-color "#eef2fe"
                  :border "2px solid grey"
                  :border-radius "2px"
                  :padding "1px"}}
    (escape-html content)]])

(defn top-ten-section []
  [:section.top-ten {:hx-get "/top-ten" :hx-trigger "every 2s"}
   [:div {:style {:border "3px solid #858" :margin "5px" :padding "5px"}}
    [:h3 "Last 20"]
    [:div
     (map clipboard-line-item (query (hdb/format {:select [:content :created-at]
                                                  :from   [:history]
                                                  :order-by [[:created-at :desc]]
                                                  :limit 20})))]]])

(defn matchize [term]
  (str/join " " (mapv #(str "*" % "*") (str/split term #" "))))

(defn home
  []
  (str
    "<!DOCTYPE html>"
    (h/html
      [:head
       [:meta {:charset "UTF-8"}]
       [:title "Staple Gun"]
       [:script {:src "https://unpkg.com/htmx.org@1.5.0/dist/htmx.min.js" :defer true}]
       [:script {:src "https://unpkg.com/hyperscript.org@0.8.1/dist/_hyperscript.min.js" :defer true}]]
      [:body {:style {:margin "10px"}}
       [:span.title
        [:h1 {:style {:display "inline"}} "Staple Gun"]
        [:div {:style {:font-size "10px"}} " Keep track of your clipboard history here."]
        [:div
         [:input {:type "text"
                  :autofocus true
                  :name "clipboard-query"
                  :hx-post "/search"
                  :hx-trigger "keyup changed once, every 2s" ;; delay:500
                  :hx-target "section#results"
                  :placeholder "Search..."}]]]
       [:section#results]
       [:section#top-ten (top-ten-section)]])))

(defn result-section [search-term]
  (let [clean (if search-term
                (str/trim (URLDecoder/decode search-term))
                "")
        results (if search-term
                  (sqlite/query db ["select * from history where content match ? order by created_at desc" clean])
                  [])]
    (h/html [:div
             [:h3 [:span (count results) " Results for: "[:pre clean]]]
             (into [:div] (map clipboard-line-item results))])))

(defn parse-query-string [query-string]
  (when query-string
    (-> query-string (str/split #"=") second)))

(defn routes [{:keys [request-method uri query-string] :as req}]
  ;; slurp the body and check it
  (let [body (when-let [b (:body req)] (slurp b))
        path (vec (rest (str/split uri #"/")))
        search-term (or (parse-query-string query-string)
                        (parse-query-string body))]
    (match [request-method path]
           [:get []] {:body (home)}
           [:get ["top-ten"]] {:body (h/html (top-ten-section))}
           [:post ["search"]] {:body (result-section search-term)}
           :else {:status 404 :body "Error 404: Page not found"})))


(defn open-port [n]
  (try (with-open [sock1 (java.net.ServerSocket. n)]
         (.getLocalPort sock1))
       (catch Exception _ (open-port (inc n)))))

(def port (open-port 4321))

(defn init! []
  (println "initializing...")
  (execute!
    [(str "create virtual table if not exists history "
          "using fts4"
          " (content TEXT, "
          "  created_at INTEGER)")])
  (reset! *last-clip (last-clip-db))
  ;; start watcher
  (future (while true
            (let [clip (:out @(shell {:out :string} "pbpaste"))]
              (insert-clip! clip))
            (Thread/sleep 100)))
  ;; sample populate:
  #_(do (insert-clip! "lemon")
        (insert-clip! "lime"))


  ;; check db:
  #_(future
      (while true
        (println "count: " (count (all-clips)))
        (Thread/sleep 1000)))

  (println "initialzation complete."))

(when (= *file* (System/getProperty "babashka.file"))
  (init!)

  ;; run server
  (let [url (str "http://localhost:" port "/")]
    (httpkit.server/run-server #'routes {:port port})
    (println "serving" url)
    (browse/browse-url url))

  @(promise))
