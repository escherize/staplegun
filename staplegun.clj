;; #! /opt/homebrew/bin/bb

(ns staplegun
  (:require
   [babashka.deps :as deps]
   [babashka.pods :as pods]
   [babashka.tasks :refer [shell]]
   [clojure.core.match :refer [match]]
   [clojure.java.browse :as browse]
   [clojure.set :as set]
   [clojure.string :as str]
   [hiccup.core :as h]
   [org.httpkit.server :as httpkit.server]
   [selmer.parser :refer [<<]])
  (:import
   [java.net URLDecoder]))

(println "starting up...")


(deps/add-deps '{:deps {com.github.seancorfield/honeysql {:mvn/version "2.2.861"}
                        camel-snake-kebab/camel-snake-kebab {:mvn/version "0.4.2"}}})
;; sql generation
(require '[honey.sql :as hdb])
;; keyword things
(require '[camel-snake-kebab.core :as csk])

;; sqlite connection
(pods/load-pod 'org.babashka/go-sqlite3 "0.0.1")
(require '[pod.babashka.go-sqlite3 :as sqlite])

(def db "The database file on disk" "staple.db")

(defn execute! [query] (when query (sqlite/execute! db query)))

(defn map-keys [f m] (zipmap (map f (keys m)) (vals m)))
(defn format-results [history-results]
  (mapv #(map-keys csk/->kebab-case %)
        history-results))

(defn query [sql] (format-results (sqlite/query db sql)))

(defn last-clip-db []
  (->> {:select [:content] :from :history :order-by [[:created-at :desc]] :limit 1}
       hdb/format
       query
       first
       :content))

(defonce *last-clip (atom nil))

(defn insert-clip-if-needed! [clip]
  (when
      ;; don't just re-insert the very last clip, since this gets called many times
      (and clip (not= @*last-clip clip))
    (reset! *last-clip clip)
    (execute! ["insert into history (content, created_at) VALUES (?, ?)" clip (quot (System/currentTimeMillis) 1000)])))

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
  (let [my-id (apply str (repeatedly 10 #(rand-nth "qwertyuiopasdfghjklzxcvbnm")))]
    [:div {:style {:min-height "30px"}}
     [:div.clipboard-line-item {:style {:margin "5px"}}
      #_[:span created-at]
      [:button {:id my-id
                :style {:width "28px" :height "28px" :line-height "1"}}
       "📎"]
      [:div {:style {:width "5px" :height "1px" :display "inline-block"}} " "]
      [:div {:style {:margin-left "35px" :margin-top "-38px"}}
       [:pre {:id my-id
              :style {:overflow-x "scroll"
                      :font-family "monospace"
                      :background-color "#eef2fe"
                      :border "2px solid grey"
                      :border-radius "2px"
                      :padding "1px 3px"

                      }}
        (escape-html content)]]]
     [:script (<< "
                   var btn = htmx.find('button#{{my-id}}');
                   btn.addEventListener('click', (_) => {
                      clipboardCopy(htmx.find('pre#{{my-id}}').textContent);

          htmx.ajax('GET', '/top-ten', {target: 'section#top-ten'})});


"
                  )]]))

(defn top-ten-section []
  [:section.top-ten
   [:div {:style {:border "3px solid #858" :margin "5px" :padding "5px" :border-radius "10px"}}
    [:h2 "Last 50"]
    [:button {:hx-get "/top-ten" :hx-target "section#top-ten"} "Refresh"]
    [:div
     (map clipboard-line-item (query (hdb/format {:select [:content :created-at]
                                                  :from   [:history]
                                                  :order-by [[:created-at :desc]]
                                                  :limit 50})))]]])

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
       [:script {:src "https://unpkg.com/htmx.org@1.5.0/dist/htmx.min.js"}]
       [:script {:src "https://unpkg.com/hyperscript.org@0.8.1/dist/_hyperscript.min.js" :defer true}]
       ]
      [:body {:style {:margin "10px"}}
       [:span.title
        [:h1 {:style {:display "inline"}} "Staple Gun"]
        [:div {:style {:font-size "10px"}} " Keep track of your clipboard history here."]
        [:div
         [:input#search-input
          {:type "search"
           :autofocus true
           :name "clipboard-query"
           :hx-post "/search"
           :hx-trigger "keyup changed delay:200, clipboard-query, once every 60s"
           :hx-target "section#results"
           :placeholder "Search..."}]]]
       [:section#results]
       [:section#top-ten (top-ten-section)]]
      [:script
       ;; read query params, hit search endpoint if needed.
       "let q = Object.fromEntries(new URLSearchParams(window.location.search).entries()).q;
        if (typeof q != 'undefined') {
          let elt = htmx.find('input#search-input');
          htmx.ajax('POST', '/search', {source: elt, target: '#result-section', values: {from_qp: q}});
          elt.value = q;
        }"]
      [:script
       ;; copy clipboard contents
       "async function clipboardCopy(text) {await navigator.clipboard.writeText(text);}"])))

(defn result-section [search-term]
  (let [clean (if search-term (str/trim (URLDecoder/decode search-term)) "")
        results (if search-term
                  (sqlite/query db ["select * from history where content match ? order by created_at desc" clean])
                  [])]
    (h/html [:div {:style {:border "3px solid #588" :margin "5px" :padding "5px" :margin-bottom "20px" :border-radius "10px"}}
             [:h3 [:span (count results) " Results for: "[:pre clean]]]
             (into [:div] (map clipboard-line-item results))])))

(defn parse-query-string [query-string]
  (when query-string
    (or
      (let [[_ v] (re-matches #".*from_qp=(.*)$" query-string)] v)
      (-> query-string (str/split #"=") second))))

(defn routes [{:keys [request-method uri query-string] :as req}]
  ;; slurp the body and check it
  (let [body (when-let [b (:body req)] (slurp b))
        path (vec (rest (str/split uri #"/")))
        search-term (or (parse-query-string query-string)
                        (parse-query-string body))]
    (match [request-method path]
           [:get []] {:body (home)}
           [:get ["top-ten"]] {:body (h/html (top-ten-section))}
           [:post ["search"]] {:body (do
                                       (println "------------------------------")
                                       (prn req)
                                       (prn body)
                                       (prn search-term)
                                       (result-section search-term))
                               :headers {"HX-Push" (if search-term
                                                     (str "?q=" search-term)
                                                     "/")}}
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

  ;; start watcher in another thread
  (future
    (while true
      (let [clip (:out @(shell {:out :string} "pbpaste"))]
        (insert-clip-if-needed! clip))
      (Thread/sleep 100)))

  (println "initialzation complete."))

(when (= *file* (System/getProperty "babashka.file"))
  (init!)

  ;; run server
  (let [url (str "http://localhost:" port "/")]
    (httpkit.server/run-server #'routes {:port port})
    (println "serving" url)
    (browse/browse-url url))

  @(promise))

;; TODO:
;; choose how many history items to show (paginated?)
;; auto refresh
;; figure a good wya to open this
