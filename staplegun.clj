;; #! /opt/homebrew/bin/bb
(ns staplegun
  (:require
   [babashka.curl :as curl]
   [babashka.deps :as deps]
   [babashka.pods :as pods]
   [babashka.process :refer [process check sh pipeline pb]]
   [babashka.fs :as fs]
   [babashka.tasks :refer [shell]]
   [cheshire.core :as json]
   [clojure.core.match :refer [match]]
   [clojure.java.browse :as browse]
   [clojure.string :as str]
   [clojure.pprint :as pprint]
   [hiccup.core :as h]
   [org.httpkit.server :as httpkit.server]
   [selmer.parser :refer [<<]]
   [clojure.walk :as walk])
  (:import
   [java.net URLDecoder]))

(deps/add-deps '{:deps {com.github.seancorfield/honeysql {:mvn/version "2.2.861"}
                        camel-snake-kebab/camel-snake-kebab {:mvn/version "0.4.2"}}})
;; sql generation
(require '[honey.sql :as hdb])
;; keyword things
(require '[camel-snake-kebab.core :as csk])

;; sqlite connection
(pods/load-pod 'org.babashka/go-sqlite3 "0.0.1")
(require '[pod.babashka.go-sqlite3 :as sqlite])

(def config
  (atom
    {:db "staple.db"
     :modifications []}))

(defn execute! [query]
  (when query
    (sqlite/execute! (:db @config) query)))

(defn map-keys [f m]
  (zipmap (map f (keys m))
          (vals m)))

(defn format-results [history-results]
  (mapv #(map-keys csk/->kebab-case %)
        history-results))

(defn query [sql]
  (format-results
    (sqlite/query (:db @config) sql)))

(defn last-clip-db []
  (->> {:select [:content]
        :from :history
        :order-by [[:created-at :desc]]
        :limit 1}
       hdb/format
       query
       first
       :content))

(defonce *last-clip (atom nil))

(defn insert-clip-if-needed! [clip]
  (when ;; don't just re-insert the very last clip, since this gets called many times
      (and clip
           (not= @*last-clip clip))
    (reset! *last-clip clip)
    (execute! ["insert into history (content, created_at) VALUES (?, ?)" clip (quot (System/currentTimeMillis) 1000)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mod helpers - available to export functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn run!
  "Run a shell command. Args are passed to babashka.process/sh.
   Output is ignored. Returns nil."
  [& args]
  (future (apply sh args))
  nil)

(defn run-out!
  "Run a shell command and return its stdout as a string (trimmed)."
  [& args]
  (str/trim (:out (apply sh args))))

(defn pretty-json
  "Parse a JSON string and return it pretty-printed."
  [s]
  (json/generate-string (json/parse-string s) {:pretty true}))

(defn from-epoch
  "Convert a Unix epoch (seconds) to an ISO-8601 string."
  [ts]
  (str (java.time.Instant/ofEpochSecond (Long/parseLong (str ts)))))

(defn word-count
  "Count whitespace-delimited words in a string."
  [s]
  (str (count (str/split (str/trim s) #"\s+"))))

(def mod-helpers
  "Documentation for functions available in mod :export handlers."
  [{:name "run!"         :args "& args"    :desc "Run a shell command (async, output ignored)"}
   {:name "run-out!"     :args "& args"    :desc "Run a shell command, return stdout (trimmed)"}
   {:name "pretty-json"  :args "s"         :desc "Pretty-print a JSON string"}
   {:name "from-epoch"   :args "ts"        :desc "Unix epoch (seconds) -> ISO-8601 string"}
   {:name "word-count"   :args "s"         :desc "Count words in a string"}
   {:name "str/upper-case"   :args "s"     :desc "Uppercase a string"}
   {:name "str/lower-case"   :args "s"     :desc "Lowercase a string"}
   {:name "str/trim"         :args "s"     :desc "Trim whitespace"}
   {:name "str/replace"      :args "s match replacement" :desc "Replace in string"}
   {:name "str/capitalize"   :args "s"     :desc "Capitalize first letter"}
   {:name "json/parse-string"    :args "s" :desc "Parse JSON to Clojure data"}
   {:name "json/generate-string" :args "data opts?" :desc "Clojure data to JSON string"}])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; modification
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn safe-re-matches [maybe-re value]
  (re-matches
    (cond-> maybe-re string? re-pattern)
    (str value)))

(defn apply-export [export match]
  (if (fn? export) (export match) export))

(defn maybe-modify
  "Returns modified value if it matches any :re in config, otherwise returns value."
  [modifications value]
  (reduce
    (fn [_ {:keys [re export]}]
      (if-let [match (safe-re-matches re value)]
        (reduced (apply-export export match))
        value))
    value
    modifications))

(defn find-matching-mod
  "Returns {:mod m :result transformed} for the first matching mod, or nil."
  [modifications value]
  (reduce
    (fn [_ {:keys [re export] :as m}]
      (when-let [match (safe-re-matches re value)]
        (reduced {:mod m :result (apply-export export match)})))
    nil
    modifications))

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

(defn clipboard-line-item [{:keys [modified content created-at]}]
  (let [my-id (apply str (repeatedly 10 #(rand-nth "qwertyuiopasdfghjklzxcvbnm")))]
    [:div {:style {:min-height "30px"}}
     [:div.clipboard-line-item {:style {:margin "3px 5px"}}
      #_[:span created-at]
      [:button {:id my-id
                :style {:width "28px" :height "28px" :line-height "1"}}
       "📎"]
      (when (not= modified content)
        [:button {:id (str my-id "-auto")
                  :style {:width "28px"
                          :height "28px"
                          :line-height "1"
                          :margin-left "5px"}}
         "🤖"])
      
      [:div {:style {:width "5px"
                     :height "1px"
                     :display "inline-block"}} " "]
      [:div {:style {:margin-left "35px" :margin-top "-38px"}}
       [:pre {:id my-id
              :style (merge {:overflow-x "scroll"
                             :font-family "monospace"
                             :background-color "#eef2fe"
                             :border "2px solid grey"
                             :border-radius "2px"
                             :padding "1px 3px"}
                            (when (not= modified content)
                              {:margin-left "32px"}))}
        (escape-html content)]
       (when (not= modified content)
         [:pre {:id (str my-id "-auto") :style {:display "none"}}
          (escape-html modified)])]]
     [:script (<< "
                   var btn = htmx.find('button#{{my-id}}');
                   btn.addEventListener('click', (_) => {
                      clipboardCopy(htmx.find('pre#{{my-id}}').textContent);

          htmx.ajax('GET', '/top-ten', {target: 'section#top-ten'})});


"
                  )]
     (when (not= modified content)
       [:script (<< "
                   var btn = htmx.find('button#{{my-id}}-auto');
                   btn.addEventListener('click', (_) => {
                      clipboardCopy(htmx.find('pre#{{my-id}}-auto').textContent);

          htmx.ajax('GET', '/top-ten', {target: 'section#top-ten'})});


"
                    )])]))

(defn mod-row [idx {:keys [re source]}]
  [:div.mod-row {:style {:border "1px solid #ccc"
                         :border-radius "4px"
                         :padding "8px"
                         :margin-bottom "6px"
                         :background-color "#fafafa"}}
   [:div {:style {:display "flex" :gap "8px" :align-items "flex-start"}}
    [:div {:style {:flex "1"}}
     [:label {:style {:font-size "12px" :color "#666"}} "Regex"]
     [:input {:type "text"
              :name (str "re-" idx)
              :value (pr-str re)
              :style {:width "100%" :font-family "monospace" :padding "4px"}}]]
    [:div {:style {:flex "2"}}
     [:label {:style {:font-size "12px" :color "#666"}} "Export"]
     [:textarea {:name (str "export-" idx)
                 :rows 2
                 :style {:width "100%" :font-family "monospace" :padding "4px" :resize "vertical"}}
      (pr-str source)]]
    [:button {:hx-delete (str "/mod/" idx)
              :hx-target "#mod-editor"
              :style {:align-self "center" :cursor "pointer"}}
     "X"]]])

(defn mod-editor []
  (let [mods (:modifications @config)]
    (h/html
      [:div#mod-editor
       [:div {:style {:display "flex" :gap "8px" :margin-bottom "8px"}}
        [:button {:hx-post "/mod/add"
                  :hx-target "#mod-editor"
                  :style {:cursor "pointer"}}
         "+ Add Mod"]
        [:button {:hx-post "/mod/save"
                  :hx-target "#mod-editor"
                  :hx-include ".mod-row input, .mod-row textarea"
                  :style {:cursor "pointer" :font-weight "bold"}}
         "Save & Reload"]]
       (map-indexed mod-row mods)])))

(defn example-mods-section []
  (let [raw (slurp "example_mods.edn")]
    [:details {:style {:margin-top "12px"}}
     [:summary {:style {:cursor "pointer" :font-weight "bold" :color "#666"}} "Example mods (from example_mods.edn)"]
     [:pre {:style {:background-color "#f5f5f5"
                    :border "1px solid #ddd"
                    :border-radius "4px"
                    :padding "8px"
                    :margin-top "6px"
                    :font-family "monospace"
                    :font-size "13px"
                    :overflow-x "auto"
                    :white-space "pre-wrap"}}
      (escape-html raw)]]))

(defn helpers-docs-section []
  [:details {:style {:margin-top "12px"}}
   [:summary {:style {:cursor "pointer" :font-weight "bold" :color "#666"}} "Available functions in :export handlers"]
   [:table {:style {:border-collapse "collapse" :margin-top "6px" :font-size "13px" :width "100%"}}
    [:thead
     [:tr {:style {:background-color "#eee"}}
      [:th {:style {:padding "4px 8px" :text-align "left" :border "1px solid #ddd"}} "Function"]
      [:th {:style {:padding "4px 8px" :text-align "left" :border "1px solid #ddd"}} "Args"]
      [:th {:style {:padding "4px 8px" :text-align "left" :border "1px solid #ddd"}} "Description"]]]
    (into [:tbody]
      (for [{:keys [name args desc]} mod-helpers]
        [:tr
         [:td {:style {:padding "4px 8px" :font-family "monospace" :border "1px solid #ddd"}} name]
         [:td {:style {:padding "4px 8px" :font-family "monospace" :border "1px solid #ddd"}} args]
         [:td {:style {:padding "4px 8px" :border "1px solid #ddd"}} desc]]))]])

(defn show-mods []
  [:div.mods [:div#mod-editor]])

(defn top-ten-section []
  [:section.top-ten
   [:div {:style {:border "3px solid #858" :margin "5px" :padding "5px" :border-radius "10px"}}
    [:h2 "Last 50"]
    [:button {:hx-get "/top-ten" :hx-target "section#top-ten"} "Refresh"]
    [:div
     (map (comp clipboard-line-item
                (fn [{:keys [content] :as li}] (assoc li :modified (maybe-modify (:modifications @config) content))))
          (query (hdb/format {:select [:content :created-at]
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
       [:style
        ".tab-btn { padding: 8px 16px; cursor: pointer; border: 1px solid #ccc; border-bottom: none;
                    border-radius: 6px 6px 0 0; background: #eee; margin-right: 2px; font-size: 14px; }
         .tab-btn.active { background: white; font-weight: bold; border-bottom: 1px solid white; margin-bottom: -1px; position: relative; z-index: 1; }
         .tab-content { display: none; border: 1px solid #ccc; border-radius: 0 6px 6px 6px; padding: 10px; }
         .tab-content.active { display: block; }"]]
      [:body {:style {:margin "10px"}}
       [:span.title
        [:h1 {:style {:display "inline"}} "Staple Gun"]
        [:div {:style {:font-size "10px"}} " Keep track of your clipboard history here."]]
       ;; tabs
       [:div {:style {:margin-top "10px"}}
        [:div {:style {:display "flex"}}
         [:button.tab-btn.active {:onclick "switchTab('history')"} "History"]
         [:button.tab-btn {:onclick "switchTab('playground')"} "Mod Playground"]]
        ;; history tab
        [:div#tab-history.tab-content.active
         [:div
          [:input#search-input
           {:type "search"
            :autofocus true
            :name "clipboard-query"
            :hx-post "/search"
            :hx-trigger "keyup changed delay:200, clipboard-query, once every 60s"
            :hx-target "section#results"
            :placeholder "Search..."}]]
         [:section#results]
         [:section#top-ten (top-ten-section)]]
        ;; playground tab
        [:div#tab-playground.tab-content
         [:h2 "Mod Playground"]
         [:div {:style {:margin-bottom "12px"}}
          [:h3 "Test a mod"]
          [:input#preview-input
           {:type "text"
            :name "preview-input"
            :hx-post "/preview"
            :hx-trigger "keyup changed delay:300ms"
            :hx-target "#preview-result"
            :placeholder "Type test input to see mod results..."}]
          [:div#preview-result]]
         [:div {:style {:border-top "1px solid #ccc" :padding-top "8px" :margin-bottom "12px"}}
          [:h3 "Run code"]
          [:textarea#eval-input
           {:name "eval-input"
            :rows 3
            :hx-post "/eval"
            :hx-trigger "keyup changed delay:500ms"
            :hx-target "#eval-result"
            :style {:width "100%" :font-family "monospace" :padding "4px" :resize "vertical"}
            :placeholder "(+ 1 2 3)  ;; eval any Clojure expression"}]
          [:div#eval-result]]
         [:div {:style {:border-top "1px solid #ccc" :padding-top "8px"}}
          [:div {:hx-get "/mod-editor" :hx-trigger "load"}]
          (helpers-docs-section)
          (example-mods-section)]]]
       [:script
        "let q = Object.fromEntries(new URLSearchParams(window.location.search).entries()).q;
         if (typeof q != 'undefined') {
           let elt = htmx.find('input#search-input');
           htmx.ajax('POST', '/search', {source: elt, target: '#result-section', values: {from_qp: q}});
           elt.value = q;
         }"]
       [:script
        "async function clipboardCopy(text) {await navigator.clipboard.writeText(text);}"]
       [:script
        "function switchTab(name) {
           document.querySelectorAll('.tab-content').forEach(el => el.classList.remove('active'));
           document.querySelectorAll('.tab-btn').forEach(el => el.classList.remove('active'));
           document.getElementById('tab-' + name).classList.add('active');
           event.target.classList.add('active');
         }"]])))

(defn result-section [search-term]
  (let [clean (if search-term (str/trim (URLDecoder/decode search-term)) "")
        results (if search-term
                  (sqlite/query (:db @config) ["select * from history where content match ? order by created_at desc limit 20" clean])
                  [])]
    (h/html [:div {:style {:border "3px solid #588" :margin "5px" :padding "5px" :margin-bottom "20px" :border-radius "10px"}}
             [:h3 [:span (count results) " Results for: "[:pre clean]]]
             (into [:div] (map clipboard-line-item results))])))

(defn parse-query-string [query-string]
  (when query-string
    (or
      (let [[_ v] (re-matches #".*from_qp=(.*)$" query-string)] v)
      (-> query-string (str/split #"=") second))))

(defn preview-section [body]
  (let [input (some-> body parse-query-string (URLDecoder/decode) str/trim)
        mods (:modifications @config)]
    (h/html
      (if (or (nil? input) (str/blank? input))
        [:div]
        (let [{:keys [mod result]} (find-matching-mod mods input)]
          (if mod
            [:div {:style {:margin-top "10px"}}
             [:div {:style {:background-color "#e8f5e9"
                            :border "2px solid #4caf50"
                            :border-radius "4px"
                            :padding "8px"
                            :margin-bottom "8px"}}
              [:div {:style {:font-weight "bold" :color "#2e7d32" :margin-bottom "4px"}}
               "Matched!"]
              [:div {:style {:margin-bottom "4px"}}
               [:strong "Pattern: "] [:code (pr-str (:re mod))]]
              [:div {:style {:margin-bottom "4px"}}
               [:strong "Export: "] [:code (pr-str (:source mod))]]]
             [:div {:style {:background-color "#eef2fe"
                            :border "1px solid grey"
                            :border-radius "2px"
                            :padding "6px"}}
              [:strong "Result: "]
              [:pre {:style {:margin "4px 0" :font-family "monospace"}}
               (escape-html result)]]]
            [:div {:style {:margin-top "10px"
                           :color "#888"
                           :font-style "italic"}}
             "No mod matched."]))))))

(defn eval-section [body]
  (let [input (some-> body parse-query-string (URLDecoder/decode) str/trim)]
    (h/html
      (if (or (nil? input) (str/blank? input))
        [:div]
        (let [result (try {:ok (pr-str (eval (read-string (str "(do " input ")"))))}
                          (catch Throwable e {:err (str e)}))]
          [:div {:style {:margin-top "8px"}}
           (if (:ok result)
             [:pre {:style {:background-color "#e8f5e9"
                            :border "1px solid #4caf50"
                            :border-radius "4px"
                            :padding "8px"
                            :font-family "monospace"
                            :white-space "pre-wrap"}}
              (escape-html (:ok result))]
             [:pre {:style {:background-color "#fbe9e7"
                            :border "1px solid #e53935"
                            :border-radius "4px"
                            :padding "8px"
                            :font-family "monospace"
                            :white-space "pre-wrap"
                            :color "#c62828"}}
              (escape-html (:err result))])])))))

(defn load-modifications! [edn-str]
  (when-let [mods (->> (read-string edn-str)
                       (mapv (fn [{:keys [re export]}]
                               {:re re
                                :source export
                                :export (eval export)})))]
    (swap! config assoc :modifications mods)))

(defn parse-form-params [body]
  (when body
    (into {}
      (for [pair (str/split body #"&")
            :let [[k v] (str/split pair #"=" 2)]]
        [(URLDecoder/decode (or k ""))
         (URLDecoder/decode (or v ""))]))))

(defn save-mods-from-form! [body]
  (let [params (parse-form-params body)
        indices (->> (keys params)
                     (keep #(second (re-matches #"re-(\d+)" %)))
                     (map #(Integer/parseInt %))
                     sort)
        edn-vec (vec
                  (for [i indices
                        :let [re-str (get params (str "re-" i))
                              export-str (get params (str "export-" i))]
                        :when (and re-str export-str
                                   (not (str/blank? re-str))
                                   (not (str/blank? export-str)))]
                    {:re (read-string re-str)
                     :export (read-string export-str)}))]
    (spit ".mods.edn" (with-out-str (pprint/pprint edn-vec)))
    (load-modifications! (slurp ".mods.edn"))))

(defn delete-mod! [idx]
  (let [mods (:modifications @config)
        new-mods (vec (concat (subvec mods 0 idx)
                              (subvec mods (inc idx))))
        edn-vec (mapv (fn [{:keys [re source]}]
                         {:re re :export source})
                       new-mods)]
    (spit ".mods.edn" (with-out-str (pprint/pprint edn-vec)))
    (load-modifications! (slurp ".mods.edn"))))

(defn add-mod! []
  (let [mods (:modifications @config)
        edn-vec (into [{:re #"new_pattern" :export "replacement"}]
                      (mapv (fn [{:keys [re source]}] {:re re :export source}) mods))
        _ (spit ".mods.edn" (with-out-str (pprint/pprint edn-vec)))]
    (load-modifications! (slurp ".mods.edn"))))

(defn routes [{:keys [request-method uri query-string] :as req}]
  ;; slurp the body and check it
  (let [body (when-let [b (:body req)] (slurp b))
        path (vec (rest (str/split uri #"/")))
        search-term (or (parse-query-string query-string)
                        (parse-query-string body))]
    (match [request-method path]
           [:get []] {:body (home)}
           [:get ["top-ten"]] {:body (h/html (top-ten-section))}
           [:get ["mod-editor"]] {:body (mod-editor)}
           [:post ["preview"]] {:body (preview-section body)}
           [:post ["eval"]] {:body (eval-section body)}
           [:post ["mod" "add"]] {:body (do (add-mod!) (mod-editor))}
           [:post ["mod" "save"]] {:body (do (save-mods-from-form! body) (mod-editor))}
           [:delete ["mod" (idx :guard [#(re-matches #"\d+" %)])]]
             {:body (do (delete-mod! (Integer/parseInt idx)) (mod-editor))}
           [:post ["search"]] {:body (do
                                       #_(println "------------------------------")
                                       #_(prn req)
                                       #_(prn body)
                                       #_(prn search-term)
                                       (result-section search-term))
                               :headers {"HX-Push" (if search-term (str "?q=" search-term) "/")}}
           :else {:status 404 :body "Error 404: Page not found"})))

(defn open-port [n]
  (when (< n 65535)
    (try (with-open [sock1 (java.net.ServerSocket. n)]
           (.getLocalPort sock1))
         (catch Exception _ (open-port (inc n))))))

(def port (open-port 4321))

(defn play! [& score]
  (let [sounds [:submarine :tink :ping :glass :bottle :purr :frog :sosumi :hero :morse :pop :blow :funk :basso]]
    (doseq [s (filter (set sounds) score)]
      (let [sound (str "/System/Library/Sounds/" (str/capitalize (name s)) ".aiff")]
        (future (shell (<<  "afplay {{sound}}" ))))
      (Thread/sleep 1000))))

(defn prepare-modifications! []
  (when-not (fs/exists? ".mods.edn")
    (fs/copy "example_mods.edn" ".mods.edn"))
  (load-modifications! (slurp ".mods.edn")))

(defn print-config []
  (println "---- config -----------")
  (pprint/pprint @config)
  (println "--- end config --------"))

(defn print-mods []
  (println "---- available mods -----------\n")
  (doseq [re (sort-by pr-str (map :re (:modifications @config)))]
    (println re "\n"))
  (println "--- end available mods --------"))

(defn init! []
  (println "initializing...")
  (execute!
    [(str "create virtual table if not exists history "
          "using fts4"
          " (content TEXT, "
          "  created_at INTEGER)")])
  (reset! *last-clip (last-clip-db))
  (prepare-modifications!)
  ;; start watcher in another thread
  (future
    (while true
      (let [clip (:out (sh "pbpaste"))]
        (when (not= @*last-clip clip)
          (let [mod-clip (maybe-modify (:modifications @config) clip)]
            (when (not= mod-clip clip)
              (println "-------------------")
              (println "Modified Clipboard!")
              (println "From | " clip)
              (println "  To | " mod-clip)
              (sh {:in mod-clip} "pbcopy")
              (play! :basso))
            (insert-clip-if-needed! mod-clip))))
      (Thread/sleep 100)))
  #_(print-config)
  (print-mods)
  (println "initialization complete."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;custom functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn main- [& args]
  ;;(prn args)
  (init!)
  ;; open server:
  (let [url (str "http://localhost:" port "/")]
    (httpkit.server/run-server #'routes {:port port})
    (println "serving" url)
    (when-not ((set args) "--no-open")
      (browse/browse-url url)))

  ;; hang out
  @(promise))

(when (= *file* (System/getProperty "babashka.file"))
  (apply main- *command-line-args*))

;; TODO:
;; paginate history items
;; auto refresh
;; figure a good way to open this
;; highlight + keycommand = modification?
