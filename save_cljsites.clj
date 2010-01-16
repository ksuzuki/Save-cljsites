;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; save_cljsites.clj - save the Clojure websites
;;
;; Copyright (c) Kei Suzuki. All rights reserved.
;; The use and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which
;; can be found in the file epl-v10.html included with this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
;;
;; This program saves the pages and its resources of some known Clojure
;; websites. Those are:
;;   http://clojure.org/
;;   http://richhickey.github.com/clojure/
;;   http://richhickey.github.com/clojure-contrib/
;;
;; Usage
;; (save-cljsites/save-all & option)
;; Save the Clojure websites to the user's current working directory. Adding the
;; :verbose option prints extra status messages while saving the websites.
;;
;; (save-cljsites/save site-key & option)
;; Save the Clojure website specified by the given site key to the user's
;; current working directory. Adding the :verbose option prints extra status
;; messages while saving the website.
;;
;; e.g. (save-cljsites/save :org :verbose)
;;
;; Use print-sites to print known site keys, their URLs and save directory
;; names.
;;
;; (savae-cljsites/print-sites)
;; Print all Clojure site keys, their URLs and save directory names.
;;
;; Requirements
;; This program requires HttpClient 4.0 and dependencies from the Apache
;; Software Foundation (http://hc.apache.org/).
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns save-cljsites
  (:import (java.io
            BufferedReader BufferedWriter File FileOutputStream FileReader
            FileWriter OutputStreamWriter StringReader)
           (javax.swing.text.html
            HTML$Attribute HTML$Tag HTMLEditorKit$ParserCallback)
           (javax.swing.text.html.parser
            ParserDelegator)
           (java.util
            Date)
           ;;
           (org.apache.http
            HttpStatus HttpVersion)
           (org.apache.http.client.methods
            HttpGet)
           (org.apache.http.conn.params
            ConnManagerParams)
           (org.apache.http.conn.scheme
            PlainSocketFactory Scheme SchemeRegistry)
           (org.apache.http.conn.ssl
            SSLSocketFactory)
           (org.apache.http.impl.client
            DefaultHttpClient)
           (org.apache.http.impl.conn.tsccm
            ThreadSafeClientConnManager)
           (org.apache.http.impl.cookie
            DateUtils)
           (org.apache.http.params
            BasicHttpParams HttpProtocolParams)
           (org.apache.http.protocol
            BasicHttpContext)
           (org.apache.http.util
            EntityUtils)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Macros

(defmacro clojure-org-url
  []
  `(first (+sites+ :org)))

(defmacro abs-save-root-dir
  []
  `(File. +user-dir+ +save-root-dir+))

(defmacro with-out-stream
  [stream & body]
  `(binding [*out* ~stream]
     ~@body))

(defmacro with-out-err
  [& body]
  `(with-out-stream *err*
     ~@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; constants and variables

;;;; Known sites: map of site-key as key and [site-url site-dir] as value
;;;; site-dir will be created under (abs-save-root-dir)

(def +sites+ {:org ["http://clojure.org/" "org"],
              :api ["http://richhickey.github.com/clojure/" "api"],
              :contrib ["http://richhickey.github.com/clojure-contrib/" "contrib"]})

;;;; Constants

(def +dot-hdrs+ ".hdrs")
(def +dot-html+ ".html")
(def +http-scheme-regex+ #"^http://")
(def +max-total-connections+ 100)
(def +paypal-pixel-gif-url+ "https://www.paypal.com/en_US/i/scr/pixel.gif")
(def +save-root-dir+ "Clojure-sites")
(def +sonian-net+ "www.sonian.net")
(def +url-regex+ #"^/?(([^.#]+\.?)+)(#.*)?$")
(def +user-dir+ (System/getProperty "user.dir"))
(def +utf-8+ "UTF-8")
(def +version+ "1.3.0")
(def +wiki-link-class+ "wiki_link")
(def +www-wikispaces-com-js-regex+ #"http://www.wikispaces.com/.*\.js$")

;;;; Refs

(def ->home-page (ref "/home"))
(def ->host-url (ref (clojure-org-url)))
(def ->root-dir (ref (File. (abs-save-root-dir) (second (+sites+ :org)))))
(def ->root-res-dir (ref (File. @->root-dir ".res")))

(def ->saved-urls (ref #{}))
(def ->saved-res (ref #{}))

(def ->stg (ref nil)) ;; "Saving" Thread group
(def ->stg-watcher (ref nil))

(def ->http-client (atom nil))
(def ->print-msg (atom false))
(def ->print-res (atom false))
(def ->start-nano-time (atom 0))

;;;; Vars

(def *alt-content* nil)
(def *content* nil)
(def *last-pos* 0)
(def *last-url* nil)
(def *res* nil)
(def *text-handlers* '())
(def *title* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; fns

(defn init-http-params
  [params]
  (ConnManagerParams/setMaxTotalConnections params +max-total-connections+)
  (doto params
    (HttpProtocolParams/setVersion HttpVersion/HTTP_1_1)
    (HttpProtocolParams/setContentCharset +utf-8+)))

(defn init-scheme-registry
  [scheme-registry]
  (doto scheme-registry
    (.register (Scheme. "http" (PlainSocketFactory/getSocketFactory) 80))
    (.register (Scheme. "https" (SSLSocketFactory/getSocketFactory) 443))))

(defn create-http-client
  []
  (let [params (BasicHttpParams.)]
    (init-http-params params)
    (let [scheme-registry (SchemeRegistry.)]
      (init-scheme-registry scheme-registry)
      (let [cm (ThreadSafeClientConnManager. params scheme-registry)]
        (DefaultHttpClient. cm params)))))

(defn norm-url
  [url]
  (str url (when-not (= (last url) \/) \/)))

(defn init-refs
  [url dir]
  (dosync
   (ref-set ->host-url (norm-url url))
   (ref-set ->home-page (if (= @->host-url (clojure-org-url)) "/home" "index.html"))
   (ref-set ->root-dir (File. (abs-save-root-dir) dir))
   (ref-set ->root-res-dir (File. @->root-dir ".res")))
  (dosync
   (ref-set ->saved-urls #{})
   (ref-set ->saved-res #{})
   (ref-set ->stg (ThreadGroup. (str "stg for " url)))
   (ref-set ->stg-watcher nil))
  (reset! ->http-client (create-http-client))
  (reset! ->print-msg false)
  (reset! ->print-res false)
  (reset! ->start-nano-time (System/nanoTime)))

(defn process-options
  [opts]
  (when (seq opts)
    (let [opts (into #{} opts)]
      (when (:verbose opts)
        (reset! ->print-msg true)))))

(defn not-saved?
  [->obj item]
  (dosync
   (if (@->obj item)
     false
     (do
       (alter ->obj conj item)
       true))))

(defn print-msg
  [& msgs]
  (when @->print-msg
    (println (apply str msgs))))

(defn print-error-msg
  [& msgs]
  (with-out-err
    (println "ERROR:" (if msgs (apply str msgs) "(no details given)"))))

(defn in-page-anchor-url?
  [url]
  (= \# (first url)))

(defn http-url
  [url]
  (re-find +http-scheme-regex+ url))

(defn valid-non-anchor-url?
  [url]
  (let [match (re-find +url-regex+ url)]
    (if match
      (nil? (last match))
      false)))

(defn apply-url-pattern
  [url]
  (let [match (re-find +url-regex+ (if (= url "/") @->home-page url))]
    (try
     (if match
       match
       (throw (Exception. (format "re-find failed with %s" url))))
     (catch Exception e
       (throw (Exception. (format "if match failed with %s" url)))))))

(defn page-name-and-match-from-url
  [url]
  (let [match (apply-url-pattern url)
        name (second match)]
    {:match match
     :name (if (= (nth match (- (count match) 2)) "html")
             (subs name 0 (- (count name) 5)) ;; 5:=(count ".html")
             name)}))

(defn to-page-name
  [url]
  (:name (page-name-and-match-from-url url)))

(defn init-res
  []
  {:a #{} :img #{} :link #{} :script #{}})

(defn create-new-file
  [dir file-name]
  (let [file (File. dir file-name)]
    (when-not (.exists dir)
      (.mkdirs dir))
    (when (.exists file)
      (.delete file))
    (if (.createNewFile file)
      file
      (throw (Exception. (str "Previous delete op failed for " file))))))

(defn write-to
  [pos & alt-content]
  (print (if *alt-content* *alt-content* (subs *content* *last-pos* pos)))
  (set! *last-pos* pos)
  (set! *alt-content* (first alt-content)))

(defn remember-resource
  [tag val]
  (let [key (keyword (str tag))]
    (set! *res* (conj *res* {key (conj (key *res*) val)}))))

(defn str-alt-content
  [tag attr key val simple]
  (str "<" tag
       (apply str (map (fn [k] (format " %s=\"%s\"" k (if (= k key) val (.getAttribute attr k))))
                       (enumeration-seq (.getAttributeNames attr))))
       (if simple "/" ">")))

(defn gen-alt-content-if
  [pred tag attr simple key val-fn]
  (when pred
    (let [val (.getAttribute attr key)]
      (remember-resource tag val)
      (str-alt-content (str tag) attr key (val-fn val) simple))))

(defn replace-to-rel-url-if-known
  [url]
  (let [nurl (norm-url url)
        fnd (filter #(= nurl (first %)) (map #(val %) +sites+))]
    (if (seq fnd)
      (let [[url dir] (first fnd)] 
        (str "../../" dir (if (= url (clojure-org-url))
                            "/home/home.html"
                            "/index/index.html")))
      url)))

(defn regen-relative-url
  [url]
  (if (http-url url)
    (replace-to-rel-url-if-known url)
    (if (in-page-anchor-url? url)
      url
      (let [name-and-match (page-name-and-match-from-url url)
            name (:name name-and-match)
            anchor (last (:match name-and-match))]
        (str "../" name "/" name +dot-html+ anchor)))))

(defn extract-host-from-url
  [url]
  (second (re-find #"http://([^/]+)/" url)))

(defn url-to-name
  [url]
  (let [name (subs url (inc (.lastIndexOf url (int \/))))
        host (extract-host-from-url (if (http-url url) url *last-url*))]
    (str (.replaceAll host "\\." "_") "-" name)))

(defn to-res-path
  [url]
  (str "../.res/" (url-to-name url)))

(defn post-text-handler
  [txthdr]
  (set! *text-handlers* (cons txthdr *text-handlers*)))

(defn call-text-handler
  [data pos]
  ((first *text-handlers*) data pos)
  (set! *text-handlers* (rest *text-handlers*)))

(defn print-resources
  [url title]
  (println (format ";;;; Resources used by the '%s' page (%s)" title url))
  (doseq [kv *res*]
    (println ";; Tag" (name (key kv)))
    (doseq [v (val kv)]
      (println v))
    (println)))

(defn complete-url
  [url]
  (if (re-find +http-scheme-regex+ url)
    url
    (let [parent (if (= (first url) \/)
                       @->host-url
                       (if *last-url*
                         *last-url*
                         @->host-url))]
      (str (subs parent 0 (.lastIndexOf parent (int \/)))
           (if (= (first url) \/) url (str \/ url))))))

(defn res-url-filter
  "Special resource url filter: currently applied only for retreiving the
   logo.gif from www.sonian.net. The site server fails to respond quite
   often. In the case when it succeeds to respond, the url is actually
   redirected to its .com domain. So try to get the gif from the .com
   directly, hoping it can avoid response failure."
  [url]
  (if (= (extract-host-from-url url) +sonian-net+)
    (.replace url ".net/" ".com/")
    url))

(defn modify-urls-in-line
  ([line]
     (modify-urls-in-line line (re-seq #"url\(([^()]+)\)" line)))
  ([line urlvsq]
     (if (seq urlvsq)
       (let [url ((first urlvsq) 1)
             res-url (res-url-filter (complete-url url))]
         (declare save-resources handle-resources-no-mod-call)
         (save-resources res-url handle-resources-no-mod-call)
         (recur (.replaceAll line url (to-res-path res-url)) (rest urlvsq)))
       line)))

(defn modify-urls-in-file
  [file]
  (print-msg "Modifying url()s in " file)
  (let [tmpfile (File/createTempFile "___" nil)]
    (with-open [writer (BufferedWriter. (FileWriter. tmpfile))]
      (loop [lines (line-seq (BufferedReader. (FileReader. file)))]
        (when lines
          (doto writer
            (.write (modify-urls-in-line (first lines)))
            (.newLine))
          (recur (next lines)))))
    (if (.delete file)
      (.renameTo tmpfile file)
      (print-error-msg "Delete-and-renaming " file " failed."))))
      
(defn to-lower-name-set
  [s]
  (into #{} (map #(.toLowerCase (str %)) s)))

(defn get-header-value-string
  [header-string]
  (second (re-find #"^[^:]+:\s*(.*)$" header-string)))

(defn save-response-headers
  [file response]
  (let [hdrs-file (create-new-file (.getParentFile file) (str (.getName file) +dot-hdrs+))
        hdrs-ostrm (FileOutputStream. hdrs-file)]
    (with-open [ostrm-wtr (OutputStreamWriter. hdrs-ostrm +utf-8+)]
      (with-out-stream ostrm-wtr
        (println "{")
        (print (apply str (map #(when-let [hdr (.getFirstHeader response %)]
                                  (when-let [val (.getValue hdr)]
                                    (when (not-empty val)
                                      (format "%s \"%s\"\n" (keyword %) val))))
                               ["Cache-Control" "Date" "Expires" "Last-Modified"])))
        (println "}")))))

(defn is-js-from-www-wikispaces-com
  "Use this function when you want to avoid downloading unnecessary .js files
   from www.wikispaces.com."
  [url]
  (re-find +www-wikispaces-com-js-regex+ url))

(defn true-with-reason
  [url reason]
  (print-msg "Get " url " because " reason)
  true)

(defn need-this-res?
  [res-url]
  (if (is-js-from-www-wikispaces-com res-url)
    false
    (let [res-fname (url-to-name res-url)]
      (if-not (.exists (File. @->root-res-dir res-fname))
        (true-with-reason res-url "the file doesn't exist")
        (let [res-hdrs-file (File. @->root-res-dir (str res-fname +dot-hdrs+))]
          (if-not (.exists res-hdrs-file)
            (true-with-reason res-url "the header info doesn't exist")
            (let [res-hdrs (read-string (slurp (.getPath res-hdrs-file)))
                  expiry-str (:Expires res-hdrs)]
              (if (or (nil? expiry-str) (empty? expiry-str))
                (true-with-reason res-url "expiry info is missing")
                (let [date-str (:Date res-hdrs)]
                  (if (or (nil? date-str) (empty? date-str))
                    (true-with-reason res-url "date info is missing")
                    (let [expiry-date (DateUtils/parseDate expiry-str)
                          date (DateUtils/parseDate date-str)]
                      (if (<= (.compareTo expiry-date date) 0)
                        (true-with-reason res-url "the server wish it's not cached")
                        (if (< (.compareTo expiry-date (Date.)) 0)
                          (true-with-reason res-url "it expired")
                          false)))))))))))))

(defn add-last-modified-param
  ([paramap]
     (declare handle-page)
     (if (= (:handler paramap) handle-page)
       ;; for page
       (let [page-name (to-page-name (:page paramap))
             page-hdrs-file (File. (File. @->root-dir page-name)
                                   (str page-name +dot-html+ +dot-hdrs+))]
         (add-last-modified-param paramap page-hdrs-file))
       ;; for resource
       (let [res-url (:url paramap)]
         (if (is-js-from-www-wikispaces-com res-url)
           paramap
           (let [res-hdrs-file (File. @->root-res-dir
                                      (str (url-to-name res-url) +dot-hdrs+))]
             (add-last-modified-param paramap res-hdrs-file))))))
  ([paramap hdrs-file]
     (if (.exists hdrs-file)
       (let [hdrs (read-string (slurp (.getPath hdrs-file)))
             last-modified (:Last-Modified hdrs)]
         (if last-modified
           (conj paramap {:last-modified last-modified})
           paramap))
       paramap)))

(defn cleanup
  []
  (.. @->http-client getConnectionManager shutdown)
  (reset! ->http-client nil)
  (dosync
   (ref-set ->saved-urls #{})
   (ref-set ->saved-res #{})
   (ref-set ->stg nil)
   (ref-set ->stg-watcher nil))
  (println "Saving Clojure"
           (second (val (first (filter #(= @->host-url (first (val %))) +sites+))))
           "site completed"
           (format "(%.2f msecs)" (/ (double (- (System/nanoTime) @->start-nano-time)) 1000000.0))))

(defn watch-stg
  "Due to the way to create the thread that runs this function the thread
   itself belongs to the stg. So see it as no Saving thread is running when
   the active count of the stg gets down to 1, not zero."
  []
  (loop [active-count (.activeCount @->stg)]
    (if (< 1 active-count)
      (recur (.activeCount @->stg))
      (cleanup))))

(defn start-stg-thread
  [f]
  (.start (Thread. @->stg f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;

(defn handle-flush
  [])

(defn handle-comment
  [data pos]
  (write-to pos))

(defn handle-EOL-string
  [eol]
  (when (pos? *last-pos*)
    (println (subs *content* *last-pos*))))

(defn handle-end-tag
  [tag pos]
  (write-to pos))

(defn handle-error
  [errmsg pos])

(defmulti handle-tags
  (fn [tag attr pos simple]
    tag))

(defmethod handle-tags :default
  [tag attr pos simple])

(defmethod handle-tags HTML$Tag/A
  [tag attr pos simple]
  (let [HREF HTML$Attribute/HREF]
    (write-to pos (if (.containsAttribute attr HTML$Attribute/CLASS +wiki-link-class+)
                    (gen-alt-content-if true tag attr simple HREF regen-relative-url)
                    (when-let [url (.getAttribute attr HREF)]
                      (when (not-empty url) ;; url can be empty.
                        (str-alt-content tag attr HREF (regen-relative-url url) simple)))))))

(defmethod handle-tags HTML$Tag/IMG
  [tag attr pos simple]
  (write-to pos (when-not (= (.getAttribute attr HTML$Attribute/SRC) +paypal-pixel-gif-url+)
                  ;; A workaround for the Paypal's pixel.gif img element that is not ending
                  ;; with '/>'.
                  (gen-alt-content-if true
                                      tag attr simple HTML$Attribute/SRC #(to-res-path %)))))

(defmethod handle-tags HTML$Tag/LINK
  [tag attr pos simple]
  (write-to pos (gen-alt-content-if (.containsAttribute attr HTML$Attribute/REL "stylesheet")
                                    tag attr simple HTML$Attribute/HREF #(to-res-path %))))

(defmethod handle-tags HTML$Tag/SCRIPT
  [tag attr pos simple]
  (write-to pos (gen-alt-content-if (and (.containsAttribute attr HTML$Attribute/TYPE "text/javascript")
                                         (.getAttribute attr HTML$Attribute/SRC))
                                    tag attr simple HTML$Attribute/SRC #(to-res-path %))))

(defmethod handle-tags HTML$Tag/TITLE
  [tag attr pos simple]
  (post-text-handler (fn [data pos]
                       (set! *title* (String. data))
                       (write-to pos)))
  (write-to pos))

(defn handle-text
  [data pos]
  (if (seq *text-handlers*)
    (call-text-handler data pos)
    (write-to pos)))

(defn parse-and-save-page
  [content]
  (binding [*content* content
            *last-pos* 0
            *alt-content* nil
            *text-handlers* '()]
    (let [rd (BufferedReader. (StringReader. *content*))
          pd (ParserDelegator.)
          cb (proxy [HTMLEditorKit$ParserCallback] []
               (flush []                        (handle-flush))
               (handleComment [data pos]        (handle-comment data pos))
               (handleEndOfLineString [eol]     (handle-EOL-string eol))
               (handleEndTag [tag pos]          (handle-end-tag tag pos))
               (handleError [errmsg pos]        (handle-error errmsg pos))
               (handleSimpleTag [tag attr pos]  (handle-tags tag attr pos true))
               (handleStartTag [tag attr pos]   (handle-tags tag attr pos false))
               (handleText [data pos]           (handle-text data pos)))]
      (.parse pd rd cb true))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;

(defn handle-page
  [response paramap]
  (when-let [entity (.getEntity response)]
    (let [body-str (EntityUtils/toString entity +utf-8+)
          page-name (to-page-name (:page paramap))
          file (create-new-file (File. @->root-dir page-name) (str page-name +dot-html+))
          file-ostrm (FileOutputStream. file)]
      (print-msg "Saving page " (:url paramap) " to " file)
      (with-open [ostrm-wtr (OutputStreamWriter. file-ostrm +utf-8+)]
        (with-out-stream ostrm-wtr
          (parse-and-save-page body-str)))
      ;; Save response headers
      ;; Note: this is not performed today because the server says 'no-cache'
      ;; and doesn't provide Last-Modified date for any page so that we can't
      ;; make more efficient use of the If-Modified-Since header for pages.
      #_(save-response-headers file response))))
  
(defn handle-resources
  [response paramap & no-mod-call]
  (let [res-url (:url paramap)
        entity (.getEntity response)]
    (when (and (not (is-js-from-www-wikispaces-com res-url)) entity)
      (let [res-file (create-new-file @->root-res-dir (url-to-name res-url))]
        (print-msg "Saving resource " res-url " to " res-file)
        (with-open [res-ostrm (FileOutputStream. res-file)]
          (.writeTo entity res-ostrm))
        ;; Save headers
        (save-response-headers res-file response)
        ;; Modify URL()s in the .css files.
        (when (and (nil? no-mod-call) (re-find (re-pattern (str @->host-url ".*\\.css$")) res-url))
          (modify-urls-in-file res-file))))))

(defn handle-resources-no-mod-call
  [response paramap]
  (handle-resources response paramap true))

(defn handle-not-status
  [status-msg response paramap msg-prefix]
  (when-let [entity (.getEntity response)]
    (.consumeContent entity))
  (print-msg msg-prefix (:url paramap) " not " status-msg))

(defn do-save
  [paramap]
  (binding [*last-url* (:url paramap)]
    (let [http-get (HttpGet. *last-url*)]
      ;; Add the If-Modified-Since param if available.
      (when (:last-modified paramap)
        (.setHeader http-get "If-Modified-Since" (:last-modified paramap)))
      (try
       ;; Execute HttpGet and handle the response.
       (let [context (BasicHttpContext.)
             response (.execute @->http-client http-get context)
             status-code (.. response getStatusLine getStatusCode)]
         (cond
           (= status-code HttpStatus/SC_OK) ((:handler paramap) response paramap)
           (and (:last-modified paramap) (= status-code HttpStatus/SC_NOT_MODIFIED)) (handle-not-status "modified" response paramap nil)
           (= status-code HttpStatus/SC_NOT_FOUND) (handle-not-status "found" response paramap "WARNING: ")
           :else (print-error-msg "HttpGet for " *last-url* " failed with " (.getStatusLine response))))
       ;; Handle exceptions.
       (catch Exception e
         (.abort http-get)
         (print-error-msg "do-save for " *last-url* ": " (.getMessage e))
         #_(.printStackTrace e))))))

(defn save-resources
  ([paramap]
     (doseq [kv (filter #(when (not= (first %) :a) %) *res*)]
       (doseq [v (fnext kv)]
         (save-resources (res-url-filter (complete-url v)) handle-resources))))
  ([res-url handler]
     (when (and (not-saved? ->saved-res res-url) (need-this-res? res-url))
       (start-stg-thread #(do-save (add-last-modified-param {:url res-url :handler handler}))))))

(defn save-page-and-resources
  [paramap]
  ;; Note: don't bother to call add-last-modified-param now because of the
  ;; reason described in handle-page.
  (do-save paramap #_(add-last-modified-param paramap))
  (when @->print-res
    (print-resources (:url paramap) *title*))
  (save-resources paramap))

(defn save-page
  [page]
  ;; Start the stg watcher if not started yet.
  (when-not @->stg-watcher
    (dosync (ref-set ->stg-watcher (Thread. @->stg #(watch-stg))))
    (.start @->stg-watcher))
  ;; Save this page.
  (binding [*res* (init-res)
            *title* "(no title)"]
    (save-page-and-resources {:page page :url (complete-url page) :handler handle-page})
    ;; Save pages linked from this page.
    (doseq [page (to-lower-name-set (:a *res*))]
      (when (and (valid-non-anchor-url? page) (not-saved? ->saved-urls page))
        (start-stg-thread #(save-page page))))))

(defn save-website
  [url dir opts]
  (if (nil? @->http-client)
    (do
      (init-refs url dir)
      (process-options opts)
      (when (not-saved? ->saved-urls @->home-page)
        (start-stg-thread #(save-page @->home-page)))
      true)
    (do
      (println "Skip Downloading" url "because the program is still busy saving" @->host-url)
      false)))

(defn print-sites
  "Print all site keys, their URLs and save directory names."
  []
  (printf "%-16s %-64s %s\n" "Site key" "URL" "Save directory")
  (doseq [[k [u d]] +sites+]
    (printf "%-16s %-64s %s\n" k u (File. (abs-save-root-dir) d))))

(defn save
  "Save the Clojure website specified by the given site key to the user's
  current working directory. Adding the :verbose option prints extra status
  messages while saving the website.
  Use print-sites to print known site keys, their URLs and save directory
  names."
  [site-key & opts]
  (if ((apply hash-set (keys +sites+)) site-key)
    (let [[url dir] (+sites+ site-key)]
      (save-website url dir opts))
    "Unknown site key. Do print-sites for knwon site keys and try again."))

(defn save-all
  "Save the Clojure websites to the user's current working directory.
  Adding the :verbose option prints extra status messages while saving the websites."
  [& opts]
  (letfn [(wait-for-stg-watcher-start-and-end
           []
           (loop [sw @->stg-watcher]
             (when (nil? sw)
               (recur @->stg-watcher)))
           (.join @->stg-watcher)
           (loop [sw @->stg-watcher]
             (when sw
               (recur @->stg-watcher))))
          (do-save-all
           []
           (doseq [[_ [url dir]] +sites+]
             (when (save-website url dir opts)
               (wait-for-stg-watcher-start-and-end))))]
    (.start (Thread. do-save-all))))
