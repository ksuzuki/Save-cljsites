;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; save_cljsites.clj - save the Clojure websites
;;
;; Copyright (c) 2010 Kei Suzuki. All rights reserved.
;; The use and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which
;; can be found in the file epl-v10.html included with this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
;;
;; Requirements
;; This program requires HttpClient 4.0 and dependencies from the Apache
;; Software Foundation (http://hc.apache.org/).
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns save-cljsites
  (:gen-class)
  (:use [clojure.stacktrace :only (print-cause-trace)]
        [clojure.contrib.command-line])
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
              :api ["http://clojure.github.com/clojure/" "api"],
              :contrib ["http://clojure.github.com/clojure-contrib/" "contrib"]})

;;;; Take these URLs same as the known site URLs.
(def +aliases+ '(["http://github.com/clojure/clojure-contrib/" "contrib"]
                   ["http://github.com/clojure/clojure-contrib" "contrib"]
                     #_["http://github.com/richhickey/clojure-contrib/" "contrib"]
                     #_["http://github.com/richhickey/clojure-contrib" "contrib"]))

;;;; Constants

(def +dot-hdrs+ ".hdrs")
(def +dot-html+ ".html")
(def +dot-html$-regex+ #"\.html$")
(def +external-doc-link+ "external-doc-link")
(def +max-total-connections+ 100)
(def +paypal-pixel-gif-url+ "https://www.paypal.com/en_US/i/scr/pixel.gif")
(def +save-root-dir+ "Clojure-sites")
(def +url-regex+ #"^(\w+:/)?((/|/?[^/#]+)+)(#.*)*$")
(def +user-dir+ (System/getProperty "user.dir"))
(def +utf-8+ "UTF-8")
(def +version+ "2.1.6")
(def +wiki-link-class+ "wiki_link")
(def +wiki-link-ext-class+ "wiki_link_ext")
(def +www-wikispaces-com-js-regex+ #"http://www.wikispaces.com/.*\.js$")

;;;; Refs

(def ->site-url (ref (clojure-org-url)))
(def ->index-html (ref "index.html"))
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
(def ->repl (atom true))

;;;; Vars

(def *alt-content* nil)
(def *content* nil)
(def *external-doc-link* false)
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

(defn init-refs
  [url dir]
  (dosync
   (ref-set ->site-url url)
   (ref-set ->index-html (if (= @->site-url (clojure-org-url)) "home" "index.html"))
   (ref-set ->root-dir (File. (abs-save-root-dir) dir))
   (ref-set ->root-res-dir (File. @->root-dir ".res")))
  (dosync
   (ref-set ->saved-urls (apply hash-set (reduce (fn [lst url]
                                                   (cons (apply str (drop-last url))
                                                         (cons url lst)))
                                                 nil
                                                 (map (fn [[url nm]] url) (vals +sites+)))))
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
   (if (@->obj (.toLowerCase (str item)))
     false
     (do
       (let [item1 (.toLowerCase (str item))
             item2 (if (= (last item1) \/) (apply str (drop-last item1)) item1)]
         (alter ->obj conj item1 item2))
       true))))

(defn print-msg
  [& msgs]
  (when @->print-msg
    (println (apply str msgs))))

(defn print-error-msg
  [& msgs]
  (with-out-err
    (println "ERROR:" (if msgs (apply str msgs) "(no details given)"))))

(defn in-page-anchor?
  [url]
  (= \# (first url)))

(defn apply-url-regex
  [url]
  (let [match (re-find +url-regex+ (if (= url "/")
                                     @->site-url
                                     url))]
    (try
     (or match
         (throw (Exception. (format "re-find failed with %s" url))))
     (catch Exception _
       (throw (Exception. (format "if match failed with %s" url)))))))

(defn url-broker
  [url]
  (let [match (apply-url-regex url)
        third (nth match 3)
        name (if (re-find +dot-html$-regex+ third)
               (subs third 0 (- (count third) 5)) ;; 5 := .html
               third)]
    {:match match
     :proto (second match)
     :path (nth match 2)
     :name name
     :anchor (last match)}))

(defn url-proto
  [url]
  (:proto (url-broker url)))

(defn page-name
  [url]
  (:name (url-broker url)))

(defn http-url?
  [url]
  (let [proto (:proto (url-broker url))]
  (if (and proto (= proto "http:/"))
    true
    false)))

(defn non-anchor-url?
  [url]
  (if (in-page-anchor? url)
    false
    (if-not (:anchor (url-broker url))
      true
      false)))

(defn init-res
  []
  {:a #{} :img #{} :link #{} :script #{}})

(defn create-new-file
  ([dir file-name]
     (create-new-file dir nil file-name))
  ([dir path file-name]
     (let [parent (if path (File. dir path) dir)
           file (File. parent file-name)]
       (when-not (.exists parent)
         (.mkdirs parent))
       (when (.exists file)
         (.delete file))
       (if (.createNewFile file)
         file
         (throw (Exception. (str "Previous delete op failed for " file)))))))

(defn write-to
  [pos & alt-content]
  (print (or *alt-content* (subs *content* *last-pos* pos)))
  (set! *last-pos* pos)
  (set! *alt-content* (first alt-content)))

(defn remember-resource
  [tag val]
  (let [key (keyword (str tag))]
    (set! *res* (conj *res* {key (conj (key *res*) val)}))))

(defn str-alt-content
  [tag attr key val simple nl]
  (str "<" tag
       (apply str (map (fn [k] (format " %s=\"%s\"" k (if (= k key) val (.getAttribute attr k))))
                       (enumeration-seq (.getAttributeNames attr))))
       (if simple "/>" ">")
       nl))

(defn gen-alt-content-if
  [pred tag attr simple key val-fn nl]
  (when pred
    (let [val (.getAttribute attr key)]
      (remember-resource tag val)
      (str-alt-content (str tag) attr key (val-fn val) simple nl))))

(defn add-html-if-missing
  [url]
  (if (or (nil? url) (= url "") (re-find #"(http://|.*\.html(#.*)*$)" url))
    url
    (let [match (re-find #"([^#]+)(#.*)*$" url)]
      (str (nth match 1) +dot-html+ (nth match 2)))))

(defn loc-url-if-known
  [url]
  (let [urlm (url-broker url)
        urlx (str (:proto urlm) (:path urlm))
        fnd (filter #(re-find (re-pattern (first %)) urlx) (concat (map #(val %) +sites+)
                                                                   +aliases+))]
    (if (seq fnd)
      (let [[urlf dir] (first fnd)
            urly (subs urlx (count urlf))]
        ;; Exception: don't get any pages in 'http://clojure.org/space'.
        (if (and (= urlf (clojure-org-url)) (re-find #"^space/" urly))
          url
          (let [depth (count (.split (subs *last-url* (count @->site-url)) "/"))]
            (str (apply str (repeat depth "../"))
                 dir "/"
                 (if (= urlf urlx)
                   (if (= urlf (clojure-org-url))
                     "home.html"
                     "index.html")
                   (add-html-if-missing urly))
                 (:anchor urlm)))))
      url)))

(defn loc-url
  [url]
  (if (in-page-anchor? url)
    url
    (if (http-url? url)
      (loc-url-if-known url)
      (add-html-if-missing (if (= (first url) \/)
                             (subs url 1)
                             url)))))

(defn loc-path
  [url]
  (let [path (subs url (count @->site-url))
        lidx (.lastIndexOf path (int \/))]
    (when (pos? lidx)
      (subs path 0 lidx))))

(defn extract-host-from-url
  [url]
  (second (re-find #"http://([^/]+)/" url)))

(defn url-to-name
  [url]
  (let [name (subs url (inc (.lastIndexOf url (int \/))))
        host (extract-host-from-url (if (http-url? url) url *last-url*))]
    (str (.replaceAll host "\\." "_") "-" name)))

(defn res-path
  [url]
  (str (apply str (repeat (count (filter #(= ".." %) (.split url "/"))) "../"))
       ".res/" (url-to-name url)))

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

(defn absolute-url
  ([url]
     (if (http-url? url)
       url
       (if (= (first url) \/)
         (absolute-url @->site-url (subs url 1))
         (absolute-url (or *last-url* @->site-url) url))))
  ([parent url]
     (let [seqs (butlast (.split (if (= (last parent) \/) (str parent "x") parent) "/"))]
       (loop [seq-abs-url nil
              seqs (concat seqs (.split url "/"))]
         (if (seq seqs)
           (recur (if (= (first seqs) "..")
                    (rest seq-abs-url)
                    (cons (first seqs) seq-abs-url))
                  (rest seqs))
           (apply str (butlast (interleave (reverse seq-abs-url) (repeat "/")))))))))

(defn modify-urls-in-line
  ([line]
     (modify-urls-in-line line (re-seq #"url\(([^()]+)\)" line)))
  ([line urlvsq]
     (if (seq urlvsq)
       (let [url ((first urlvsq) 1)
             res-url (absolute-url url)]
         (declare save-resources handle-resources-no-mod-call)
         (save-resources {:page-url *last-url*} res-url handle-resources-no-mod-call)
         (recur (.replaceAll line url (url-to-name res-url)) (rest urlvsq)))
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
       (let [page-url (:page-url paramap)
             path (loc-path page-url)
             page-name (page-name page-url)
             page-hdrs-file (File. (if path (File. @->root-dir path) @->root-dir)
                                   (str page-name +dot-html+ +dot-hdrs+))]
         (add-last-modified-param paramap page-hdrs-file))
       ;; for resource
       (let [res-url (:res-url paramap)]
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
  (println "=== Saving Clojure"
           (second (val (first (filter #(= @->site-url (first (val %))) +sites+))))
           "site completed"
           (format "(%.2f msecs) ===" (/ (double (- (System/nanoTime) @->start-nano-time)) 1000000.0))))

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

(defn handle-error
  [errmsg pos])

(defmulti handle-end-tag
  (fn [tag pos]
    tag))

(defmethod handle-end-tag :default
  [tag pos]
  (write-to pos))

(defmethod handle-end-tag HTML$Tag/SPAN
  [tag pos]
  (when *external-doc-link*
    (set! *external-doc-link* false))
  (write-to pos))

(defmulti handle-tags
  (fn [tag attr pos simple]
    tag))

(defmethod handle-tags :default
  [tag attr pos simple]
  (write-to pos))

(defmethod handle-tags HTML$Tag/A
  [tag attr pos simple]
  (let [HREF HTML$Attribute/HREF]
    (write-to pos (if (or (.containsAttribute attr HTML$Attribute/CLASS +wiki-link-class+)
                          (and (.containsAttribute attr HTML$Attribute/CLASS +wiki-link-ext-class+)
                               (not (url-proto (.getAttribute attr HREF))))
                          *external-doc-link*)
                    (gen-alt-content-if true
                                        tag attr simple HREF loc-url nil)
                    (when-let [url (.getAttribute attr HREF)]
                      (when (not-empty url) ;; url can be empty.
                        (str-alt-content tag attr HREF (loc-url url) simple nil)))))))

(defmethod handle-tags HTML$Tag/IMG
  [tag attr pos simple]
  (write-to pos (when-not (= (.getAttribute attr HTML$Attribute/SRC) +paypal-pixel-gif-url+)
                  ;; A workaround for the Paypal's pixel.gif img element that is not ending
                  ;; with '/>'.
                  (gen-alt-content-if true
                                      tag attr simple HTML$Attribute/SRC #(res-path %) nil))))

(defmethod handle-tags HTML$Tag/LINK
  [tag attr pos simple]
  (write-to pos (gen-alt-content-if (or (.containsAttribute attr HTML$Attribute/REL "stylesheet")
                                        (.containsAttribute attr HTML$Attribute/REL "icon")
                                        (.containsAttribute attr HTML$Attribute/REL "shortcut icon"))
                                    tag attr simple HTML$Attribute/HREF #(res-path %) "\n")))

(defmethod handle-tags HTML$Tag/SCRIPT
  [tag attr pos simple]
  (write-to pos (gen-alt-content-if (and (.containsAttribute attr HTML$Attribute/TYPE "text/javascript")
                                         (.getAttribute attr HTML$Attribute/SRC))
                                    tag attr simple HTML$Attribute/SRC #(res-path %) nil)))

(defmethod handle-tags HTML$Tag/SPAN
  [tag attr pos simple]
  (when (.containsAttribute attr HTML$Attribute/ID +external-doc-link+)
    (set! *external-doc-link* true))
  (write-to pos))

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
            *text-handlers* '()
            *external-doc-link* false]
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
    (let [page-url (:page-url paramap)
          path (loc-path page-url)
          page-name (page-name page-url)
          file (create-new-file @->root-dir path (str page-name +dot-html+))
          file-ostrm (FileOutputStream. file)
          body-str (EntityUtils/toString entity +utf-8+)]
      (print-msg "Saving page " page-url " to " file)
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
  (let [res-url (:res-url paramap)
        entity (.getEntity response)]
    (when (and (not (is-js-from-www-wikispaces-com res-url)) entity)
      (let [res-file (create-new-file @->root-res-dir (url-to-name res-url))]
        (print-msg "Saving resource " res-url " to " res-file)
        (with-open [res-ostrm (FileOutputStream. res-file)]
          (.writeTo entity res-ostrm))
        ;; Save headers
        (save-response-headers res-file response)
        ;; Modify URL()s in the .css files.
        (when (and (nil? no-mod-call) (re-find (re-pattern (str @->site-url ".*\\.css$")) res-url))
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
    (let [url (:url paramap)
          http-get (HttpGet. url)]
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
          :else (print-error-msg "HttpGet for " url " failed with " (.getStatusLine response))))
       ;; Handle exceptions.
       (catch Exception e
         (.abort http-get)
         (print-error-msg "do-save for " url ": " (.getMessage e))
         (print-cause-trace e))))))

(defn save-resources
  ([paramap]
     (doseq [kv (filter #(when (not= (first %) :a) %) *res*)]
       (doseq [v (fnext kv)]
         (save-resources paramap (absolute-url v) handle-resources))))
  ([paramap res-url handler]
     (when (and (not-saved? ->saved-res res-url) (need-this-res? res-url))
       (start-stg-thread #(do-save (add-last-modified-param (assoc paramap
                                                              :url res-url
                                                              :res-url res-url
                                                              :handler handler)))))))

(defn save-page-and-resources
  [paramap]
  ;; Note: don't bother to call add-last-modified-param now because of the
  ;; reason described in handle-page.
  (do-save paramap #_(add-last-modified-param paramap))
  (when @->print-res
    (print-resources (:url paramap) *title*))
  (save-resources paramap))

(defn save-page
  [page-url]
  ;; Start the stg watcher if not started yet.
  (when-not @->stg-watcher
    (dosync (ref-set ->stg-watcher (Thread. @->stg #(watch-stg))))
    (.start @->stg-watcher))
  ;; Save this page.
  (binding [*res* (init-res)
            *title* "(no title)"
            *last-url* (absolute-url page-url)]
    (save-page-and-resources {:url *last-url*
                              :page-url *last-url*
                              :handler handle-page})
    ;; Save pages linked from this page.
    (doseq [page-url (:a *res*)]
      (when (non-anchor-url? page-url)
        (let [abs-page-url (absolute-url page-url)]
          (when (not-saved? ->saved-urls abs-page-url)
            (start-stg-thread #(save-page abs-page-url))))))))

(defn save-website
  [url dir opts]
  (if (nil? @->http-client)
    (do
      (init-refs url dir)
      (process-options opts)
      (when (not-saved? ->saved-urls (absolute-url @->index-html))
        (start-stg-thread #(save-page @->index-html)))
      true)
    (do
      (println "Skip Downloading" url "because the program is still busy saving" @->site-url)
      false)))

(defn print-sites
  "Print all site keys, their URLs and directories to save."
  []
  (println (format "%-16s %-64s %s" "Site key" "URL" "Directory to save"))
  (doseq [[k [u d]] +sites+]
    (println (format "%-16s %-64s %s" (if @->repl k (name k)) u (File. (abs-save-root-dir) d)))))

(defn save
  "Save the Clojure website specified by a site key to the user's current
  working directory. Adding the :verbose option will print extra status
  messages."
  [site-key & opts]
  (if ((apply hash-set (keys +sites+)) site-key)
    (let [[url dir] (+sites+ site-key)]
      (save-website url dir opts))
    (println (str "Unknown site key. "
                  (if @->repl
                    "Run print-sites" 
                    "Use the --info option")
                  " to print knwon site keys and try again."))))

(defn save-all
  "Save the Clojure websites to the user's current working directory.
  Adding the :verbose option will print extra status messages."
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

(defn -main
  [& argv]
  (try
   (with-command-line argv
     (str "save-cljsites version " +version+ " -- save Clojure sites\n"
          "usage: save-cljsites [options]")
     [[help?    h? "Print this help message"]
      [info?    i? "Print all Clojure site keys, their URLs and directories to save"]
      [site     s  "Save Clojure website specified by a site key"]
      [verbose? v? "Print extra status messages"]]
     ;;
     (swap! ->repl (fn [_ v] v) false)
     (cond
      info? (print-sites)
      site (save (keyword site) (if verbose? :verbose))
      :else (save-all (if verbose? :verbose))))
   (catch Exception _
     (println "Exception error occurred, probably due to invalid option?"))))
