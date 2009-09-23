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
;; This program saves the pages and its resources of the Clojure.org and
;; Clojure-contrib websites.
;;
;; Usage
;; (save-cljsites/save & option)
;; Save both the Clojure.org and Clojure-contrib websites to the user's current
;; working directory.
;;
;; (save-cljsites/save-org & option)
;; Save the Clojure.org website only.
;;
;; (save-cljsites/save-contrib & option)
;; Save the Clojure-contrib website only.
;;
;; All these three calls take one option :verbose currently, which let them
;; print extra status messages while saving the websites.
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
;;;;

;;;; Constants

(def +Clojure-contrib+ "Clojure-contrib")
(def +Clojure-org+ "Clojure.org")
(def +clojure-contrib-url+ "http://richhickey.github.com/clojure-contrib/")
(def +clojure-org-url+ "http://clojure.org/")
(def +dot-hdrs+ ".hdrs")
(def +dot-html+ ".html")
(def +http-scheme-regex+ #"^http://")
(def +href-regex+ #"^/?(([^.#]+\.?)+)(#.*)?$")
(def +max-total-connections+ 100)
(def +paypal-pixel-gif-url+ "https://www.paypal.com/en_US/i/scr/pixel.gif")
(def +utf-8+ "UTF-8")
(def +version+ "1.2.1")
(def +wiki-link-class+ "wiki_link")
(def +www-wikispaces-com-js-regex+ #"http://www.wikispaces.com/.*\.js$")

;;;; Refs

(def ->home-page (ref "/home"))
(def ->home-page-url (ref +clojure-org-url+))
(def ->root-dir (ref (File. (File. (System/getProperty "user.dir")) +Clojure-org+)))
(def ->root-dir-name (ref +Clojure-org+))
(def ->root-res-dir (ref (File. @->root-dir ".res")))

(def ->saved-hrefs (ref #{}))
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
;;;;

(defmacro with-out-stream
  [stream & body]
  `(binding [*out* ~stream]
	 ~@body))

(defmacro with-out-err
  [& body]
  `(with-out-stream *err*
	 ~@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;

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

(defn consistent-dir-url
  [url]
  (if (= \/ (last url))
	url
	(str url \/)))

(defn init-refs
  [home-page-url root-dir-name]
  (dosync
   (ref-set ->home-page-url (consistent-dir-url home-page-url))
   (ref-set ->home-page (if (= @->home-page-url +clojure-org-url+) "/home" "index.html"))
   (ref-set ->root-dir (File. (File. (System/getProperty "user.dir")) root-dir-name))
   (ref-set ->root-dir-name root-dir-name)
   (ref-set ->root-res-dir (File. @->root-dir ".res")))
  (dosync
   (ref-set ->saved-hrefs #{})
   (ref-set ->saved-res #{})
   (ref-set ->stg (ThreadGroup. "stg"))
   (ref-set ->stg-watcher nil))
  (reset! ->http-client (create-http-client))
  (reset! ->print-msg false)
  (reset! ->print-res false)
  (reset! ->start-nano-time (System/nanoTime)))

(defn process-options
  [opts]
  (let [opts (apply hash-set opts)]
  (when (:verbose opts)
	(reset! ->print-msg true))))

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

(defn anchor-href?
  [href]
  (= \# (first href)))

(defn valid-non-anchor-href?
  [href]
  (let [match (re-find +href-regex+ href)]
	(if match
	  (nil? (last match))
	  false)))

(defn apply-href-pattern
  [href]
  (let [match (re-find +href-regex+ (if (= href "/") @->home-page href))]
	(if match
	  match
	  (throw (Exception. href)))))

(defn page-name-and-match-from-href
  [href]
  (let [match (apply-href-pattern href)
		name (second match)]
	{:match match
	 :name (if (= (nth match (- (count match) 2)) "html")
			 (subs name 0 (- (count name) 5)) ;; 5:=(count ".html")
			 name)}))

(defn page-name-from-href
  [href]
  (:name (page-name-and-match-from-href href)))

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

(defn regen-relative-href
  [href]
  (if (or (anchor-href? href) (re-find +http-scheme-regex+ href))
	href
	(let [name-and-match (page-name-and-match-from-href href)
		  name (:name name-and-match)
		  anchor (last (:match name-and-match))]
	  (str "../" name "/" name +dot-html+ anchor))))

(defn to-res-path
  [path]
  (str "../.res" (subs path (.lastIndexOf path (int \/)))))

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

(defn target-url
  [href]
  (if (re-find +http-scheme-regex+ href)
	href
	(let [parent-url (if (= \/ (first href))
					   @->home-page-url
					   (if *last-url*
						 *last-url*
						 @->home-page-url))]
	  (str (subs parent-url 0 (.lastIndexOf parent-url (int \/)))
		   (if (= \/ (first href)) href (str \/ href))))))

(defn modify-urls-in-line
  ([line]
	 (modify-urls-in-line line (re-seq #"url\(([^()]+)\)" line)))
  ([line urlvsq]
	 (if (empty? urlvsq)
	   line
	   (let [url ((first urlvsq) 1)
			 res-url (target-url url)]
		 (declare save-resource-if-require handle-resources-no-mod-call)
		 (save-resource-if-require {:url res-url :handler handle-resources-no-mod-call})
		 (recur (.replaceAll line url (second (re-find #"/?([^/]+)$" url))) (rest urlvsq))))))

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
  (let [match (re-find #"^[^:]+:\s*(.*)$" header-string)]
	(nth match 1)))

(defn save-response-headers
  [file response]
  (let [hdrs-file (create-new-file (.getParentFile file) (str (.getName file) +dot-hdrs+))
		hdrs-ostrm (FileOutputStream. hdrs-file)]
	(with-open [ostrm-wtr (OutputStreamWriter. hdrs-ostrm +utf-8+)]
	  (with-out-stream ostrm-wtr
		(println "{")
		(print (apply str (map #(when-let [hdr (.getFirstHeader response %)]
								  (when-let [val (.getValue hdr)]
									(when-not (empty? val)
									  (format "%s \"%s\"\n" (keyword %) val))))
							   ["Cache-Control" "Date" "Expires" "Last-Modified"])))
		(println "}")))))

(defn js-from-www-wikispaces-com?
  "Use this function when you want to avoid downloading unnecessary .js files
   from www.wikispaces.com."
  [url]
  (re-find +www-wikispaces-com-js-regex+ url))

(defn true-with-reason
  [url reason]
  (print-msg "Get " url " because " reason)
  true)

(defn require?
  [res-url]
  (if (js-from-www-wikispaces-com? res-url)
	false
	(let [res-fname (subs res-url (.lastIndexOf res-url (int \/)))]
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
	   (let [page-name (page-name-from-href (:href paramap))
			 page-hdrs-file (File. (File. @->root-dir page-name)
								   (str page-name +dot-html+ +dot-hdrs+))]
		 (add-last-modified-param paramap page-hdrs-file))
	   ;; for resource
	   (let [res-url (:url paramap)]
		 (if (js-from-www-wikispaces-com? res-url)
		   paramap
		   (let [res-hdrs-file (File. @->root-res-dir
									  (str (subs res-url (.lastIndexOf res-url (int \/))) +dot-hdrs+))]
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
  (println "Saving" @->root-dir-name "is done!"
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

(defn invoke
  ([f]
	 (f))
  ([f opts]
	 (apply f opts))
  ([f url name opts]
	 (apply f url name opts)))

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
					(gen-alt-content-if true tag attr simple HREF regen-relative-href)
					(when-let [href (.getAttribute attr HREF)]
					  (str-alt-content tag attr HREF (regen-relative-href href) simple))))))

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
  (if-not (empty? *text-handlers*)
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
		  page-name (page-name-from-href (:href paramap))
		  file (create-new-file (File. @->root-dir page-name) (str page-name +dot-html+))
		  file-ostrm (FileOutputStream. file)]
	  (print-msg "Saving " (:url paramap) " to " file)
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
	(when (and (not (js-from-www-wikispaces-com? res-url)) entity)
	  (let [res-file (create-new-file @->root-res-dir (subs res-url (.lastIndexOf res-url (int \/))))]
		(print-msg "Saving " res-url " to " res-file)
		(with-open [res-ostrm (FileOutputStream. res-file)]
		  (.writeTo entity res-ostrm))
		;; Save headers
		(save-response-headers res-file response)
		;; Modify URL()s in the .css files.
		(when (and (nil? no-mod-call) (re-find (re-pattern (str @->home-page-url ".*\\.css$")) res-url))
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
		   :else (print-error-msg "HttpGet for " (:url paramap) " failed with " (.getStatusLine response))))
	   ;; Handle exceptions.
	   (catch Exception e
		 (.abort http-get)
		 (print-error-msg "Fatal error in do-save: " (.getMessage e))
		 (.printStackTrace e))))))

(defn save-resource-if-require
  [paramap]
  (let [res-url (:url paramap)]
	(when (and (not-saved? ->saved-res res-url) (require? res-url))
	  (start-stg-thread #(do-save (add-last-modified-param paramap))))))

(defn save-resources
  [paramap]
  (doseq [kv (filter #(when (not= (first %) :a) %) *res*)]
	(doseq [v (fnext kv)]
	  (save-resource-if-require {:url (target-url v) :handler handle-resources}))))

(defn save-page-and-resources
  [paramap]
  ;; Note: don't bother to call add-last-modified-param now because of the
  ;; reason described in handle-page.
  (do-save paramap #_(add-last-modified-param paramap))
  (when @->print-res
	(print-resources (:url paramap) *title*))
  (save-resources paramap))

(defn save-page
  [href]
  ;; Start the stg watcher if not started yet.
  (when-not @->stg-watcher
	(dosync (ref-set ->stg-watcher (Thread. @->stg #(watch-stg))))
	(.start @->stg-watcher))
  ;; Save this page.
  (binding [*res* (init-res)
			*title* "(no title)"]
	(save-page-and-resources {:href href :url (target-url href) :handler handle-page})
	;; Save pages linked from this page.
	(doseq [href (to-lower-name-set (:a *res*))]
	  (when (and (valid-non-anchor-href? href) (not-saved? ->saved-hrefs href))
		(start-stg-thread #(save-page href))))))

(defn save-website
  [home-page-url root-dir-name & opts]
  (if (nil? @->http-client)
	(do
	  (init-refs home-page-url root-dir-name)
	  (process-options opts)
	  (when (not-saved? ->saved-hrefs @->home-page)
		(start-stg-thread #(save-page @->home-page)))
	  true)
	(do
	  (println "Busy saving a website. Try again later.")
	  false)))

(defn save-org
  "Save the Clojure.org website to the user's current working directory.
  Adding the :verbose option prints extra status messages  while saving the
  website."
  [& opts]
  (invoke save-website +clojure-org-url+ +Clojure-org+ opts))

(defn save-contrib
  "Save the Clojure-contrib website to the user's current working directory.
  Adding the :verbose option prints extra status messages while saving the
  website."
  [& opts]
  (invoke save-website +clojure-contrib-url+ +Clojure-contrib+ opts))

(defn save
  "Save both the Clojure.org and Clojure-contrib websites to the user's current
  working directory. Adding the :verbose option prints extra status messages
  while saving the websites."
  [& opts]
  (.start (Thread. (fn []
					 (when (invoke save-org opts)
					   (loop [stg-watcher @->stg-watcher]
						 (if stg-watcher
						   (.join stg-watcher)
						   (recur @->stg-watcher)))
					   (invoke save-contrib opts))))))
