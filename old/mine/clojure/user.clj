;; From: http://groups.google.com/group/clojure/msg/96ed91f823305f02
;; usage:
;; (show Object)   ; give it a class
;; (show Object 1) ; a class and a method number to see details
;; (show {})       ; or give it an instance

(import '(java.io LineNumberReader InputStreamReader PushbackReader)
	'(java.lang.reflect Modifier Method Constructor)
	'(clojure.lang RT))


(defn show
  ([x] (show x nil))
  ([x i]
     (let [c (if (class? x) x (class x))
	   items (sort
		  (for [m (concat (.getFields c)
				  (.getMethods c)
				  (.getConstructors c))]
		    (let [static? (bit-and Modifier/STATIC
					   (.getModifiers m))
			  method? (instance? Method m)
			  ctor?   (instance? Constructor m)
			  text (if ctor?
				 (str "(" (apply str (interpose ", " (.getParameterTypes m))) ")")
				 (str
				  (if (pos? static?) "static ")
				  (.getName m) " : "
				  (if method?
				    (str (.getReturnType m) " ("
					 (count (.getParameterTypes m)) ")")
				    (str (.getType m)))))]
		      [(- static?) method? text (str m) m])))]
       (if i
	 (last (nth items i))
	 (do (println "=== " c " ===")
	     (doseq [[e i] (map list items (iterate inc 0))]
	       (printf "[%2d] %sn" i (nth e 2))))))))


;; From: http://clj-me.blogspot.com/2008/05/jumping-to-javadocs-from-repl.html
;; usage:
;; (javadoc Throwable) opens a window displaying Throwable's javadoc
;; hint: (javadoc (class some-object))

(defn open-url [url]
  (let [htmlpane (javax.swing.JEditorPane. url)]
    (.setEditable htmlpane false)
    (.addHyperlinkListener htmlpane
                           (proxy [javax.swing.event.HyperlinkListener] []
                             (hyperlinkUpdate [#^javax.swing.event.HyperlinkEvent e]
                                              (when (= (.getEventType e)
                                                       (. javax.swing.event.HyperlinkEvent$EventType ACTIVATED))
                                                (if (instance? javax.swing.text.html.HTMLFrameHyperlinkEvent e)
                                                  (.. htmlpane getDocument (processHTMLFrameHyperlinkEvent e))
                                                  (try
                                                   (.setPage htmlpane (.getURL e))
                                                   (catch Throwable t (.printStacktrace t))))))))
    (doto (new javax.swing.JFrame)
      (.setContentPane (new javax.swing.JScrollPane htmlpane))
      (.setBounds 32 32 700 900)
      (.show))))

(defn javadoc [c]
  (let [url (str "http://java.sun.com/javase/6/docs/api/"
		 (.. c getName (replace "." "/")
		     (replace "$" "."))
		 ".html")]
    (open-url url)))

