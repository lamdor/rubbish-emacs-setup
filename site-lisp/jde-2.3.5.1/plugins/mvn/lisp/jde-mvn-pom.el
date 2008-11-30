;;;; jde-mvn-pom.el --- Tools for using attributes from a Maven POM in
;;;; JDE project files
;;
;; Copyright (c) 2007-2008 Espen Wiborg <espenhw@grumblesmurf.org>
;;
;; Permission to use, copy, modify, and distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.
;;
;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
;; WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
;; AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
;; DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
;; PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
;; TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
;; PERFORMANCE OF THIS SOFTWARE.
;;

(require 'cl)
(require 'xml)
(require 'nxml-mode)

;;; Internal variables

(defvar *jde-mvn-pom-cache*
  (make-hash-table :test 'equal)
  "The cache for parsed POM files.  Don't touch.")

(defvar *pom-node* nil
  "The default POM node operated on by most of the entrypoints.")

(defvar *jde-mvn-output-buffer* (get-buffer-create " *POM parse output*")
  "The buffer that output from mvn is written to.")

(defvar *jde-mvn-default-plugin-group-id* 'org.apache.maven.plugins)

(defvar *jde-mvn-current-pom* "")

(defconst +jde-mvn-scope-precedence+ '(:test :runtime :compile :provided :system))

(defconst +jde-mvn-allowed-scopes+ (list* "import"
                                          (mapcar #'(lambda (s)
                                                      (substring (symbol-name s) 1))
                                                  +jde-mvn-scope-precedence+)))

;;; Utility functions

(defun make-keyword (string)
  "Make a keyword out of STRING."
  (intern (concat ":" string)))

(defun file-last-modified-time (file)
  "The last-modified time of `file', as a 32-bit integer."
  (let ((lastmod (nth 5 (file-attributes file))))
    (logior (lsh (nth 0 lastmod) 16)
            (nth 1 lastmod))))

(defun xml-parse-file* (pom-file)
  "Parses the file and returns the root node."
  (car (xml-parse-file pom-file)))

(defun xml-first-child (node element)
  "Returns the first `element' child of `node'."
  (car (xml-get-children node element)))

(defun xml-element-content (node)
  "Returns the (string) content of `node', recursively (much like the string() XPath function), or `nil' if there is no content."
  (if (null node)
      nil
    (mapconcat (lambda (e)
                 (if (stringp e) e ""))
               (xml-node-children node)
               "")))

;;; Convenience functions

(defun jde-mvn-maven-project-file-p (filename)
  (let ((default-directory (file-name-directory filename)))
    (jde-mvn-find-pom-file jde-mvn-pom-file-name t)))

(defun jde-mvn-maybe-reload-pom-file ()
  (condition-case err
      (let ((pom-file-path (jde-mvn-find-pom-file jde-mvn-pom-file-name t)))
        (if (and
             pom-file-path
             jde-project-context-switching-enabled-p
             (not (jde-debugger-running-p))
             (not (string=
                   (file-truename *jde-mvn-current-pom*)
                   (file-truename pom-file-path))))
            (progn
              (setq *jde-mvn-current-pom* pom-file-path)
              (jde-load-project-file)
              (jde-wiz-set-bsh-project))))
    (error (message 
	    "Project file reload error: %s" 
	    (error-message-string err)))))    

(defun jde-mvn-clear-pom-cache ()
  "Clears the pom cache.  You may have to do this if you use parent
POMs, as the caching mechanism only takes the timestamp of the child
pom into account."
  (interactive)
  (clrhash *jde-mvn-pom-cache*))

(defun jde-mvn-get-pom-from-cache (pom-file)
  (let ((pom-file-last-mod-time (file-last-modified-time pom-file))
        (cached-pom-data (gethash pom-file *jde-mvn-pom-cache*)))
    (if (and cached-pom-data
             (<= pom-file-last-mod-time (car cached-pom-data)))
        (cdr cached-pom-data)
      nil)))

;;; Getting stuff from the POM

(defun* jde-mvn-get-pom-property (property &optional default-value
                                           (pom-node *pom-node*))
  "Resolves the Maven project property (e.g. `project.name').
Returns DEFAULT-VALUE if the property is not defined."
  (if (symbolp property)
      (setq property (symbol-name property)))
  (let ((elements (mapcar 'intern (split-string property "\\.")))
        (node pom-node))
    (cond ((eq (car elements) 'project)
           (setq elements (cdr elements))
           (while elements
             (setq node (xml-first-child node (pop elements)))))
          (t
           (setq node (xml-first-child node 'properties))
           (when node
             (setq node (xml-first-child node (intern property))))))
    (if node
        (xml-element-content node)
      default-value)))

(defun* jde-mvn-get-pom-plugin (pom-node
                                plugin-artifact-id
                                &optional (plugin-group-id *jde-mvn-default-plugin-group-id*)
                                (section 'build))
  "Returns the <plugin> element for the GROUP-ID:ARTIFACT-ID plugin in
SECTION."
  (let ((plugins (xml-get-children
                  (xml-first-child (xml-first-child pom-node section)
                                   'plugins)
                  'plugin)))
    (find (cons plugin-group-id plugin-artifact-id)
          plugins
          :test (function equal)
          :key (lambda (node)
                 (let ((group-id (xml-element-content
                                  (xml-first-child node 'groupId)))
                       (artifact-id (xml-element-content
                                     (xml-first-child node 'artifactId))))
                   (cons (or (and group-id (intern group-id))
                             *jde-mvn-default-plugin-group-id*)
                         (intern artifact-id)))))))

(defun* jde-mvn-get-pom-plugin-configuration-property
    (pom-node plugin-artifact-id property
              &optional (plugin-group-id 'org.apache.maven.plugins)
              (section 'build))
  "Returns the configured value of PROPERTY (a symbol) for the
GROUP-ID:PLUGIN-ARTIFACT-ID plugin in SECTION, that is the value of
the <PROPERTY> element in the <configuration> element in that plugin."
  (let* ((plugin-configuration
          (xml-first-child (jde-mvn-get-pom-plugin pom-node plugin-artifact-id
                                                   plugin-group-id section)
                           'configuration))
         (node (xml-first-child plugin-configuration
                                property)))
    (when node 
      (xml-element-content node))))

;;; More convenience functions

(defun* jde-mvn-compiler-source (&optional (pom-node *pom-node*))
  "Returns the configured `source' option for javac."
  (or (jde-mvn-get-pom-plugin-configuration-property pom-node
                                                     'maven-compiler-plugin
                                                     'source)
      jde-mvn-default-compiler-source))

(defun* jde-mvn-compiler-target (&optional (pom-node *pom-node*))
  "Returns the configured `target' option for javac."
  (or (jde-mvn-get-pom-plugin-configuration-property pom-node
                                                     'maven-compiler-plugin
                                                     'target)
      jde-mvn-default-compiler-target))

(defun* jde-mvn-compiler-encoding (&optional (pom-node *pom-node*))
  "Returns the configured `encoding' option for javac."
  (jde-mvn-get-pom-plugin-configuration-property pom-node
                                                 'maven-compiler-plugin
                                                 'encoding))

(defun* jde-mvn-get-compile-classpath (&optional (pom-node *pom-node*))
  (jde-mvn-get-classpath pom-node 'jde-mvn:compile-classpath))

(defun* jde-mvn-get-runtime-classpath (&optional (pom-node *pom-node*))
  (jde-mvn-get-classpath pom-node 'jde-mvn:runtime-classpath))

(defun* jde-mvn-get-test-classpath (&optional (pom-node *pom-node*))
  (jde-mvn-get-classpath pom-node 'jde-mvn:test-classpath))

(defun jde-mvn-get-classpath (pom-node classpath-type)
  (xml-get-attribute-or-nil pom-node classpath-type))

(defun jde-mvn-make-sourcepath (pom-node include-dependency-sources)
  "Create a list of source locations from the POM.  If
INCLUDE-DEPENDENCY-SOURCES is non-NIL, the list will include any
source artifacts for the dependencies of the POM."
  (nconc (list (jde-mvn-get-pom-property "project.build.sourceDirectory"
                                         nil
                                         pom-node))
         (when include-dependency-sources
           (delete-if-not #'file-exists-p
                          (loop for artifact in (jde-mvn-get-compile-classpath pom-node)
                                collect (jde-mvn-make-source-artifact artifact))))))

(defun jde-mvn-make-source-artifact (artifact)
  (replace-in-string artifact "\\.jar$" "-sources.jar"))

;;; Calling Maven

(defun jde-mvn-pom-call-maven (pom-file goals &rest properties)
  "Call Maven asynchronously on POM-FILE, executing GOALS (a
string designator or a list of string designators) with
PROPERTIES defined on the command line.  PROPERTIES must be an
even-length list.  Returns the Maven process.

Examples:
 (jde-mvn-pom-call-maven pom-file 'help:describe :plugin \"help\")

 (jde-mvn-pom-call-maven pom-file '(install \"site\") :maven.test.skip t)"
  (unless (consp goals) (setq goals (list goals)))
  (let* ((extra-args
          (append
           (mapcar #'(lambda (e)
                       (cond ((symbolp e) (symbol-name e))
                             (t e)))
                   goals)
           (jde-mvn-make-maven-arguments properties)))
         (command-args (list* jde-mvn-command  "-B" "-N" "-f"
                              pom-file extra-args))
         (command-string (mapconcat #'identity command-args " ")))
    (with-current-buffer *jde-mvn-output-buffer*
      (erase-buffer)
      (insert command-string "\n"))
    (let ((default-directory (file-name-directory pom-file)))
      (message "Calling %s..." command-string)
      (apply #'start-process "Maven"
             *jde-mvn-output-buffer*
             command-args))))

(defun* jde-mvn-prompt-for-pom-file (&optional (prompt "POM file (default %s): "))
  "Prompt for a file, defaulting to the return of `jde-mvn-find-pom-file'."
  (let ((default-file (jde-mvn-find-pom-file jde-mvn-pom-file-name t)))
    (expand-file-name
     (minibuffer-with-setup-hook
         (lambda () (setq minibuffer-default default-file))
       (read-file-name (format "POM file (default %s): " default-file) nil default-file)))))

(defun* jde-mvn-resolve-source-artifacts (&optional (pom-file (jde-mvn-find-pom-file jde-mvn-pom-file-name t)))
  "Ask Maven to download all the source artifacts it can find for
the dependencies of the POM."
  (interactive (list (jde-mvn-prompt-for-pom-file)))
  (jde-mvn-pom-call-maven pom-file "dependency:sources"))

;;; The meat of the matter

(defun* jde-mvn-set-jde-variables (&key (pom-node *pom-node*) include-dependency-sources)
  "Sets the JDE variables `jde-project-name', `jde-global-classpath',
`jde-compile-option-directory', `jde-compile-option-source',
`jde-compile-option-target', `jde-compile-option-encoding',
`jde-sourcepath' and `jde-built-class-path' to sensible values
based on the given POM.  If INCLUDE-DEPENDENCY-SOURCES is T,
`jde-sourcepath' will include the source artifacts for the
project's dependencies if the source exists in your local
repository."
  (let ((target-directory (jde-mvn-get-pom-property 'project.build.outputDirectory
                                                    nil pom-node)))
    (when (jde-mvn-compiler-encoding pom-node)
      (jde-set-variables 
       '(jde-compile-option-encoding (jde-mvn-compiler-encoding pom-node))))
    (jde-set-variables
     '(jde-project-name (jde-mvn-get-pom-property 'project.name "Unnamed project"
                                                  pom-node))
     '(jde-compile-option-source (list (jde-mvn-compiler-source pom-node)))
     '(jde-compile-option-target (list (jde-mvn-compiler-target pom-node)))
     '(jde-sourcepath (jde-mvn-make-sourcepath pom-node include-dependency-sources)))
    (jde-mvn-setup-classpath-and-output)))

(defmacro* with-pom ((&optional (pom-file '(jde-mvn-find-pom-file jde-mvn-pom-file-name t))) &body body)
  "Asynchronously execute BODY with `*pom-node*' bound to the
parsed POM-FILE."
  (let ((pom-param (gensym)))
    `(jde-mvn-pom-call-with-pom
      (lambda (,pom-param)
        (let ((*pom-node* ,pom-param))
          ,@body))
      ,pom-file)))

(put 'with-pom 'lisp-indent-function 1)

;;; Helper for jde-mvn-pom-call-with-pom
(defun jde-mvn-pom-parse-pom-and-call (buffer closure pom-file)
  (condition-case error
      (let ((pom (jde-mvn-pom-parse-pom-from-buffer buffer)))
        (if (null pom)
            (progn
              (display-buffer buffer t)
              ;; This won't actually do anything other than
              ;; print to *Messages*, but allows me to debug if
              ;; I need to
              (error "Failed to parse effective POM; the contents of the buffer %s might help diagnose" (buffer-name buffer)))
          (let ((classpaths
                 (jde-mvn-pom-parse-dependency-tree-from-buffer
                  buffer
                  (jde-mvn-pom-parse-dependency-list-from-buffer buffer))))
            ;; Push the classpaths onto the POM tree
            (mapc #'(lambda (cpspec)
                      (push cpspec (nth 1 pom)))
                  classpaths))
          (puthash pom-file
                   (cons (file-last-modified-time pom-file)
                         pom)
                   *jde-mvn-pom-cache*)
          (funcall closure pom)
          (message "POM parsing done.")))
    (error (display-buffer buffer t)
           (puthash pom-file (cons (file-last-modified-time pom-file)
                                   (list 'invalid-pom (cdr error)))
                    *jde-mvn-pom-cache*)
           (jde-mvn-pom-clear-parsing pom-file)
           (funcall #'signal (car error) (cdr error)))))

(defvar *jde-mvn-poms-in-parsing* nil)

(defun jde-mvn-pom-flag-parsing (pom-file)
  (push pom-file *jde-mvn-poms-in-parsing*))

(defun jde-mvn-pom-clear-parsing (pom-file)
  (setq *jde-mvn-poms-in-parsing*
        (delete pom-file
                *jde-mvn-poms-in-parsing*)))

(defun* jde-mvn-pom-call-with-pom (closure &optional (pom-file (jde-mvn-find-pom-file)))
  "Calls CLOSURE with one argument: The parsed POM from POM-FILE,
augmented with classpath information.  Three classpaths are
provided as attributes on the root node: jde-mvn:compile-classpath,
jde-mvn:runtime-classpath and jde-mvn:test-classpath.

If POM-FILE exists in the cache, CLOSURE is called immediately;
otherwise, a Maven process is started asynchronously, and CLOSURE
will be called when that process exits."
  (let ((cached-pom (jde-mvn-get-pom-from-cache pom-file)))
    (cond (cached-pom
           (funcall closure cached-pom))
          ((member pom-file *jde-mvn-poms-in-parsing*)
           (message "Ignoring duplicate parse request for %s." pom-file))
          (t
           (jde-mvn-pom-flag-parsing pom-file)
           (lexical-let ((closure closure)
                         (pom-file pom-file))
             (let ((goals '(help:effective-pom dependency:tree dependency:list))
                   (properties '(:outputAbsoluteArtifactFilename t)))
               (if jde-mvn-use-server
                   (progn
                     (apply #'jde-mvn-call-mvn-server jde-mvn-pom-visible
                            pom-file goals
                            #'(lambda (buf msg)
                                (unwind-protect 
                                    (jde-mvn-pom-parse-pom-and-call buf
                                                                    closure
                                                                    pom-file)
                                  (jde-mvn-pom-clear-parsing pom-file)))
                            properties)
                     (jde-mvn-wait-for-server))
                 ;; not server-mode
                 (message "Parsing POM in the background...")
                 (let ((process (apply 'jde-mvn-pom-call-maven
                                       pom-file goals properties)))
                   (set-process-sentinel
                    process
                    (lambda (process event)
                      (when (memq (process-status process) '(signal exit))
                        ;; OK, process is dead
                        (if (or (eq (process-status process) 'signal)
                                (/= (process-exit-status process) 0))
                            (progn
                              (when (process-buffer process)
                                (with-current-buffer (process-buffer process)
                                  (goto-char (point-max)))
                                (display-buffer (process-buffer process) t))
                              (message "%s exited abnormally" jde-mvn-command))
                          ;; Normal exit
                          (unwind-protect
                              (jde-mvn-pom-parse-pom-and-call
                               *jde-mvn-output-buffer*
                               closure
                               pom-file)
                            (jde-mvn-pom-clear-parsing pom-file))))))))))))))

(defun jde-mvn-pom-parse-dependency-tree-from-buffer (buffer artifactmap)
  "Parses the output of mvn dependency:tree."
  (with-current-buffer buffer
    (goto-char (point-min))
    (search-forward "[dependency:tree]")
    ;; First line is this artifact, skip it
    (forward-line 2)
    (let ((dependencies (loop while (looking-at "\\[INFO\\] +[+|\\\\]")
                              collect (jde-mvn-pom-artifact-coordinates)
                              do (forward-line 1))))
      (loop for dependency in dependencies
            do (setf (cdr dependency)
                     (cdr (assoc-string (cdr dependency) artifactmap))))
      (flet ((find-dependencies (scope)
                                (let ((scopes (memq scope
                                                    +jde-mvn-scope-precedence+)))
                                  (mapcar #'cdr
                                          (remove-if-not #'(lambda (s)
                                                             (memq s scopes))
                                                         dependencies
                                                         :key #'car)))))
        (list (cons 'jde-mvn:compile-classpath
                    (find-dependencies :compile))
              (cons 'jde-mvn:runtime-classpath
                    (find-dependencies :runtime))
              (cons 'jde-mvn:test-classpath
                    (find-dependencies :test)))))))

(defun jde-mvn-pom-split-line (separator)
  (let ((point (point))
        (eol (progn (end-of-line) (point))))
    (goto-char point)
    (split-string 
     (buffer-substring-no-properties
      point
      (progn (skip-chars-forward "^ " eol) (point)))
     separator)))

(defun jde-mvn-pom-artifact-coordinates ()
  (forward-line 0)
  ;; skip "[INFO]"
  (forward-char 6)
  ;; skip any tree-drawing characters
  (skip-chars-forward "^[:alnum:]")
  (let ((components (jde-mvn-pom-split-line ":")))
    (cons (make-keyword (car (last components)))
          (mapconcat #'identity components ":"))))

(defun jde-mvn-pom-artifact-coordinates-and-location ()
  (forward-line 0)
  ;; skip "[INFO]"
  (forward-char 6)
  (skip-chars-forward "^[:alnum:]")
  (let ((components (jde-mvn-pom-split-line ":")))
    (cons (mapconcat #'identity (butlast components) ":")
          (car (last components)))))

(defun jde-mvn-pom-parse-dependency-list-from-buffer (buffer)
  (with-current-buffer buffer
    (goto-char (point-min))
    (search-forward "[dependency:list]")
    (forward-line 1)
    (search-forward "[INFO]  ")
    (forward-line 0)
    (loop while (looking-at "\\[INFO\\]  +")
          collect (jde-mvn-pom-artifact-coordinates-and-location)
          do (forward-line 1))))


(defun jde-mvn-pom-parse-pom-from-buffer (buffer)
  (with-current-buffer buffer
    (goto-char (point-max))
    (search-backward "</project>")
    (skip-chars-forward "^>")
    (forward-char 1)
    (let ((end (point)))
      (car (xml-parse-region (or (search-backward "<?xml " nil t)
                                 (re-search-backward "<project[[:space:]]"))
                             end)))))

(defun jde-mvn-pom-find-all-directories (dir)
  (let ((dirs (remove-if-not #'file-directory-p
                             (mapcar #'(lambda (d)
                                         (expand-file-name d dir))
                                     (remove-if #'(lambda (d)
                                                    (char-equal ?.
                                                                (elt d 0)))
                                                (directory-files dir))))))
    (if dirs
        (mapcan #'jde-mvn-pom-find-all-directories dirs)
      (list dir))))

(defvar jde-mvn-pom-cached-artifact-info nil)

(defun jde-mvn-pom-clear-artifact-cache ()
  (interactive)
  (setq jde-mvn-pom-cached-artifact-info nil))

(defun jde-mvn-pom-find-all-artifacts ()
  (if jde-mvn-pom-cached-artifact-info
      jde-mvn-pom-cached-artifact-info
    (message "%s" "Gathering artifact info from local repository...")
    (setq jde-mvn-pom-cached-artifact-info
          (let ((dirs (jde-mvn-pom-find-all-directories jde-mvn-local-repository))
                (artifacts nil))
            (dolist (dir dirs)
              (let* ((relative (file-relative-name dir jde-mvn-local-repository))
                     (parts (split-string relative "/"))
                     (version (car (last parts)))
                     (artifact-id (car (last parts 2)))
                     (group-id (mapconcat #'identity (butlast parts 2) ".")))
                (let* ((group-info (or (assoc group-id artifacts)
                                       (let ((elt (cons group-id nil)))
                                         (push elt artifacts)
                                         elt)))
                       (artifact-info (or (assoc artifact-id (cdr group-info))
                                          (let ((elt (cons artifact-id nil)))
                                            (push elt (cdr group-info))
                                            elt))))
                  (push version (cdr artifact-info)))))
            (dolist (group-info artifacts)
              (dolist (artifact-info (cdr group-info))
                (setf (cdr artifact-info) (nreverse (cdr artifact-info))))
              (setf (cdr group-info) (nreverse (cdr group-info))))
            (nreverse artifacts)))))

(defvar jde-mvn-pom-group-id-history nil)

(defun jde-mvn-pom-prompt-for-group-id ()
  (completing-read "Group ID: " (mapcar #'car
                                        (jde-mvn-pom-find-all-artifacts))
                   nil nil nil 'jde-mvn-pom-group-id-history))

(defvar jde-mvn-pom-artifact-id-history nil)

(defun jde-mvn-pom-prompt-for-artifact-id (group-id)
  (completing-read "Artifact ID: " (mapcar #'car 
                                           (cdr (assoc group-id
                                                       (jde-mvn-pom-find-all-artifacts))))
                   nil nil nil 'jde-mvn-pom-artifact-id-history))

(defvar jde-mvn-pom-version-history nil)

(defun jde-mvn-pom-prompt-for-version (group-id artifact-id)
  (completing-read "Artifact version: "
                   (cdr (assoc artifact-id 
                               (cdr (assoc group-id
                                           (jde-mvn-pom-find-all-artifacts)))))
                   nil nil nil 'jde-mvn-pom-version-history))

(defun jde-mvn-pom-prompt-for-scope (&optional include-import)
  (intern (completing-read
           "Scope: "
           +jde-mvn-allowed-scopes+)))

(defun* jde-mvn-pom-add-dependency (groupId artifactId version
                                            &optional (scope 'compile) (type 'jar)
                                            classifier
                                            (pom-file (jde-mvn-find-pom-file)))
  "Adds a dependency on groupId:artifactId:version to POM-FILE,
optionally specifying scope, type and classifier.  If called
interactively, will prompt (with completion!) for groupId,
artifactId and version.  If called interactively with a prefix
argument it will also prompt for scope.  If VERSION is NIL or the
empty string, no version will be specified in the POM (useful
when using dependencyManagement)."
  (interactive
   (let* ((group-id (jde-mvn-pom-prompt-for-group-id))
          (artifact-id (jde-mvn-pom-prompt-for-artifact-id group-id))
          (version (jde-mvn-pom-prompt-for-version group-id artifact-id)))
     (append (list group-id artifact-id version)
             (when current-prefix-arg
               (list (jde-mvn-pom-prompt-for-scope))))))
  (save-excursion
    (save-restriction
      (pop-to-buffer (find-file pom-file))
      (widen)
      (unless nxml-prolog-end
        (nxml-clear-dependent-regions (point-min) (point-max))
        (setq nxml-scan-end (copy-marker (point-min) nil))
        (nxml-with-unmodifying-text-property-changes
          (nxml-clear-inside (point-min) (point-max))
          (nxml-with-invisible-motion
            (nxml-scan-prolog))))
      (goto-char (point-min))
      (search-forward "<project")
      (nxml-down-element)
      (nxml-forward-element)
      (nxml-backward-element)
      ;; point is now at start of an element
      (ignore-errors
        (while (not (looking-at "<dependencies"))
          (nxml-forward-element 2)
          (nxml-backward-element)))
      ;; point is now either at start of dependencies element or just
      ;; after the close-tag of the last child of <project>
      (let ((start-region (point)))
        (if (looking-at "<dependencies")
            (nxml-forward-element)
          (insert "\n<dependencies>\n</dependencies>"))
        (nxml-down-element -1)
        (insert "<dependency>\n"
                (format "<groupId>%s</groupId>\n" groupId)
                (format "<artifactId>%s</artifactId>\n" artifactId))
        (when (and version (not (string= version "")))
          (insert (format "<version>%s</version>\n" version)))
        (unless (eq scope 'compile)
          (insert (format "<scope>%s</scope>\n" (symbol-name scope))))
        (unless (eq type 'jar)
          (insert (format "<type>%s</type>\n" type)))
        (when classifier
          (insert (format "<classifier>%s</classifier>\n" classifier)))
        (insert "</dependency>\n")
        (nxml-up-element)
        (indent-region start-region (point))))))

(defun jde-mvn-test-source-p (filename)
  "Returns non-NIL if FILENAME is a test source file (that is, if
it is somewhere below the Maven test sources directory."
  (let ((default-directory (file-name-directory filename)))
    (with-pom ()
      (string-match-p (concat "^"
                              (regexp-quote
                               (jde-mvn-get-pom-property
                                'project.build.testSourceDirectory "")))
                      filename))))

(defun jde-mvn-setup-classpath-and-output ()
  "Sets `jde-global-classpath', `jde-compile-option-classpath',
`jde-compile-option-directory' and `jde-built-class-path'
correctly depending on the result of `jde-mvn-test-source-p'."
  (when (jde-mvn-maven-project-file-p (buffer-file-name))
    (let ((test-source-p (jde-mvn-test-source-p (buffer-file-name))))
      (with-pom ()
        (let* ((classes-directory (jde-mvn-get-pom-property 'project.build.outputDirectory))
               (test-classes-directory (jde-mvn-get-pom-property
                                        'project.build.testOutputDirectory))
               (test-classpath (list* classes-directory (jde-mvn-get-test-classpath)))
               (target-directory (if test-source-p test-classes-directory classes-directory)))
          (jde-set-variables
           '(jde-global-classpath (list* target-directory
                                         (if test-source-p
                                             test-classpath
                                           (jde-mvn-get-runtime-classpath))))
           '(jde-compile-option-classpath (list* target-directory
                                                 (if test-source-p
                                                     test-classpath
                                                   (jde-mvn-get-compile-classpath))))
           '(jde-compile-option-directory target-directory)
           '(jde-built-class-path (if test-source-p
                                      (list test-classes-directory classes-directory)
                                    (list classes-directory)))))))))

;;; Insinuate jde-mvn-maybe-reload-pom-file and jde-mvn-setup-classpath-and-output
(add-hook 'jde-entering-java-buffer-hook 'jde-mvn-maybe-reload-pom-file)
(add-hook 'jde-entering-java-buffer-hook 'jde-mvn-setup-classpath-and-output)

(provide 'jde-mvn-pom)
