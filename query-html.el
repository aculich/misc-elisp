(require 'url)
(require 'w3m)
(require 'json)

(defvar local-tidy-command "tidy -utf8 -asxml --tidy-mark no --doctype omit --quote-nbsp no -f /dev/null")
(defvar local-tidy-default-namespace "\"x=http://www.w3.org/1999/xhtml\"")
(defvar local-xmlstarlet-command "xmlstarlet sel -N %s -t -m \"%s\" -v .")
(defvar local-wget-command "wget -q -O - \"%s\"")
(defvar yql-api-url "http://query.yahooapis.com/v1/public/yql?q=")
(defvar xpath-query-method-alist
  '((yql   . "//")
    (shell . "//x:")
    (elisp . "/descendant::")))

;; (require 'dom)
;; (require 'xpath) ;; xpath is broken
(defun query-html-elisp (url &optional xpath)
  (message
"Unfortunately the elisp implementation of xpath on the emacswiki
is terribly broken and the example query, at least on my machine,
very quickly gobbles up all available memory. I declare the
current version to be broken and suggest that you not use it. To
even get it to work I had to modify how it imported wisent. If
you really want to use the elisp xpath, then you should first
fix all the problems with that package."))

(defun query-html-shell (url &optional xpath)
  "Get the title of an Amazon movie found at URL.
Requires `wget', `tidy', and `xmlstarproc' to be available on the
system."
  (let* ((wget (format local-wget-command url))                           ;; output the web page to stdout
         (tidy local-tidy-command)                                        ;; fix all the broken html and output as an xml document
         (ns local-tidy-default-namespace)                                ;; required for the default namespace that tidy uses for xml output
         (xmlstarlet (format local-xmlstarlet-command ns xpath))          ;; match the xpath expression and print out the node that is found
         (command (mapconcat 'identity (list wget tidy xmlstarlet) "|"))) ;; build a unix command pipeline
    (with-temp-buffer
      (let* ((result (shell-command command (current-buffer)))            ;; run the command and insert contents into temp buffer
             (string (if (= result 0)
                         (buffer-string)                                  ;; return the contents of the buffer as a string
                       (error (format "Error code %s while running %s"    ;; unless there was an error
                                      result command)))))
        (if (string= "\n" (substring string -1))                          ;; get rid of the trailing newline that shell-command inserts
            (substring string 0 -1)
          string)))))

(defun query-html-yql-json (url)
  "Run the YQL query specified by URL and return the result as JSON."
  (with-temp-buffer
    (url-insert-file-contents (concat url "&format=json"))                ;; insert url contents into temp buffer
    (beginning-of-buffer)                                                 ;; make sure point is at the beginning
    (json-read)))                                                         ;; read in the JSON (note: add error handling)

(defun query-html-yql (url &optional xpath)
  "Gets the title of an Amazon movie found at URL.
Uses Yahoo's YQL web service."
  (let* ((yql "http://query.yahooapis.com/v1/public/yql?q=")              ;; Yahoo's YQL query interface
         (type "json")                                                    ;; we want JSON rather than xml results from YQL
         (query (w3m-url-encode-string                                    ;; browse-url-encode-url does a bad job of url encoding, so use w3m instead
                 (format
                  "select * from html where url=\"%s\" and (xpath=\"%s\")";; the YQL query that we want to run
                  url xpath)))
         (url (concat yql query))                                         ;; concat it all together into a url to pass to find-file
         (results                                                         ;; return just the list of results from the JSON query
          (cdr (assoc 'results (assoc 'query (query-html-yql-json url))))))
    (cdr (assoc 'content (cdar results)))))

(defun query-html (method url xpath)
  "Fetch URL and tidy it up into validated XML and then run the XPATH query.
METHOD should match a valid entry in `xpath-query-method-alist'.
NOTE: the xpath query is extremely limited in this implementation
and should really be re-written to be more general."
  (let* ((prefix (cdr (assoc method xpath-query-method-alist)))
         (func (intern (concat "query-html-" (symbol-name method)))))
    (funcall func (format "%s" url) (format xpath prefix))))              ;; run the xpath query using the specified method ('yql or 'shell)

(defun query-html-amazon-title (url &optional method)
  "Gets the title of an Amazon movie found at URL.
The default METHOD is 'yql since it does not require anything
extra to be installed on the system and will work across all
platforms."
  (replace-regexp-in-string                                               ;; normalize the title by
   "^[[:space:]]+\|[[:space:]]+$" ""                                      ;; getting rid of leading and trailing whitespace
   (replace-regexp-in-string                                              ;; after removing all newlines and
    "[[:space:]\n]+" " "                                                  ;; converting tabs or multiple spaces into a single space
    (let ((method (or method 'yql))                                       ;; use yql as the default method if none specified
          (xpath "%sspan[@id='btAsinTitle']"))
      (query-html method url xpath)))))

;;;;;;;;;;;;;;
;; ;; Examples
;;
;; (query-html-amazon-title "http://www.amazon.com/dp/B000055Y0X/?tag=xahh-20")
;; (query-html-amazon-title "http://www.amazon.com/dp/B000055Y0X/?tag=xahh-20" 'yql)
;; (query-html-amazon-title "http://www.amazon.com/dp/B000055Y0X/?tag=xahh-20" 'shell)
;; (query-html-amazon-title "http://www.amazon.com/dp/B000055Y0X/?tag=xahh-20" 'elisp)

(provide 'query-html)

;; JSON results returned from YQL
;;
;; ((query
;;  (results
;;   (span
;;    (content . \"Dr. Strangelove, Or: How I Learned\\n        to Stop Worrying and Love the Bomb (Special Edition)\\n        (1964)\")
;;    (id . \"btAsinTitle\")))
;;  (lang . \"en-US\")
;;  (created . \"2010-11-05T14:58:44Z\")
;;  (count . \"2\")))

;; NOTE: There is a tidy.el file on emacswiki:
;;
;; http://www.emacswiki.org/cgi-bin/wiki/download/tidy.el
;;
;; however, it is currently only useful for tidying up the content of
;; a buffer, not for use in a pipeline, but could be refactored to be
;; more useful
