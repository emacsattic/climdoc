;;; CLIMDOC: An emacs interface the the Harlequin CLIM-2.0 reference

;; Requires FSF emacs 20.3 or greater

;; Written by Larry Hunter, LHunter@nih.gov on July 27, 1999.  This is
;; a US Government work, and is uncopyrightable.

;; License: public-domain

;; To use this program, place this file in your load path, optionally
;; byte-compile it, and bind the function CLIMDOC to a convenient key (I use
;; \C-h\z myself).

;; I use CLIMDOC as a supplement to the hyperspec browser written by Erik
;; Naggum (http://sourcery.naggum.no/emacs/hyperspec.el).  

;; CLIMDOC is the user interface.  It will pop up a browser window on the CLIM
;; documentation for the argument (which can be read with completion from
;; the minibuffer, with default being the symbol near point).

(defun climdoc (entry)
  "View the CLIM documenation on ENTRY.
If there is more than one page of documentation on ENTRY, successive calls
to CLIMDOC will cycle through them."
  (interactive
   (let ((default (climdoc-symbol-near-point)))
     (unless climdoc-index-alist (climdoc-parse-index-pages))
     (list (completing-read "CLIM Documentation on: "
                            climdoc-index-alist nil t default default))))
  (let* ((pages (cdr (assoc entry climdoc-index-alist)))
         (last-same (member climdoc-last-page pages))
         (page (if (and last-same (cdr last-same)) ; if we looked at the
                   (cadr last-same)                ; last, just use the first.
                   (car pages))))
    (setq climdoc-last-page page)
    (browse-url (concat climdoc-root page))))

;; Pick your favorite key binding.  Here's mine (commented out).

; (define-key lisp-mode-map "\C-h\z" 'climdoc)

;; CLIMDOC-ROOT is the root directory of the CLIM documentation hierarchy.

(defcustom climdoc-root "http://www.harlequin.com/education/books/CLIM-2.0/"
  "The root directory of the CLIM Documentation hierarchy.
This is usually http://www.harlequin.com/education/books/CLIM-2.0/"
  :type 'directory)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; No user servicable parts inside.  Should only need changing if Harlequin
;; changes the CLIM web pages (ha!)

(defvar climdoc-index-alist nil
  "An alist associating index entries with html files that document them.")

(defvar climdoc-link-regexp "<a href=\"\\([^\"]*\\)\">\\([^<]*\\)</a>"
  "A regular expression used to parse hyperlinks  Has two subexpressions,
  the first corresponding to the link, and the second to the label used
  for the hyperlink.")

(defvar climdoc-last-page nil
  "The last page viewed by CLIMDOC.  Used to cycle through multiple entries.")

(defvar climdoc-index-top "GUID_353.HTM"
  "The filename (relative to climdoc root) where the index of index pages lives")

;; CLIMDOC-PARSE-INDEX-PAGES extracts all of the hyperlinks from the index
;; pages, and builds an alist from them.  Should be called only once,
;; generally at the first invocation of climdoc.  The top index page has
;; pointers to all the other index pages, so we follow all of these.

(defun climdoc-parse-index-pages ()
  "Parse the climdoc index pages to create climdoc index alist."
  (setq climdoc-index-alist nil)
  (let ((index-top-site (concat climdoc-root climdoc-index-top))
        (index-buffer (generate-new-buffer "climdoc"))
        (index-site-list nil))
    (get-http-page index-top-site index-buffer)
    (with-current-buffer index-buffer
      (message "Parsing CLIM documentation index files...")
      (save-match-data
        (goto-char (point-min))
        (let ((begin (re-search-forward (regexp-quote "<PRE>")))
              (end (re-search-forward (regexp-quote "</PRE>"))))
          (goto-char begin)
          (while (re-search-forward climdoc-link-regexp end t)
            (push (match-string 1) index-site-list))))
      (while index-site-list
        (climdoc-parse-index-page (pop index-site-list) index-buffer)))
      (message "done")))

;; Parse an individual page.  Note that one page (the : index) doesn't have
;; </PRE> to end the list (a typo on the page), so we need a fallback to
;; determine the end of the index links.

(defun climdoc-parse-index-page (page buffer)
  "Parse an individual climdoc index page"
  (with-current-buffer buffer
    (erase-buffer)
    (get-http-page (concat climdoc-root page) buffer)
    (save-match-data
      (goto-char (point-min))
      (let ((begin (re-search-forward (regexp-quote "<PRE>")))
            (end (or (re-search-forward (regexp-quote "</PRE>") nil t)
                     (re-search-forward (regexp-quote "<ADDRESS>")))))
        (goto-char begin)
        (while (re-search-forward climdoc-link-regexp end t)
          (let* ((key (match-string 2))
                 (url (match-string 1))
                 (previous (assoc key climdoc-index-alist)))
            (if previous
                (unless (member url (cdr previous))
                  (setcdr previous (cons url (cdr previous))))
                (setq climdoc-index-alist
                      (cons (list key url)
                            climdoc-index-alist)))))))))

;; Some utility functions:

;; CLIMDOC-SYMBOL-NEAR-POINT returns a string containing the symbol near
;; point.  Not everyone has THING-AT-POINT, so use this for now.

(defun climdoc-symbol-near-point ()
  "Returns a string containing the symbol near point."
  (save-excursion
    (let ((begin (progn (skip-syntax-backward "w_") (point)))
          (end (progn (skip-syntax-forward "w_") (point))))
      (buffer-substring begin end))))

;; Very basic http interface.  Use GET method to download an HTML page, which is written to a buffer.

(defun get-http-page (url &optional results-buffer)
  (unless results-buffer
    (setq results-buffer  (generate-new-buffer "http-get")))
  (let* ((host (url-host url))
         (port (or (url-port url) "80"))
         (file (or (url-file url) "/"))
         (request (format
                   (concat
                    "GET %s HTTP/1.0\r\n"     ; The request
                    "Host: %s\r\n"            ; Who we want to talk to
                    "\r\n")                   ; End request
                   file
                   host))
         (process (open-network-stream "http-get" results-buffer
                                       host (string-to-int port))))
    (unwind-protect
         (save-excursion
           (set-buffer results-buffer)
           (process-send-string process request)
           (message "Request sent, waiting for response")
           (while (memq (process-status process) '(run open))
             (accept-process-output process 1)))
      (condition-case ()
          (delete-process process)
        (error nil))))
  results-buffer)

;; This is most definitely not a robust URL parser, but it works for this
;; application.

(defun url-host (urlstring)
  (save-match-data
   (string-match "\/\/\\\([^\:\/]+\\\)[\/:]" urlstring)
   (match-string 1 urlstring)))

(defun url-port (urlstring)
  (save-match-data
    (string-match "\/\/[^\:\/]+:\\\([0-9]+\\\)\/" urlstring)
    (match-string 1 urlstring)))

(defun url-file (urlstring)
  (save-match-data
    (string-match "\/\/[^\/]+\\\(\/.*$\\\)" urlstring)
    (match-string 1 urlstring)))

(provide 'climdoc)
