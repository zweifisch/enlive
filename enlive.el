
(require 'cl)

(defalias 'enlive-parse-region 'libxml-parse-html-region)

(defun enlive-parse (input)
  (with-temp-buffer
    (insert input)
    (libxml-parse-html-region (point-min) (point-max))))

(defun enlive-fetch (url encoding)
  (with-current-buffer (url-retrieve-synchronously url)
    (goto-char (point-min))
    (search-forward-regexp "\n[\t\n ]*\n+")
    (decode-coding-region (point) (point-max) encoding)
    (libxml-parse-html-region (point) (point-max))))

(defun enlive-direct-children (element)
  (when (listp element)
    (cdr (cdr element))))

(defun enlive-text (element)
  (if (stringp element) element
    (let ((result ""))
      (dolist (child (enlive-direct-children element))
        (setq result (concat result (enlive-text child))))
      result)))

(defun enlive-attr (element attr)
  (cdr (assoc attr (cadr element))))

(defun enlive-has-class (element class)
  (let ((class-names (enlive-attr element 'class)))
    (when class-names 
      (member class (split-string class-names)))))

(defun enlive-some (element predict)
  (if (funcall predict element) element
    (let (result)
      (dolist (child (enlive-direct-children element))
        (when (and (listp child) (null result))
          (let ((r (enlive-get-element-by-id child id)))
            (when r (setq result r)))))
      result)))

(defun enlive-filter (element predict)
  (let ((results (when (funcall predict element) (list element)))
         (children (enlive-direct-children element)))
    (when children
      (dolist (child children)
        (when (listp child)
          (let ((elements (enlive-filter child predict)))
            (when elements
              (setq results (append results elements)))))))
    results))

(defun enlive-get-elements-by-class-name (element class)
  (enlive-filter element (lambda (node) (enlive-has-class node class))))

(defun enlive-get-elements-by-tag-name (element tag)
  (enlive-filter element (lambda (node) (eq tag (car node)))))

(defun enlive-get-element-by-id (element id)
  (enlive-some element (lambda (node) (string= id (enlive-attr node 'id)))))

(defun enlive-match-element (element criteria)
  (let ((tag (car criteria)))
    (catch 'failed
      (unless (or (string= tag ":") (string= tag "."))
        (setq criteria (cdr criteria))
        (when (not (string= (symbol-name (car element)) tag))
          (throw 'failed nil)))
      (dotimes (i (/ (length criteria) 2))
               (let ((type (nth (+ i i) criteria))
                     (val (nth (+ i i 1) criteria)))
                 (unless 
                     (cond ((string= ":" type) (string= val (enlive-attr element 'id )))
                           ((string= "." type) (enlive-has-class element val)))
                   (throw 'failed nil))))
      element)))

(defun enlive-find-element (element criteria)
  (enlive-filter element (lambda (node) (enlive-match-element node criteria))))

(defun enlive-parse-single-selector (selector prev)
  (cond ((eq selector '>) '(enlive-direct-children element))
        ((eq selector '*) '(identity element))
        (t `(,(if (eq prev '>) 'enlive-match-element 'enlive-find-element)
             element ',(butlast (cdr (split-string (symbol-name selector) "\\b"))))))) 

(defun enlive-parse-selector (selector)
  (let ((prev nil))
    (mapcar (lambda (x)
              (let ((parsed (enlive-parse-single-selector x prev)))
                (setq prev x)
                parsed))
            selector)))

(defun enlive-query (element selector)
  (let ((criteria (enlive-parse-selector selector)))
    (while (and element criteria)
      (let ((head (car criteria)))
        (setq criteria (cdr criteria))
        (setq element (eval head))))
    element))

(enlive-query 
 (enlive-parse "<div id=\"id1\" class=\"cls cls2\"><span class=\"cls2\"></span></div>") [.cls2])

(enlive-get-elements-by-class-name
 (enlive-parse "<div id=\"id1\" class=\"cls cls2\"></div>") "cls2")

(enlive-get-element-by-id
 (enlive-parse "<div id=\"id1\" class=\"cls cls2\"></div>") "id1")

(enlive-find-element
 (enlive-parse "<div id=\"id1\" class=\"cls cls2\"></div>") '(":" "id1"))
