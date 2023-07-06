;; -*- lexical-binding: t -*-

(defvar form-cycle-current-cycle-state nil)
(defvar form-cycle-position nil)
(defvar form-cycle-initial-position nil)
(defvar form-cycle-new-cycle-p nil)
(defvar form-cycle-undo-previous-function nil)
(defvar form-cycle-function 'form-cycle-default-cycle-function)

(defvar form-cycle-debug nil)

(defmacro form-cycle-debug (thing &optional tag)
  (if form-cycle-debug
      (if tag
          `(princ (format "%s: %s: %s\n" ',tag ',thing ,thing))
        `(princ (format "%s: %s\n" ',thing ,thing)))
    '()))
 
;; Basic functionality: Inserting strings in a cycle.

(defun form-cycle-make-undo-function (length)
  (lambda ()
    (when form-cycle-initial-position
      (goto-char form-cycle-initial-position))
    (delete-forward-char length)
    (setf form-cycle-undo-previous-function nil)))

(defun form-cycle-default-cycle-function (form)
  (unless form-cycle-undo-previous-function
    (setf form-cycle-undo-previous-function
          (lambda ()
            (when form-cycle-initial-position
              (goto-char form-cycle-initial-position))
            (form-cycle-debug form form-cycle-undo-previous-function)
            (form-cycle-debug (length form) form-cycle-undo-previous-function)
            (delete-forward-char (length form))
            (setf form-cycle-undo-previous-function nil))))
  (insert form)
  form)
                            
(defun form-cycle-next ()
  (form-cycle-debug form-cycle-current-cycle-state)
  (let ((next (pop form-cycle-current-cycle-state)))
    (when next
      (setf form-cycle-current-cycle-state
            (append form-cycle-current-cycle-state (list next))))
    (when form-cycle-undo-previous-function
      (funcall form-cycle-undo-previous-function))
    (funcall form-cycle-function next) 
    (setf form-cycle-position (point))))

(defun form-cycle-new-cycle-p ()
  (or (null form-cycle-position)
      (and form-cycle-position
           (not (= form-cycle-position (point)))))) ; Point has moved

(defun form-cycle-initiate (cycle)
  (if (form-cycle-new-cycle-p)
      (let ((form-cycle-new-cycle-p t)) 
        (form-cycle-debug "New Cycle.")
        (setq form-cycle-current-cycle-state cycle
              form-cycle-undo-previous-function nil
              form-cycle-initial-position (point))
        (form-cycle-next))
    (form-cycle-next)))

(defun form-cycle-test ()
  (interactive)
  (form-cycle-initiate '("abc" "foo" "bar")))

;; Adding names and place point.

(defvar form-cycle-current-name nil)
(defvar form-cycle-name-marker "_")
(defvar form-cycle-point-marker "@")
(defvar form-cycle-up-list-initially-p nil)
(defvar form-cycle-up-list-initially-sexp-string nil)
(defvar form-cycle-raise-list-initially-p nil)

(defun form-cycle-symbol-at-point ()
  (let ((sym (symbol-at-point)))
    (when (and sym
               (string-match "^\\_<" (symbol-name sym)))
      sym)))

(defun form-cycle-beginning-of-symbol-maybe ()
  (when (form-cycle-symbol-at-point)
    (beginning-of-sexp)))

;; Point should be at the beginning of the form that was previously
;; inserted.
(defun form-cycle-with-name (form)
  ;; Initialize
  (when form-cycle-new-cycle-p
    (when form-cycle-up-list-initially-p
      (save-excursion
        (up-list -1)
        (setf form-cycle-up-list-initially-sexp-string
              (buffer-substring-no-properties
               (point) 
               (progn (forward-sexp) (point)))))) 
    (let ((sym (symbol-at-point)))
      (if (and sym
               (string-match "^\\_<" (symbol-name sym)))
          (progn
            (unless (looking-at "\\_<")
              (beginning-of-sexp))
            (kill-sexp)
            (setf form-cycle-current-name (symbol-name sym)
                  form-cycle-initial-position (point)))
        (setf form-cycle-current-name "")))
    (when form-cycle-up-list-initially-p
      (up-list -1)             
      (kill-sexp)      
      (setf form-cycle-initial-position (point)))
    (when form-cycle-raise-list-initially-p
      (form-cycle-beginning-of-symbol-maybe)
      (raise-sexp)
      (setf form-cycle-initial-position (point))))
  
  (let ((place-point)
        (string)
        (options))

    (when (consp form)
      (setf options (rest form)
            form (car form)))

    (when (or (functionp form)
              (symbolp form))
      (setf form (funcall form)))

    (when (getf options 'map-form)
      (save-excursion
        (setf form (funcall (getf options 'map-form) form))))

    ;; Build string
    (with-temp-buffer 
      (insert form)
      (beginning-of-buffer)
      ;; _ -> name
      (replace-string form-cycle-name-marker form-cycle-current-name)
      (beginning-of-buffer)
      ;; Figure out where to place point
      (when (search-forward form-cycle-point-marker nil t)
        (delete-backward-char (length form-cycle-point-marker))
        (setf place-point (1- (point))))
      (setf string (buffer-substring-no-properties 1 (buffer-end 1))))
    (form-cycle-debug string form-cycle-with-name)
    (when (getf options 'map-string)
      (save-excursion
        (setf string (funcall (getf options 'map-string) string))))
    (form-cycle-default-cycle-function string)
    (when place-point
      (goto-char form-cycle-initial-position)
      (forward-char place-point)
      (when (getf options 'after-place-point)
        (save-excursion
          (funcall (getf options 'after-place-point)))))
    (when (getf options 'place-point)
      (let ((p (getf options 'place-point)))
        (typecase p
          (integer (goto-char (+ form-cycle-initial-position p)))
          (function (funcall p)))))
    (when (getf options 'after-cycle)
      (let ((end (save-excursion
                   (let ((len (length string)))
                     (goto-char (+ form-cycle-initial-position len))
                     (push-mark)
                     (point)))))
        (save-excursion 
          (funcall (getf options 'after-cycle)))
        (unless (= (mark) end)
          (form-cycle-debug (mark))
          (form-cycle-debug end)
          (setf form-cycle-undo-previous-function
                (form-cycle-make-undo-function 
                 (- (mark) form-cycle-initial-position))))
        (pop-mark)))
    string))

(defun form-cycle-test-with-name ()
  (interactive)
  (let ((form-cycle-function 'form-cycle-with-name))
    (form-cycle-initiate '("(defun _ () @)"
                          "(defclass _ () (@))"))))

;; Context aware lisp forms

(defun form-cycle-%%%-to-subseq-of-current-name (form length)
  (if (< (length form-cycle-current-name) length)
      form
    (let ((prefix (subseq form-cycle-current-name 0 length)))
      (replace-regexp-in-string "%%%" prefix form))))

(defun form-cycle-%%%-to-first-char-of-current-name (form)
  (form-cycle-%%%-to-subseq-of-current-name form 1)) 

(defun form-cycle-%%%-to-first-two-chars-of-current-name (form)
  (form-cycle-%%%-to-subseq-of-current-name form 2))

(defun form-cycle-indent-defun ()
  (beginning-of-defun)
  (indent-pp-sexp))

(defun form-cycle-toplevel-form-nth (n)
  (condition-case nil
    (save-excursion 
      (beginning-of-defun)
      (down-list)
      (forward-sexp n)
      (symbol-name (symbol-at-point)))
    (error nil)))

(defun form-cycle-toplevel-form-name ()
  (condition-case nil
    (save-excursion 
      (beginning-of-defun)
      (down-list)
      (forward-sexp 2)
      (symbol-name (symbol-at-point)))
    (error nil)))

(defun form-cycle-match-context-pattern (pattern context)
  (if (and (null pattern)
           (null context))
      'match-toplevel
    (loop with pattern-rest = pattern
          for pattern-head = (car pattern-rest)
          for part in context
          when (and part                ; skip nil
                    (eq part pattern-head))
          do (setf pattern-rest (rest pattern-rest))
          when (null pattern-rest)
          return t
          finally return nil))) ; if complete pattern was not matched       
  
(defun pve-surrounding-sexp-car ()
  (save-excursion
    (ignore-errors
      (up-list -1)
      (when (looking-at "( *\\_<")      ; list with symbol at car
        (search-forward-regexp "\\_<")
        (symbol-at-point)))))

(defun form-cycle-at-toplevel-p ()
  (save-excursion
    (let ((top t))
      (ignore-errors (up-list -1)
                     (setf top nil))
      top)))  

(defun form-cycle-gather-context ()
  (save-excursion
    (nreverse
     (loop until (form-cycle-at-toplevel-p)
           for car = (pve-surrounding-sexp-car)
           collect car
           do (up-list -1)))))

(defun form-cycle-determine-context (known-contexts)
  (save-excursion
    (block done 
      (let* ((top-level t)
             (current-context (or (form-cycle-gather-context)
                                  '(cycle-toplevel)))
             (options)
             (matched-context
              (loop for c in known-contexts
                    for opts = nil
                    for pat = (cond ((null (car c))
                                     nil)
                                    ((symbolp (car c))
                                     (list (car c)))
                                        ; Have options
                                    ((and (listp (car c))
                                          (listp (caar c)))
                                     (setf opts (rest (car c)))
                                     (caar c)) 
                                    (t (car c))) 
                    when (form-cycle-match-context-pattern pat current-context)
                    return (progn (setf options opts)
                                  (rest c)))))
        ;; Matched-context is the list of forms, i.e. the second
        ;; element of the context.
        (list 'forms (form-cycle-process-includes matched-context
                                                 known-contexts)
              'options options)))))

(defun form-cycle-pattern-key (context)
  (if (and (listp (car context))
           (listp (caar context)))
      (caar context)
    (car context)))

(defun form-cycle-process-includes (context known-contexts)
  ;; Context is the one chosen by form-cycle-determine-context.
  (let (complete-context)
    (loop for form in context
          if (and (consp form) ; (form-cycle-include-context foo)
                  (eq (first form) 'form-cycle-include-context))
          do (loop for form2 in (rest (find (second form) ; foo 
                                            known-contexts
                                            :key #'form-cycle-pattern-key
                                            :test #'equal))
                   do (push form2 complete-context))
          else do (push form complete-context))
    (nreverse complete-context)))
        
(defun form-cycle-lisp-forms (&optional lisp-forms)
  (interactive)
  (when (null lisp-forms)
    (setf lisp-forms form-cycle-lisp-forms))
  (let ((form-cycle-function 'form-cycle-with-name)
        (context (form-cycle-determine-context lisp-forms))
        (form-cycle-up-list-initially-p)
        (form-cycle-up-list-initially-sexp-string)
        (form-cycle-raise-list-initially-p))
    (loop for opt in (getf context 'options)
          do
          (case opt
            (up-list (setf form-cycle-up-list-initially-p t))))
    (form-cycle-debug context)
    (form-cycle-initiate (getf context 'forms))))

(defun form-cycle-add (pattern forms &optional options)
  (if (listp pattern)
      (progn
        (pushnew (list* (list* pattern options)
                        forms)
                 form-cycle-lisp-forms
                 :test #'equal
                 :key #'form-cycle-pattern-key)
        t)
    nil))

(defun form-cycle-add-interactively (pattern forms &optional options)
  (if (form-cycle-add pattern forms options)
      (progn
        (message "Ok.")
        t)
    (progn 
      (message "Error")
      nil)))

(defun form-cycle-find (pattern)
  (find pattern form-cycle-lisp-forms
        :key #'form-cycle-pattern-key
        :test #'equal))

(defun form-cycle-find-interactively (pattern)
  (message "%s" (form-cycle-find pattern)))

(defun form-cycle-delete (pattern)
  (if (form-cycle-find pattern)
      (progn
        (setf form-cycle-lisp-forms
              (cl-delete pattern form-cycle-lisp-forms
                         :key #'form-cycle-pattern-key
                         :test #'equal))
        t)
    nil))

(defun form-cycle-delete-interactively (pattern)
  (if (form-cycle-delete pattern)
      (progn (message "Deleted.")
             t)
    (progn (message "Error")
           nil)))

(defun form-cycle-replace (pattern forms &optional options)
  (form-cycle-delete pattern)
  (form-cycle-add pattern forms options))

(defun form-cycle-replace-interactively (pattern forms &optional options)
  (form-cycle-delete-interactively pattern)
  (form-cycle-add-interactively pattern forms options))

(defvar form-cycle-lisp-forms
  '((progn (form-cycle-include-context nil))

    (format 
     "\"~S~%\""
     "\"~A~%\""
     "\"~{~A~^, ~}~%\""
     "\"~{~S~^, ~}~%\"")
 
    (assert
     "(null @_)"
     "(not (null @_))")

    (((loop for-as-hash) up-list)
     (":for _ :being :each :hash-key :using (:hash-value val) :in @")
     (":for _ :being :each :hash-value :using (:hash-key key) :in @")
     (":for %%% :being :each :hash-key :using (:hash-value val) :in _"
      map-form form-cycle-%%%-to-first-char-of-current-name)
     (":for %%% :being :each :hash-value :using (:hash-key key) :in _"
      map-form form-cycle-%%%-to-first-char-of-current-name))

    (((loop for-as-package) up-list)
     (":for sym :being :each :symbol :in @" map-form form-cycle-%%%-to-first-char-of-current-name)
     (":for sym :being :each :present-symbol :in @" map-form form-cycle-%%%-to-first-char-of-current-name)
     (":for sym :being :each :external-symbol :in @" map-form form-cycle-%%%-to-first-char-of-current-name)
     (":for sym :being :each :symbol :in (find-package \"_\")"
      map-form (lambda (form) 
                 (setf form-cycle-current-name (upcase form-cycle-current-name))
                 form))
     (":for sym :being :each :present-symbol :in (find-package \"_\")"
      map-form (lambda (form) 
                 (setf form-cycle-current-name (upcase form-cycle-current-name))
                 form))
     (":for sym :being :each :external-symbol :in (find-package \"_\")"
      map-form (lambda (form) 
                 (setf form-cycle-current-name (upcase form-cycle-current-name))
                 form)))

    (((loop simple-loop-clauses) up-list)
     (":for %%% :in _" map-form form-cycle-%%%-to-first-char-of-current-name)
     (":for _ :from 0 :to ")
     (":for _ :from 0 :below ")
     (":for %%% = _" map-form form-cycle-%%%-to-first-char-of-current-name)
     (":for %%% = _ :then " map-form form-cycle-%%%-to-first-char-of-current-name)
     (":for %%% :across _" map-form form-cycle-%%%-to-first-char-of-current-name))

    (((loop loop-clause) up-list)
     "(simple-loop-clauses @)"
     "(for-as-hash @)"
     "(for-as-package @)")

    (loop "(loop-clause@)"
          (form-cycle-include-context nil))

    (cycle-toplevel
     "(defun _ (@)\n  )"
     "(defmethod _ (@)\n  )"
     "(defgeneric _ (@))"
     "(defclass _ ()\n  (@))"
     "(defvar _ @)"
     "(defparameter _ @)"
     ("(defmethod initialize-instance :after ((%%% _) &key)\n  @)" 
      map-form form-cycle-%%%-to-first-char-of-current-name)
     "(defpackage #:_
  (:use #:cl)
  (:local-nicknames)
  (:export))\n\n(in-package #:_)")

    (defclass
      "(%_ :initarg :_ :initform nil)"
      "(%_ :initarg :_ :accessor _ :initform nil)"
      ("(%_ :initarg :_
    :accessor _
    :initform nil)" after-cycle form-cycle-indent-defun)
      ("(%_ :initarg :_
    :accessor %%%-_
    :initform nil)" 
       map-form (lambda (string) 
                  (replace-regexp-in-string "%%%" 
                                            (form-cycle-toplevel-form-name)
                                            string))
       after-cycle form-cycle-indent-defun))

    ((defpackage :local-nicknames)
     ("\"_\"" map-string upcase)
     "#:_"
     ("(#:%%% #:_)" map-form form-cycle-%%%-to-first-char-of-current-name)
     ("(#:%%% #:_)" map-form form-cycle-%%%-to-first-two-chars-of-current-name)
     ("(#:%%% #:_)" map-form (lambda (form)
                               (form-cycle-%%%-to-subseq-of-current-name form 3))))

    (((defpackage option) up-list)
     ("(:use @)
  (option)")
     ("(:local-nicknames @)
  (option)")
     ("(:export @)
  (option)")
     ("(:shadow @)
  (option)")
     ("(:shadowing-import-from @)
  (option)")
     ("(:import-from @)
  (option)")
     ("(:documentation \"@\")
  (option)"))

    ((defpackage)
     ("\"_\"" map-string upcase)
     "#:_"
     "(option)")
    
    ((defgeneric :method)
     ("(%%% _)" map-form form-cycle-%%%-to-first-char-of-current-name)
     (form-cycle-include-context nil))

    (defgeneric
      "(:method (@)
    )"
      "(:documentation \"\")")

    (defmethod
      ("(%%% _)" map-form form-cycle-%%%-to-first-char-of-current-name)
      (form-cycle-include-context nil))

    (asdf:defsystem 
     "(:file \"_\")"
     "(:module \"_\"
                        :components (@))")

    (((form-cycle-add) up-list)
     ("ok" map-string (lambda (s)
                        (let* ((form (car (read-from-string
                                           form-cycle-up-list-initially-sexp-string)))
                               (command (rest form))
                               (pattern (getf command 'pattern))
                               (forms (getf command 'forms))
                               (options (getf command 'options)))
                          (form-cycle-add-interactively pattern forms options)
                          form-cycle-up-list-initially-sexp-string))
      place-point 1))

    (((form-cycle-replace) up-list)
     ("ok" map-string (lambda (s)
                        (let* ((form (car (read-from-string
                                           form-cycle-up-list-initially-sexp-string)))
                               (command (rest form))
                               (pattern (getf command 'pattern))
                               (forms (getf command 'forms))
                               (options (getf command 'options)))
                          (form-cycle-replace-interactively pattern forms options)
                          form-cycle-up-list-initially-sexp-string))
      place-point 1))
    
    (((form-cycle-delete) up-list)
     ("ok" map-string (lambda (s)
                        (let ((form (car (read-from-string
                                          form-cycle-up-list-initially-sexp-string))))
                          (form-cycle-delete-interactively (second form))
                          form-cycle-up-list-initially-sexp-string))
      place-point 1))

    (((form-cycle-find) up-list)
     ("ok" map-string (lambda (s)
                        (let ((form (car (read-from-string
                                          form-cycle-up-list-initially-sexp-string))))
                          (print form-cycle-up-list-initially-sexp-string)
                          (print form)
                          (form-cycle-find-interactively (second form)))
                          form-cycle-up-list-initially-sexp-string))
      place-point 1)

    ;; Always matches.
    (nil "(setf _ @)"
         "(make-instance '_ @)"
         "(let ((_ @))\n    )"
         "(let* ((_ @))\n    )"
         "(lambda (_) @)"
         "(check-type _ @)"
         "(assert @_)"
         "(declare (ignore _))"
         ("(return-from %%% @_)"        
          map-form (lambda (string) 
                     (let ((name (form-cycle-toplevel-form-name)))
                       (if name
                           (replace-regexp-in-string "%%%" name string)
                         string)))))))

