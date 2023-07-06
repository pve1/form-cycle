;; -*- lexical-binding: t -*-

(defvar pve-cycle-current-cycle-state nil)
(defvar pve-cycle-position nil)
(defvar pve-cycle-initial-position nil)
(defvar pve-cycle-new-cycle-p nil)
(defvar pve-cycle-undo-previous-function nil)
(defvar pve-cycle-function 'pve-cycle-default-cycle-function)

(defvar pve-cycle-debug nil)

(defmacro pve-cycle-debug (thing &optional tag)
  (if pve-cycle-debug
      (if tag
          `(princ (format "%s: %s: %s\n" ',tag ',thing ,thing))
        `(princ (format "%s: %s\n" ',thing ,thing)))
    '()))
 
;; Basic functionality: Inserting strings in a cycle.

(defun pve-cycle-make-undo-function (length)
  (lambda ()
    (when pve-cycle-initial-position
      (goto-char pve-cycle-initial-position))
    (delete-forward-char length)
    (setf pve-cycle-undo-previous-function nil)))

(defun pve-cycle-default-cycle-function (form)
  (unless pve-cycle-undo-previous-function
    (setf pve-cycle-undo-previous-function
          (lambda ()
            (when pve-cycle-initial-position
              (goto-char pve-cycle-initial-position))
            (pve-cycle-debug form pve-cycle-undo-previous-function)
            (pve-cycle-debug (length form) pve-cycle-undo-previous-function)
            (delete-forward-char (length form))
            (setf pve-cycle-undo-previous-function nil))))
  (insert form)
  form)
                            
(defun pve-cycle-next ()
  (pve-cycle-debug pve-cycle-current-cycle-state)
  (let ((next (pop pve-cycle-current-cycle-state)))
    (when next
      (setf pve-cycle-current-cycle-state
            (append pve-cycle-current-cycle-state (list next))))
    (when pve-cycle-undo-previous-function
      (funcall pve-cycle-undo-previous-function))
    (funcall pve-cycle-function next) 
    (setf pve-cycle-position (point))))

(defun pve-cycle-new-cycle-p ()
  (or (null pve-cycle-position)
      (and pve-cycle-position
           (not (= pve-cycle-position (point)))))) ; Point has moved

(defun pve-cycle-initiate (cycle)
  (if (pve-cycle-new-cycle-p)
      (let ((pve-cycle-new-cycle-p t)) 
        (pve-cycle-debug "New Cycle.")
        (setq pve-cycle-current-cycle-state cycle
              pve-cycle-undo-previous-function nil
              pve-cycle-initial-position (point))
        (pve-cycle-next))
    (pve-cycle-next)))

(defun pve-cycle-test ()
  (interactive)
  (pve-cycle-initiate '("abc" "foo" "bar")))

;; Adding names and place point.

(defvar pve-cycle-current-name nil)
(defvar pve-cycle-name-marker "_")
(defvar pve-cycle-point-marker "@")
(defvar pve-cycle-up-list-initially-p nil)
(defvar pve-cycle-up-list-initially-sexp-string nil)
(defvar pve-cycle-raise-list-initially-p nil)

(defun pve-cycle-symbol-at-point ()
  (let ((sym (symbol-at-point)))
    (when (and sym
               (string-match "^\\_<" (symbol-name sym)))
      sym)))

(defun pve-cycle-beginning-of-symbol-maybe ()
  (when (pve-cycle-symbol-at-point)
    (beginning-of-sexp)))

;; Point should be at the beginning of the form that was previously
;; inserted.
(defun pve-cycle-with-name (form)
  ;; Initialize
  (when pve-cycle-new-cycle-p
    (when pve-cycle-up-list-initially-p
      (save-excursion
        (up-list -1)
        (setf pve-cycle-up-list-initially-sexp-string
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
            (setf pve-cycle-current-name (symbol-name sym)
                  pve-cycle-initial-position (point)))
        (setf pve-cycle-current-name "")))
    (when pve-cycle-up-list-initially-p
      (up-list -1)             
      (kill-sexp)      
      (setf pve-cycle-initial-position (point)))
    (when pve-cycle-raise-list-initially-p
      (pve-cycle-beginning-of-symbol-maybe)
      (raise-sexp)
      (setf pve-cycle-initial-position (point))))
  
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
      (replace-string pve-cycle-name-marker pve-cycle-current-name)
      (beginning-of-buffer)
      ;; Figure out where to place point
      (when (search-forward pve-cycle-point-marker nil t)
        (delete-backward-char (length pve-cycle-point-marker))
        (setf place-point (1- (point))))
      (setf string (buffer-substring-no-properties 1 (buffer-end 1))))
    (pve-cycle-debug string pve-cycle-with-name)
    (when (getf options 'map-string)
      (save-excursion
        (setf string (funcall (getf options 'map-string) string))))
    (pve-cycle-default-cycle-function string)
    (when place-point
      (goto-char pve-cycle-initial-position)
      (forward-char place-point)
      (when (getf options 'after-place-point)
        (save-excursion
          (funcall (getf options 'after-place-point)))))
    (when (getf options 'place-point)
      (let ((p (getf options 'place-point)))
        (typecase p
          (integer (goto-char (+ pve-cycle-initial-position p)))
          (function (funcall p)))))
    (when (getf options 'after-cycle)
      (let ((end (save-excursion
                   (let ((len (length string)))
                     (goto-char (+ pve-cycle-initial-position len))
                     (push-mark)
                     (point)))))
        (save-excursion 
          (funcall (getf options 'after-cycle)))
        (unless (= (mark) end)
          (pve-cycle-debug (mark))
          (pve-cycle-debug end)
          (setf pve-cycle-undo-previous-function
                (pve-cycle-make-undo-function 
                 (- (mark) pve-cycle-initial-position))))
        (pop-mark)))
    string))

(defun pve-cycle-test-with-name ()
  (interactive)
  (let ((pve-cycle-function 'pve-cycle-with-name))
    (pve-cycle-initiate '("(defun _ () @)"
                          "(defclass _ () (@))"))))

;; Context aware lisp forms

(defun pve-cycle-%%%-to-subseq-of-current-name (form length)
  (if (< (length pve-cycle-current-name) length)
      form
    (let ((prefix (subseq pve-cycle-current-name 0 length)))
      (replace-regexp-in-string "%%%" prefix form))))

(defun pve-cycle-%%%-to-first-char-of-current-name (form)
  (pve-cycle-%%%-to-subseq-of-current-name form 1)) 

(defun pve-cycle-%%%-to-first-two-chars-of-current-name (form)
  (pve-cycle-%%%-to-subseq-of-current-name form 2))

(defun pve-cycle-indent-defun ()
  (beginning-of-defun)
  (indent-pp-sexp))

(defun pve-cycle-toplevel-form-nth (n)
  (condition-case nil
    (save-excursion 
      (beginning-of-defun)
      (down-list)
      (forward-sexp n)
      (symbol-name (symbol-at-point)))
    (error nil)))

(defun pve-cycle-toplevel-form-name ()
  (condition-case nil
    (save-excursion 
      (beginning-of-defun)
      (down-list)
      (forward-sexp 2)
      (symbol-name (symbol-at-point)))
    (error nil)))

(defun pve-cycle-match-context-pattern (pattern context)
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

(defun pve-cycle-at-toplevel-p ()
  (save-excursion
    (let ((top t))
      (ignore-errors (up-list -1)
                     (setf top nil))
      top)))  

(defun pve-cycle-gather-context ()
  (save-excursion
    (nreverse
     (loop until (pve-cycle-at-toplevel-p)
           for car = (pve-surrounding-sexp-car)
           collect car
           do (up-list -1)))))

(defun pve-cycle-determine-context (known-contexts)
  (save-excursion
    (block done 
      (let* ((top-level t)
             (current-context (or (pve-cycle-gather-context)
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
                    when (pve-cycle-match-context-pattern pat current-context)
                    return (progn (setf options opts)
                                  (rest c)))))
        ;; Matched-context is the list of forms, i.e. the second
        ;; element of the context.
        (list 'forms (pve-cycle-process-includes matched-context
                                                 known-contexts)
              'options options)))))

(defun pve-cycle-pattern-key (context)
  (if (and (listp (car context))
           (listp (caar context)))
      (caar context)
    (car context)))

(defun pve-cycle-process-includes (context known-contexts)
  ;; Context is the one chosen by pve-cycle-determine-context.
  (let (complete-context)
    (loop for form in context
          if (and (consp form) ; (pve-cycle-include-context foo)
                  (eq (first form) 'pve-cycle-include-context))
          do (loop for form2 in (rest (find (second form) ; foo 
                                            known-contexts
                                            :key #'pve-cycle-pattern-key
                                            :test #'equal))
                   do (push form2 complete-context))
          else do (push form complete-context))
    (nreverse complete-context)))
        
(defun pve-cycle-lisp-forms (&optional lisp-forms)
  (interactive)
  (when (null lisp-forms)
    (setf lisp-forms pve-cycle-lisp-forms))
  (let ((pve-cycle-function 'pve-cycle-with-name)
        (context (pve-cycle-determine-context lisp-forms))
        (pve-cycle-up-list-initially-p)
        (pve-cycle-up-list-initially-sexp-string)
        (pve-cycle-raise-list-initially-p))
    (loop for opt in (getf context 'options)
          do
          (case opt
            (up-list (setf pve-cycle-up-list-initially-p t))))
    (pve-cycle-debug context)
    (pve-cycle-initiate (getf context 'forms))))

(defun pve-cycle-add (pattern forms &optional options)
  (if (listp pattern)
      (progn
        (pushnew (list* (list* pattern options)
                        forms)
                 pve-cycle-lisp-forms
                 :test #'equal
                 :key #'pve-cycle-pattern-key)
        t)
    nil))

(defun pve-cycle-add-interactively (pattern forms &optional options)
  (if (pve-cycle-add pattern forms options)
      (progn
        (message "Ok.")
        t)
    (progn 
      (message "Error")
      nil)))

(defun pve-cycle-find (pattern)
  (find pattern pve-cycle-lisp-forms
        :key #'pve-cycle-pattern-key
        :test #'equal))

(defun pve-cycle-find-interactively (pattern)
  (message "%s" (pve-cycle-find pattern)))

(defun pve-cycle-delete (pattern)
  (if (pve-cycle-find pattern)
      (progn
        (setf pve-cycle-lisp-forms
              (cl-delete pattern pve-cycle-lisp-forms
                         :key #'pve-cycle-pattern-key
                         :test #'equal))
        t)
    nil))

(defun pve-cycle-delete-interactively (pattern)
  (if (pve-cycle-delete pattern)
      (progn (message "Deleted.")
             t)
    (progn (message "Error")
           nil)))

(defun pve-cycle-replace (pattern forms &optional options)
  (pve-cycle-delete pattern)
  (pve-cycle-add pattern forms options))

(defun pve-cycle-replace-interactively (pattern forms &optional options)
  (pve-cycle-delete-interactively pattern)
  (pve-cycle-add-interactively pattern forms options))

(defvar pve-cycle-lisp-forms
  '((progn (pve-cycle-include-context nil))

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
      map-form pve-cycle-%%%-to-first-char-of-current-name)
     (":for %%% :being :each :hash-value :using (:hash-key key) :in _"
      map-form pve-cycle-%%%-to-first-char-of-current-name))

    (((loop for-as-package) up-list)
     (":for sym :being :each :symbol :in @" map-form pve-cycle-%%%-to-first-char-of-current-name)
     (":for sym :being :each :present-symbol :in @" map-form pve-cycle-%%%-to-first-char-of-current-name)
     (":for sym :being :each :external-symbol :in @" map-form pve-cycle-%%%-to-first-char-of-current-name)
     (":for sym :being :each :symbol :in (find-package \"_\")"
      map-form (lambda (form) 
                 (setf pve-cycle-current-name (upcase pve-cycle-current-name))
                 form))
     (":for sym :being :each :present-symbol :in (find-package \"_\")"
      map-form (lambda (form) 
                 (setf pve-cycle-current-name (upcase pve-cycle-current-name))
                 form))
     (":for sym :being :each :external-symbol :in (find-package \"_\")"
      map-form (lambda (form) 
                 (setf pve-cycle-current-name (upcase pve-cycle-current-name))
                 form)))

    (((loop simple-loop-clauses) up-list)
     (":for %%% :in _" map-form pve-cycle-%%%-to-first-char-of-current-name)
     (":for _ :from 0 :to ")
     (":for _ :from 0 :below ")
     (":for %%% = _" map-form pve-cycle-%%%-to-first-char-of-current-name)
     (":for %%% = _ :then " map-form pve-cycle-%%%-to-first-char-of-current-name)
     (":for %%% :across _" map-form pve-cycle-%%%-to-first-char-of-current-name))

    (((loop loop-clause) up-list)
     "(simple-loop-clauses @)"
     "(for-as-hash @)"
     "(for-as-package @)")

    (loop "(loop-clause@)"
          (pve-cycle-include-context nil))

    (cycle-toplevel
     "(defun _ (@)\n  )"
     "(defmethod _ (@)\n  )"
     "(defgeneric _ (@))"
     "(defclass _ ()\n  (@))"
     "(defvar _ @)"
     "(defparameter _ @)"
     ("(defmethod initialize-instance :after ((%%% _) &key)\n  @)" 
      map-form pve-cycle-%%%-to-first-char-of-current-name)
     "(defpackage #:_
  (:use #:cl)
  (:local-nicknames)
  (:export))\n\n(in-package #:_)")

    (defclass
      "(%_ :initarg :_ :initform nil)"
      "(%_ :initarg :_ :accessor _ :initform nil)"
      ("(%_ :initarg :_
    :accessor _
    :initform nil)" after-cycle pve-cycle-indent-defun)
      ("(%_ :initarg :_
    :accessor %%%-_
    :initform nil)" 
       map-form (lambda (string) 
                  (replace-regexp-in-string "%%%" 
                                            (pve-cycle-toplevel-form-name)
                                            string))
       after-cycle pve-cycle-indent-defun))

    ((defpackage :local-nicknames)
     ("\"_\"" map-string upcase)
     "#:_"
     ("(#:%%% #:_)" map-form pve-cycle-%%%-to-first-char-of-current-name)
     ("(#:%%% #:_)" map-form pve-cycle-%%%-to-first-two-chars-of-current-name)
     ("(#:%%% #:_)" map-form (lambda (form)
                               (pve-cycle-%%%-to-subseq-of-current-name form 3))))

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
     ("(%%% _)" map-form pve-cycle-%%%-to-first-char-of-current-name)
     (pve-cycle-include-context nil))

    (defgeneric
      "(:method (@)
    )"
      "(:documentation \"\")")

    (defmethod
      ("(%%% _)" map-form pve-cycle-%%%-to-first-char-of-current-name)
      (pve-cycle-include-context nil))

    (asdf:defsystem 
     "(:file \"_\")"
     "(:module \"_\"
                        :components (@))")

    (((pve-cycle-add) up-list)
     ("ok" map-string (lambda (s)
                        (let* ((form (car (read-from-string
                                           pve-cycle-up-list-initially-sexp-string)))
                               (command (rest form))
                               (pattern (getf command 'pattern))
                               (forms (getf command 'forms))
                               (options (getf command 'options)))
                          (pve-cycle-add-interactively pattern forms options)
                          pve-cycle-up-list-initially-sexp-string))
      place-point 1))

    (((pve-cycle-replace) up-list)
     ("ok" map-string (lambda (s)
                        (let* ((form (car (read-from-string
                                           pve-cycle-up-list-initially-sexp-string)))
                               (command (rest form))
                               (pattern (getf command 'pattern))
                               (forms (getf command 'forms))
                               (options (getf command 'options)))
                          (pve-cycle-replace-interactively pattern forms options)
                          pve-cycle-up-list-initially-sexp-string))
      place-point 1))
    
    (((pve-cycle-delete) up-list)
     ("ok" map-string (lambda (s)
                        (let ((form (car (read-from-string
                                          pve-cycle-up-list-initially-sexp-string))))
                          (pve-cycle-delete-interactively (second form))
                          pve-cycle-up-list-initially-sexp-string))
      place-point 1))

    (((pve-cycle-find) up-list)
     ("ok" map-string (lambda (s)
                        (let ((form (car (read-from-string
                                          pve-cycle-up-list-initially-sexp-string))))
                          (print pve-cycle-up-list-initially-sexp-string)
                          (print form)
                          (pve-cycle-find-interactively (second form)))
                          pve-cycle-up-list-initially-sexp-string))
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
                     (let ((name (pve-cycle-toplevel-form-name)))
                       (if name
                           (replace-regexp-in-string "%%%" name string)
                         string)))))))

