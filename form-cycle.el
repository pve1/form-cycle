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
            (append form-cycle-current-cycle-state (list next)))
      (when form-cycle-undo-previous-function
        (funcall form-cycle-undo-previous-function))
      (funcall form-cycle-function next) 
      (setf form-cycle-position (point)))))

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
      (cond ((and sym
                  (string-match "^\\_<" (symbol-name sym)))
             (unless (looking-at "\\_<")
               (beginning-of-sexp))
             (kill-sexp)
             (setf form-cycle-current-name (symbol-name sym)
                   form-cycle-initial-position (point)))

            ((and (not sym)
                  (looking-at "("))
             (kill-sexp)
             (setf form-cycle-current-name (substring-no-properties
                                            (current-kill 0))
                   form-cycle-initial-position (point)))
            (t (setf form-cycle-current-name ""))))

    (when form-cycle-up-list-initially-p
      (up-list -1)             
      (kill-sexp)      
      (setf form-cycle-initial-position (point)))
    (when form-cycle-raise-list-initially-p
      (form-cycle-beginning-of-symbol-maybe)
      (raise-sexp)
      (setf form-cycle-initial-position (point))))

  (let* ((place-point)
         (form-string-designator (form-cycle-context-form-string form))
         (form-string (if (or (functionp form-string-designator)
                              (symbolp form-string-designator))
                          (funcall form-string-designator)
                        form-string-designator))
         (getopt (lambda (opt)
                   (second (form-cycle-context-form-assoc form opt))))
         (string))

    (when (form-cycle-context-form-assoc form 'map-form)
      (save-excursion
        (setf form-string
              (funcall (funcall getopt 'map-form) form-string))))

    ;; Build string
    (with-temp-buffer 
      (insert form-string)
      (beginning-of-buffer)
      ;; _ -> name
      (if (equal form-cycle-current-name "")
          ;; Ignore @, place point at _ instead.
          (let ()
            ;; _ -> @
            (save-excursion (replace-string form-cycle-name-marker 
                                            form-cycle-point-marker
                                            nil 1 (buffer-end 1)))
            ;; Find first @
            (search-forward form-cycle-point-marker nil t)
            (unless (eobp)
              (forward-char))
            ;; Delete the other @'s
            (save-excursion (replace-string form-cycle-point-marker "")))
        (replace-string form-cycle-name-marker form-cycle-current-name))
      (beginning-of-buffer)
      ;; Figure out where to place point
      (when (search-forward form-cycle-point-marker nil t)
        (delete-backward-char (length form-cycle-point-marker))
        (setf place-point (1- (point))))
      (setf string (buffer-substring-no-properties 1 (buffer-end 1))))
    (form-cycle-debug string form-cycle-with-name)
    (when (funcall getopt 'map-string)
      (save-excursion
        (setf string (funcall (funcall getopt 'map-string) string))))
    (form-cycle-default-cycle-function string)
    (when place-point
      (goto-char form-cycle-initial-position)
      (forward-char place-point)
      (when (funcall getopt 'after-place-point)
        (save-excursion
          (funcall (funcall getopt 'after-place-point)))))
    (when (funcall getopt 'place-point)
      (let ((p (funcall getopt 'place-point)))
        (typecase p
          (integer (goto-char (+ form-cycle-initial-position p)))
          (function (funcall p)))))
    (when (funcall getopt 'after-cycle)
      (let ((end (save-excursion
                   (let ((len (length string)))
                     (goto-char (+ form-cycle-initial-position len))
                     (push-mark)
                     (point)))))
        (save-excursion 
          (funcall (funcall getopt 'after-cycle)))
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

(defun form-cycle-%%%-to-toplevel-name (form)
  (replace-regexp-in-string "%%%" 
                            (form-cycle-toplevel-form-name)
                            form))

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

(defun form-cycle-surrounding-sexp-car ()
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
           for car = (form-cycle-surrounding-sexp-car)
           collect car
           do (up-list -1)))))

(defun form-cycle-match-context-pattern (pattern current-context)
  (if (and (null pattern)
           (null current-context))
      'match-toplevel
    (progn   
      (when (and (symbolp pattern)
                 (setf pattern (list pattern))))
      (loop with pattern-rest = pattern
            for pattern-head = (car pattern-rest)
            for part in current-context
            when (or (eq part pattern-head)
                     (equal '(*) pattern-head))
            do (setf pattern-rest (rest pattern-rest))
            when (null pattern-rest)
            return t
            finally return nil)))) ; if complete pattern was not matched 

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

(defun form-cycle-determine-matching-contexts (known-contexts)
  (save-excursion
    (block done 
      (let* ((current-context (form-cycle-gather-context)))
        (loop for c in known-contexts
              for pat = (form-cycle-context-pattern c)
              when (form-cycle-match-context-pattern pat current-context)
              collect c)))))

(defun form-cycle-process-includes (context known-contexts &optional already-included)
  ;; Context is the one chosen by form-cycle-determine-context.
  (let (complete-context
        patterns-included)
    (loop for form in (form-cycle-context-forms context)
          if (and (consp form) ; (form-cycle-include foo)
                  (eq (first form) 'form-cycle-include))
          do (let ((pat (rest form)))
               (unless (or (find pat already-included :test #'equal)
                           (find pat patterns-included :test #'equal))                 
                 (loop for form2 in (form-cycle-context-forms
                                     (form-cycle-find pat))
                       do (push form2 complete-context))
                 (push pat patterns-included)))
          else do (push form complete-context))

    ;; Recurse if an include directive was found.
    (if patterns-included
        (form-cycle-process-includes
         (form-cycle-make-context 
          (form-cycle-context-pattern context)
          (nreverse complete-context)
          (form-cycle-context-pattern-options context))
         known-contexts
         (append patterns-included already-included))
      (form-cycle-make-context 
       (form-cycle-context-pattern context)
       (nreverse complete-context)
       (form-cycle-context-pattern-options context)))))

(defun form-cycle-lisp-forms (&optional lisp-forms)
  (interactive)
  (when (null lisp-forms)
    (setf lisp-forms form-cycle-lisp-forms))
  (let ((form-cycle-function 'form-cycle-with-name)
        (context (form-cycle-process-includes
                  (first (form-cycle-determine-matching-contexts 
                          lisp-forms))
                  lisp-forms))
        (form-cycle-up-list-initially-p)
        (form-cycle-up-list-initially-sexp-string)
        (form-cycle-raise-list-initially-p))
    (loop for opt in (form-cycle-context-pattern-options context)
          do
          (case opt
            (up-list (setf form-cycle-up-list-initially-p t))))
    (form-cycle-initiate (form-cycle-context-forms context))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun form-cycle-check-plist (thing)
  (check-type thing list)
  (loop for (a b) on thing by #'cddr
        do (check-type a symbol)))

(defun form-cycle-check-alist (thing)
  (check-type thing list)
  (loop for i in thing 
        do (unless (symbolp i)
             (check-type i cons))))

(defun form-cycle-check-option (thing)
  (if (consp thing)
      (check-type (car thing) symbol)
    (check-type thing symbol)))

;; Accessors
(defun form-cycle-make-context (pattern forms &optional options)
  (check-type pattern (or symbol list))
  (check-type forms list)
  (dolist (opt options)
    (form-cycle-check-option opt))
  (dolist (f forms)
    (if (listp f)
        (unless (eq (car f) 'form-cycle-include)
          (check-type (car f) string)
          (form-cycle-check-alist (rest f)))
      (check-type f string)))
  (list (list* 'pattern pattern)
        (list* 'options options)
        (list* 'forms forms)))

(defun form-cycle-context-pattern (context)
  (cdr (assq 'pattern context)))

(defun form-cycle-context-pattern-options (context)
  (cdr (assq 'options context)))

;; Like assoc but a symbol is considered equal to (foo t)
(defun form-cycle-context-assoc (context-alist key)
  (loop for i in context-alist
        do (cond ((and (consp i)
                       (equal (car i) key))
                  (return i))
                 ((equal key i)
                  (return (list i t))))))

(defun form-cycle-context-pattern-assoc (context key)
  (form-cycle-context-assoc
   (form-cycle-context-pattern-options context)
   key))

(defun form-cycle-context-forms (context)
  (cdr (assq 'forms context)))
  
(defun form-cycle-context-form-options (form)
  (if (listp form)
      (rest form)
    nil))

(defun form-cycle-context-form-string (form)
  (if (listp form)
      (first form)
    form))

(defun form-cycle-context-form-assoc (form key)
  (form-cycle-context-assoc
   (form-cycle-context-form-options form)
   key))

(defun form-cycle-read-context-from-buffer ()
  (let (forms)
    (loop for form = (condition-case err
                         (read (current-buffer))
                       (end-of-file (return)))
          do (push form forms))
    (setf forms (nreverse forms))
    (let ((pat (if (consp (first forms))
                   (caar forms)
                 (first forms)))
          (options (if (consp (first forms))
                       (rest (first forms)) 
                     nil)))
      (form-cycle-make-context 
       (nth 0 forms)
       (nthcdr 2 forms)
       (nth 1 forms)))))

;; Manipulation

(defmacro form-cycle-define-pattern (pattern options &rest forms)
  `(form-cycle-add ',pattern ',forms ',options))

(put 'form-cycle-define-pattern 'lisp-indent-function 2)

(defun form-cycle-add (pattern forms &optional options)
  (if (form-cycle-find pattern)
      (form-cycle-replace pattern forms options)
    (pushnew (form-cycle-make-context pattern forms options)
             form-cycle-lisp-forms
             :test #'equal
             :key #'form-cycle-context-pattern)))

(defun form-cycle-add-semi-interactively (pattern forms &optional options)
  (if (form-cycle-add pattern forms options)
      (progn
        (message "Ok.")
        t)
    (progn 
      (message "Error")
      nil)))

;; Adds a form to the context.
(defun form-cycle-add-interactively (b e)
  (interactive "r")
  (let* ((context (form-cycle-gather-context))
         (pattern (read-from-minibuffer "Context: "
                                        (with-output-to-string
                                          (princ context))
                                        nil 
                                        t))
         (exist (form-cycle-find pattern)))
    (if exist
        (form-cycle-replace-semi-interactively 
         pattern
         (append (list (buffer-substring-no-properties b e))
                 (form-cycle-context-forms exist))
         (form-cycle-context-pattern-options exist))
      (form-cycle-add-semi-interactively 
       pattern 
       (list (buffer-substring-no-properties
              b e))))))

(defun form-cycle-edit-interactively ()
  (interactive)
  (let* ((context (form-cycle-gather-context))
         (pattern (read-from-minibuffer "Context: "
                                        (with-output-to-string
                                          (princ context))
                                        nil 
                                        t))
         (exist (form-cycle-find pattern)))
    (unless exist
      (form-cycle-add pattern nil nil)
      (setf exist (form-cycle-find pattern)))
    (form-cycle-open-context-edit-buffer exist)))

(define-minor-mode form-cycle-edit-mode "" nil " Form-Cycle-Edit"
  '(("\C-c\C-k" . form-cycle-edit-save))
  (when form-cycle-edit-mode
    t))

(defun form-cycle-edit-save ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (let ((context (form-cycle-read-context-from-buffer)))
      (form-cycle-replace-semi-interactively
       (form-cycle-context-pattern context)
       (form-cycle-context-forms context)
       (form-cycle-context-pattern-options context)))))

(defun form-cycle-open-context-edit-buffer (context)
  (let ((buf (get-buffer-create "*Form cycle edit*")))
    (switch-to-buffer-other-window buf)
    (emacs-lisp-mode)
    (form-cycle-edit-mode 1)
    (delete-region 1 (buffer-end 1))
    (insert ";; Press C-c C-k to save this context.
;; Valid pattern options: up-list
;; Valid form options: map-form map-string after-cycle
;; Form may also be (form-cycle-include-context PATTERN)
\n")
    (insert ";; Pattern\n\n")
    (insert (prin1-to-string
             (form-cycle-context-pattern context)))
    (insert "\n\n;; Options\n\n")
    (insert (prin1-to-string 
             (form-cycle-context-pattern-options context)))
    (insert "\n\n;; Forms\n\n")  
    (loop for form in (form-cycle-context-forms context)
          do
          (insert (prin1-to-string form)) 
          (newline)
          (newline))))

(defun form-cycle-move-to-front-interactively ()
  (interactive)
  (let* ((context (form-cycle-gather-context))
         (pattern (read-from-minibuffer "Context: "
                                        (with-output-to-string
                                          (princ context))
                                        nil 
                                        t))
         (exist (form-cycle-find pattern)))
    (if exist
        (form-cycle-delete pattern)
      (error "No such pattern."))
    (form-cycle-add (form-cycle-context-pattern exist)
                    (form-cycle-context-forms exist) 
                    (form-cycle-context-pattern-options exist))
    (message "Ok.")))

(defun form-cycle-find (pattern)
  (find pattern 
        form-cycle-lisp-forms
        :key #'form-cycle-context-pattern
        :test #'equal))

(defun form-cycle-context-position (pattern)
  (position pattern form-cycle-lisp-forms
            :key #'form-cycle-context-pattern
            :test #'equal))

(defun form-cycle-find-semi-interactively (pattern)
  (message "%s" (prin1-to-string (form-cycle-find pattern))))

(defun form-cycle-delete (pattern)
  (if (form-cycle-find pattern)
      (progn
        (setf form-cycle-lisp-forms
              (cl-delete pattern form-cycle-lisp-forms
                         :key #'form-cycle-context-pattern
                         :test #'equal))
        t)
    nil))

(defun form-cycle-delete-semi-interactively (pattern)
  (if (form-cycle-delete pattern)
      (progn (message "Deleted.")
             t)
    (progn (message "Error")
           nil)))

(defun form-cycle-delete-interactively ()
  (interactive)
  (let* ((context (form-cycle-gather-context))
         (pattern (read-from-minibuffer "Context: "
                                        (with-output-to-string
                                          (princ context))
                                        nil 
                                        t)))
    (form-cycle-delete-semi-interactively pattern)))

(defun form-cycle-replace (pattern forms &optional options)
  (let ((pos (form-cycle-context-position pattern)))
    (when pos
      (setf (nth pos form-cycle-lisp-forms)
            (form-cycle-make-context pattern forms options)))))

(defun form-cycle-replace-semi-interactively (pattern forms &optional options)
  (form-cycle-replace pattern forms options)
  (message "Ok.")) 

(defvar form-cycle-lisp-forms nil)

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
     (":for %%% :being :each :hash-key :using (:hash-value val) :in _"
      map-form form-cycle-%%%-to-first-char-of-current-name)
     (":for _ :being :each :hash-value :using (:hash-key key) :in @")
     (":for %%% :being :each :hash-value :using (:hash-key key) :in _"
      map-form form-cycle-%%%-to-first-char-of-current-name))

    (((loop for-as-package) up-list)
     (":for sym :being :each :symbol :in @" 
      map-form form-cycle-%%%-to-first-char-of-current-name)

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
     "(simple-loop-clauses _@)"
     "(for-as-hash _@)"
     "(for-as-package _@)")

    (loop "(loop-clause _@)"
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
                          (form-cycle-add-semi-interactively pattern forms options)
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
                          (form-cycle-replace-semi-interactively pattern forms options)
                          form-cycle-up-list-initially-sexp-string))
      place-point 1))
    
    (((form-cycle-delete) up-list)
     ("ok" map-string (lambda (s)
                        (let ((form (car (read-from-string
                                          form-cycle-up-list-initially-sexp-string))))
                          (form-cycle-delete-semi-interactively (second form))
                          form-cycle-up-list-initially-sexp-string))
      place-point 1))

    (((form-cycle-find) up-list)
     ("ok" map-string (lambda (s)
                        (let ((form (car (read-from-string
                                          form-cycle-up-list-initially-sexp-string))))
                          (form-cycle-find-semi-interactively (second form)))
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

