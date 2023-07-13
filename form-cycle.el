;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'ido)

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
    (delete-char length)
    (setf form-cycle-undo-previous-function nil)))

(defun form-cycle-default-cycle-function (form)
  (unless form-cycle-undo-previous-function
    (setf form-cycle-undo-previous-function
          (lambda ()
            (when form-cycle-initial-position
              (goto-char form-cycle-initial-position))
            (form-cycle-debug form form-cycle-undo-previous-function)
            (form-cycle-debug (length form) form-cycle-undo-previous-function)
            (delete-char (length form))
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

(defun form-cycle-skip ()
  (setf form-cycle-position (point))
  (throw 'form-cycle-skip 'form-cycle-skip)) 

(defvar form-cycle-skip nil)

(defun form-cycle-new-cycle-p ()
  (or (null form-cycle-position)
      (and form-cycle-position
           (not (= form-cycle-position (point)))))) ; Point has moved

(defun form-cycle-initiate (cycle)
  (let ((skip-count 0))
    (cl-tagbody
     again  
     (when (eq 'form-cycle-skip
               (catch 'form-cycle-skip 
                 (if (form-cycle-new-cycle-p)
                     (let ((form-cycle-new-cycle-p t)) 
                       (form-cycle-debug "New Cycle.")
                       (setq form-cycle-current-cycle-state cycle
                             form-cycle-undo-previous-function nil
                             form-cycle-initial-position (point))
                       (form-cycle-next))
                   (form-cycle-next))))
       (when (< skip-count (length form-cycle-current-cycle-state))
         (incf skip-count)
         (go again))))))
   
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
    (thing-at-point--beginning-of-sexp)))

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
               (thing-at-point--beginning-of-sexp))
             (kill-sexp)
             (setf form-cycle-current-name (symbol-name sym)
                   form-cycle-initial-position (point)))

            ((and (not sym)
                  (looking-at "(\\|\""))
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
      (goto-char (point-min))
      ;; _ -> name
      (if (equal form-cycle-current-name "")
          ;; If no name was given, ignore @ and place point at _
          ;; instead.
          (let ()
            ;; _ -> @
            (save-excursion (replace-string form-cycle-name-marker 
                                            form-cycle-point-marker
                                            nil 1 (buffer-end 1)))
            ;; Find first @
            (search-forward form-cycle-point-marker nil t)
            ;; Delete the other @'s
            (save-excursion (replace-string form-cycle-point-marker "")))
        (replace-string form-cycle-name-marker form-cycle-current-name))
      (goto-char (point-min))
      ;; Figure out where to place point
      (when (search-forward form-cycle-point-marker nil t)
        (delete-char (- (length form-cycle-point-marker)))
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
                     (push-mark nil t)
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

(defvar form-cycle-lisp-patterns nil)

(defun form-cycle-%%%-to-subseq-of-current-name (form length)
  (if (< (length form-cycle-current-name) length)
      form
    (let ((prefix (cl-subseq form-cycle-current-name 0 length)))
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
        (forward-sexp 1)
        (forward-sexp n)
        (symbol-name (symbol-at-point)))
    (error nil)))

(defun form-cycle-read-nth-toplevel-form (n)
  (condition-case nil
      (save-excursion 
        (beginning-of-defun)
        (down-list)
        (forward-sexp 1)
        (forward-sexp n)
        (backward-sexp)
        (read (current-buffer)))
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

(defun form-cycle-match-context-pattern (pattern current-context 
                                                 &optional allowed-range max-depth)
  (block done
    (when (and max-depth (< max-depth (length current-context)))
      (return-from done nil))
    (when (and (null pattern)
               (null current-context))
      (return-from done t))
    (unless (listp pattern)
      (error "Bad pattern."))
    (form-cycle-debug allowed-range)
    (loop with pattern-rest = (reverse pattern)
          for range from 0
          for pattern-head = (car pattern-rest)
          for part in (reverse current-context)
          when (and (or (eq part pattern-head)
                        (equal '(*) pattern-head))
                    (or (null allowed-range)
                        (<= range allowed-range)))
          do (setf pattern-rest (rest pattern-rest))
          when (null pattern-rest)
          return t
          finally return nil))) ; if complete pattern was not matched 

(defun form-cycle-determine-matching-contexts (known-contexts)
  (save-excursion
    (block done 
      (let* ((current-context (form-cycle-gather-context)))
        (loop for c in known-contexts
              for pat = (form-cycle-context-pattern c)
              for range = (form-cycle-context-match-range c)
              for max-depth = (form-cycle-context-max-depth c)              
              when (form-cycle-match-context-pattern pat current-context range max-depth)
              collect c)))))

(defun form-cycle-process-includes (context known-contexts &optional already-included)
  (let (complete-context
        patterns-included)
    (loop for form in (form-cycle-context-forms context)
          if (and (consp form) ; (include foo)
                  (eq (first form) 'include))
          do (let ((pat (rest form)))
               (unless (or (cl-find pat already-included :test #'equal)
                           (cl-find pat patterns-included :test #'equal))                 
                 (loop for form2 in (form-cycle-context-forms
                                     (form-cycle-find-context pat))
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

(defun form-cycle-lisp-patterns (&optional lisp-forms initiate-fn)
  (interactive)
  (when (null lisp-forms)
    (setf lisp-forms form-cycle-lisp-patterns))
  (let* ((form-cycle-function 'form-cycle-with-name)
         (matching-contexts (form-cycle-determine-matching-contexts 
                             lisp-forms))
         (context (form-cycle-process-includes
                   (first matching-contexts)
                   lisp-forms))
         (form-cycle-up-list-initially-p)
         (form-cycle-up-list-initially-sexp-string)
         (form-cycle-raise-list-initially-p))

    (when (< 1 (length matching-contexts))
      (message "Matching patterns: %s"
               (mapcar #'form-cycle-context-pattern 
                       matching-contexts)))

    (form-cycle-debug (mapcar 
                       (lambda (x) (form-cycle-context-pattern x))
                       matching-contexts))
    (form-cycle-debug (form-cycle-gather-context)) 

    (loop for opt in (form-cycle-context-pattern-options context)
          do
          (case opt
            (up-list (setf form-cycle-up-list-initially-p t))))
    (if initiate-fn
        (funcall initiate-fn context)
      (form-cycle-initiate
       (form-cycle-context-forms context)))))

(defun form-cycle-lisp-patterns-ido (&optional lisp-forms)
  (interactive)    
  (form-cycle-lisp-patterns 
   lisp-forms
   (lambda (context)
     (let* ((mangled-original-pairs)
            (choice (ido-completing-read
                     "" 
                     (mapcar (lambda (string)
                               (let (mangled)
                                 (setf mangled (replace-regexp-in-string "\n" " " string)
                                       mangled (replace-regexp-in-string "  +" " " mangled))
                                 (push (cons mangled string) mangled-original-pairs)
                                 mangled))
                             (mapcar 'form-cycle-context-form-string
                                     (form-cycle-context-forms context)))))
            (form (cl-find (cdr (cl-find choice mangled-original-pairs 
                                         :test #'equal
                                         :key #'car))
                           (form-cycle-context-forms context)
                           :test #'equal
                           :key #'form-cycle-context-form-string)))
       (form-cycle-initiate (list form))))))

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
        (unless (eq (car f) 'include)
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

(defun form-cycle-context-match-range (context)
  (let ((r (form-cycle-context-pattern-assoc context 'range)))
    (if r 
        (second r)
      (if (form-cycle-context-pattern-assoc context 'immediate)
          0
        nil))))  

(defun form-cycle-context-max-depth (context)
  (let ((r (form-cycle-context-pattern-assoc context 'max-depth)))
    (if r 
        (second r)
      (if (form-cycle-context-pattern-assoc context 'toplevel)
          0
        nil)))) 
  
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
    (loop for form = (condition-case nil
                         (read (current-buffer))
                       (end-of-file (return)))
          do (push form forms))
    (setf forms (nreverse forms))
    (form-cycle-make-context 
     (nth 0 forms)
     (nthcdr 2 forms)
     (nth 1 forms))))

(defmacro form-cycle-with-context (bindings context &rest rest)
  "(form-cycle-with-context (pattern options forms) CONTEXT &rest REST)"
  (destructuring-bind (pattern options forms) bindings
    (let ((c (cl-gensym)))
      `(let* ((,c ,context)
              (,pattern (form-cycle-context-pattern ,c))
              (,options (form-cycle-context-pattern-options ,c))
              (,forms (form-cycle-context-forms ,c)))
         ,@rest))))

(defmacro form-cycle-with-form (bindings form &rest rest)
  (destructuring-bind (string options) bindings
    (let ((f (cl-gensym)))
      `(let* ((,f ,form)
              (,string (form-cycle-context-form-string ,f))
              (,options (form-cycle-context-form-options ,f)))
         ,@rest))))

(put 'form-cycle-with-context 'lisp-indent-function 2)
(put 'form-cycle-with-form 'lisp-indent-function 2)


;; Manipulation

(defmacro form-cycle-define-pattern (pattern options &rest forms)
  `(form-cycle-add-context ',pattern ',forms ',options))

(put 'form-cycle-define-pattern 'lisp-indent-function 2)

(defun form-cycle-add-context (pattern forms &optional options)
  (if (form-cycle-find-context pattern)
      (form-cycle-replace-context pattern forms options)
    (pushnew (form-cycle-make-context pattern forms options)
             form-cycle-lisp-patterns
             :test #'equal
             :key #'form-cycle-context-pattern)))

(defun form-cycle-add-context-semi-interactively (pattern forms &optional options)
  (if (form-cycle-add-context pattern forms options)
      (progn
        (message "Added.")
        t)
    (progn 
      (message "Error adding.")
      nil)))

;; Adds a form to the context.
(defun form-cycle-add (b e)
  (interactive "r")
  (let* ((context (form-cycle-gather-context))
         (pattern (read-from-minibuffer "Context: "
                                        (with-output-to-string
                                          (princ context))
                                        nil 
                                        t))
         (exist (form-cycle-find-context pattern)))
    (if exist
        (progn 
          (form-cycle-replace-context 
           pattern
           (append (list (buffer-substring-no-properties b e))
                   (form-cycle-context-forms exist))
           (form-cycle-context-pattern-options exist))
          (message "Added."))
      (form-cycle-add-context-semi-interactively 
       pattern 
       (list (buffer-substring-no-properties
              b e))))))

(defun form-cycle-edit ()
  (interactive)
  (let* ((context (form-cycle-gather-context))
         (pattern (read-from-minibuffer "Context: "
                                        (with-output-to-string
                                          (princ context))
                                        nil 
                                        t))
         (exist (form-cycle-find-context pattern)))
    ;; (unless exist
    ;;   (form-cycle-add-context pattern nil nil)
    ;;   (setf exist (form-cycle-find-context pattern)))
    (form-cycle-open-context-edit-buffer 
     (or exist (form-cycle-make-context pattern nil nil)))))

(define-minor-mode form-cycle-edit-mode "" nil " Form-Cycle-Edit"
  '(("\C-c\C-c" . form-cycle-edit-save)
    ("\C-c\C-s" . form-cycle-save)) 
  (when form-cycle-edit-mode
    t))

(defun form-cycle-edit-save ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let* ((context (form-cycle-read-context-from-buffer))
           (exist (form-cycle-find-context 
                   (form-cycle-context-pattern context))))
      (cond ((and exist 
                  (null (form-cycle-context-forms context)))
             (form-cycle-delete-context-semi-interactively 
              (form-cycle-context-pattern context)))
            
            (exist
             (form-cycle-replace-context-semi-interactively
              (form-cycle-context-pattern context)
              (form-cycle-context-forms context)
              (form-cycle-context-pattern-options context)))

            ((not exist)
             (form-cycle-add-context-semi-interactively 
              (form-cycle-context-pattern context)
              (form-cycle-context-forms context)
              (form-cycle-context-pattern-options context)))))))

(defun form-cycle-open-context-edit-buffer (context)
  (let ((buf (get-buffer-create "*Form cycle edit*")))
    (switch-to-buffer-other-window buf)
    (emacs-lisp-mode)
    (form-cycle-edit-mode 1)
    (delete-region 1 (buffer-end 1))
    (insert ";; Press C-c C-c to update this context.
;; Press C-c C-s to write all patterns to a file.
;; Valid pattern options: up-list toplevel (depth N) immediate (range N).
;; Valid form options: map-form map-string after-cycle
;; Form may also be (include . PATTERN)
;; If no forms are given, then the context is deleted.
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

(defun form-cycle-move-to-front ()
  (interactive)
  (let* ((context (form-cycle-gather-context))
         (pattern (read-from-minibuffer "Context: "
                                        (with-output-to-string
                                          (princ context))
                                        nil 
                                        t))
         (exist (form-cycle-find-context pattern)))
    (if exist
        (form-cycle-delete-context pattern)
      (error "No such pattern."))
    (form-cycle-add-context 
     (form-cycle-context-pattern exist)
     (form-cycle-context-forms exist) 
     (form-cycle-context-pattern-options exist))
    (message "Moved to front.")))

(defun form-cycle-find-context (pattern)
  (cl-find pattern 
           form-cycle-lisp-patterns
           :key #'form-cycle-context-pattern
           :test #'equal))

(defun form-cycle-context-position (pattern)
  (cl-position pattern form-cycle-lisp-patterns
               :key #'form-cycle-context-pattern
               :test #'equal))

(defun form-cycle-find-context-semi-interactively (pattern)
  (message "%s" (prin1-to-string (form-cycle-find-context pattern))))

(defun form-cycle-delete-context (pattern)
  (if (form-cycle-find-context pattern)
      (progn
        (setf form-cycle-lisp-patterns
              (cl-delete pattern form-cycle-lisp-patterns
                         :key #'form-cycle-context-pattern
                         :test #'equal))
        t)
    nil))

(defun form-cycle-delete-context-semi-interactively (pattern)
  (if (form-cycle-delete-context pattern)
      (progn (message "Deleted.")
             t)
    (progn (message "Error deleting.")
           nil)))

(defun form-cycle-delete ()
  (interactive)
  (let* ((context (form-cycle-gather-context))
         (pattern (read-from-minibuffer "Context: "
                                        (with-output-to-string
                                          (princ context))
                                        nil 
                                        t)))
    (form-cycle-delete-context-semi-interactively pattern)))

(defun form-cycle-replace-context (pattern forms &optional options)
  (let ((pos (form-cycle-context-position pattern)))
    (if pos
        (setf (nth pos form-cycle-lisp-patterns)
              (form-cycle-make-context pattern forms options))
      nil)))

(defun form-cycle-replace-context-semi-interactively (pattern forms &optional options)
  (if (form-cycle-replace-context pattern forms options)
      (message "Replaced.")
    (error "Error replacing.")))

(defun form-cycle-save ()
  (interactive)
  (let ((file (expand-file-name
               (read-file-name "Save to: " 
                               nil nil nil "form-cycle-patterns.el"))))
    (with-temp-buffer
      (dolist (context (reverse form-cycle-lisp-patterns))
        (newline)
        (form-cycle-with-context (pat opt forms) context
          (insert
           (pp-to-string
            `(form-cycle-define-pattern ,pat ,opt ,@forms)))))
      (write-file file nil))))

(defun form-cycle-load ()
  (interactive)
  (let ((file (expand-file-name
               (read-file-name "Load patterns from: " 
                               nil nil nil "form-cycle-patterns.el"))))
    (load-file file)))

(defun form-cycle-clear ()
  (interactive)
  (setf form-cycle-lisp-patterns nil)
  (message "Cleared patterns."))
  
