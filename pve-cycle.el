;; -*- lexical-binding: t -*-

(defvar pve-cycle-current-cycle-state nil)
(defvar pve-cycle-position nil)
(defvar pve-cycle-initial-position nil)
(defvar pve-cycle-new-cycle-p nil)
(defvar pve-cycle-undo-previous-function nil)
(defvar pve-cycle-function 'pve-cycle-default-cycle-function)

(setf pve-cycle-debug t)

(defmacro pve-cycle-debug (thing)
  `(when pve-cycle-debug
     (print ,thing)))

;; Basic functionality: Inserting strings in a cycle.

(defun pve-cycle-default-cycle-function (form)
  (unless pve-cycle-undo-previous-function
    (setf pve-cycle-undo-previous-function
          (lambda ()
            (when pve-cycle-initial-position
              (goto-char pve-cycle-initial-position))
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

;; Point should be at the beginning of the form that was previously
;; inserted.
(defun pve-cycle-with-name (form)
  ;; Initialize
  (when pve-cycle-new-cycle-p
    (let ((sym (symbol-at-point)))
      (if sym
          (progn
            (unless (looking-at "\\_<")
              (beginning-of-sexp))
            (kill-sexp)
            (setf pve-cycle-current-name (symbol-name sym)
                  pve-cycle-initial-position (point)))
        (setf pve-cycle-current-name "")))) 
  
  (let ((place-point)
        (string)
        (options))
    (when (consp form)
      (setf options (rest form)
            form (car form)))
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
    (when (getf options 'map-string)
      (setf string (funcall (getf options 'map-string) string)))
    (pve-cycle-default-cycle-function string)
    (when place-point
      (goto-char pve-cycle-initial-position)
      (forward-char place-point)
      (when (getf options 'after-place-point)
        (save-excursion
          (funcall (getf options 'after-place-point)))))
    (when (getf options 'after-cycle)
      (save-excursion
        (funcall (getf options 'after-cycle))))
    string))

(defun pve-cycle-test-with-name ()
  (interactive)
  (let ((pve-cycle-function 'pve-cycle-with-name))
    (pve-cycle-initiate '("(defun _ () @)"
                          "(defclass _ () (@))"))))

;; Context aware lisp forms

(defvar pve-cycle-lisp-forms
  '((cycle-toplevel
     "(defun _ ()\n  @)"
     "(defmethod _ ()\n  @)"
     "(defclass _ ()\n  (@))"
     "(defgeneric _ ())")

    (defclass
      "%_"
      "(%_ :initarg :_ :initform nil)"
      "(%_ :initarg :_ :accessor _ :initform nil)"
      ("(%_ :initarg :_
    :accessor _
    :initform nil)" after-cycle (lambda () 
                                  (beginning-of-defun)
                                  (indent-pp-sexp))))

    ((defpackage :export)
     "#:_"
     ("\"_\"" map-string upcase))
 
    ;; Always matches.
    (nil "(let ((_ @))\n    )"
         "(let* ((_ @))\n    )")))

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
    (nreverse (loop until (pve-cycle-at-toplevel-p)
                    for car = (pve-surrounding-sexp-car)
                    collect car
                    do (up-list -1)))))

(defun pve-cycle-determine-context (known-contexts)
  (save-excursion
    (block done 
      (let* ((top-level t)
             (current-context (or (pve-cycle-gather-context)
                                  '(cycle-toplevel)))
             (matched-context
              (loop for c in known-contexts
                    for pat = (cond ((null (car c))
                                     nil)
                                    ((symbolp (car c))
                                     (list (car c)))
                                    (t (car c)))                             
                    when (pve-cycle-match-context-pattern pat current-context)
                    return (rest c))))
        matched-context))))
        
(defun pve-cycle-test-lisp-forms ()
  (interactive)
  (let ((pve-cycle-function 'pve-cycle-with-name)
        (context (pve-cycle-determine-context
                  pve-cycle-lisp-forms)))
    (pve-cycle-debug context)
    (pve-cycle-initiate context)))


