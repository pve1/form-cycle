;; -*- lexical-binding: t -*-

(defvar pve-cycle-current-cycle-state nil)
(defvar pve-cycle-position nil)
(defvar pve-cycle-new-cycle-p nil)
(defvar pve-cycle-function 'pve-cycle-default-cycle-function)

(setq pve-cycle-current-cycle-state nil)
(setq pve-cycle-position nil)
(setq pve-cycle-new-cycle-p nil)

(defun pve-cycle-default-cycle-function (form)
  (unless (pve-cycle-new-cycle-p)
    (save-excursion
      (when (sexp-at-point)
        (kill-sexp))))
  (save-excursion
    (insert form)))
                            
(defun pve-cycle-next ()
  (let ((next (pop pve-cycle-current-cycle-state)))
    (when next
      (setf pve-cycle-current-cycle-state
            (append pve-cycle-current-cycle-state (list next))))
    (funcall pve-cycle-function next) 
    (setf pve-cycle-position (point))))

(defun pve-cycle-new-cycle-p ()
  (or (null pve-cycle-position)
      (and pve-cycle-position
           (not (= pve-cycle-position (point)))))) ; Point has moved

(defun pve-at-beginning-of-symbol-p ()
  (looking-at "\\_<"))

(defun pve-cycle-at-toplevel-p ()
  (save-excursion
    (let ((top t))
      (ignore-errors (up-list -1)
                     (setf top nil))
      top)))  

(defun pve-surrounding-sexp-car ()
  (save-excursion
    (ignore-errors
      (up-list -1)
      (when (looking-at "( *\\_<")      ; list with symbol at car
        (search-forward-regexp "\\_<")
        (symbol-at-point)))))

(defun pve-cycle-determine-context (known-forms)
  (save-excursion
    (block done 
      (let ((top-level t))
        (loop until (pve-cycle-at-toplevel-p)
              for car = (pve-surrounding-sexp-car)
              do 
              (setf top-level nil)
              (if (and car (find car known-forms))
                  (return-from done car)
                (up-list -1)))
        top-level))))

(defun pve-cycle-initiate (cycle)
  (if (pve-cycle-new-cycle-p)
      (let ((pve-cycle-new-cycle-p t))
        (setq pve-cycle-current-cycle-state cycle)
        (pve-cycle-next))
    (pve-cycle-next)))

;; Lisp forms

(defvar pve-cycle-form-beginning nil)
(defvar pve-cycle-lisp-forms-name-marker "_")
(defvar pve-cycle-lisp-forms-point-marker "@")
(defvar pve-cycle-lisp-forms
  '((t ("(defun _ ()
  @)"
        "(defmethod _ ()
  @)"
        "(defgeneric _ ()
  @)"
        "(defclass _ ()
  @)"
        "(defvar *_* @)"
        "(defparameter *_* @)"))

    ;; Not toplevel
    (nil ("(let ((_ @))
    )"
          "(let* ((_ @))
    )"))))

;; Point should be at the beginning of the form that was previously
;; inserted.
(defun pve-cycle-lisp-forms-insert-form (form name &optional delete-previous)
  (let ((point (point))
        (place-point)
        (string))
    (when delete-previous
      (kill-sexp))
  (with-temp-buffer 
    (insert form)
    (beginning-of-buffer)
    ;; _ -> name
    (replace-string pve-cycle-lisp-forms-name-marker name)
    (beginning-of-buffer)
    ;; Figure out where to place point
    (when (search-forward pve-cycle-lisp-forms-point-marker nil t)
      (delete-backward-char (length pve-cycle-lisp-forms-point-marker))
      (setf place-point (1- (point))))
    (setf string (buffer-substring-no-properties 1 (buffer-end 1))))
  (save-excursion
    (insert string)) 
  (when place-point
    (forward-char place-point))))

(defun pve-cycle-lisp-forms-cycle-function (form)
  (if pve-cycle-new-cycle-p
      (let* ((current-symbol (symbol-at-point))
             (symbol-name (if current-symbol
                              (symbol-name current-symbol)
                            "")))
        ;; Move to beginning of symbol if necessary.
        (when (and current-symbol 
                   (not (pve-at-beginning-of-symbol-p)))
          (beginning-of-sexp))
        (setf pve-cycle-form-beginning (point))
        ;; Remember symbol name.
        (setf pve-cycle-lisp-forms-current-name symbol-name)
        (pve-cycle-lisp-forms-insert-form
         form 
         symbol-name
         current-symbol))
    (progn 
      (goto-char pve-cycle-form-beginning)
      (pve-cycle-lisp-forms-insert-form
       form 
       pve-cycle-lisp-forms-current-name
       t))))

(defun pve-cycle-lisp-forms ()
  (interactive)
  (let* ((pve-cycle-function 'pve-cycle-lisp-forms-cycle-function)
         (context (pve-cycle-determine-context
                   (mapcar #'first pve-cycle-lisp-forms)))
         ;; Lookup correct form list based on context.
         (forms (second (assoc context pve-cycle-lisp-forms))))
    (pve-cycle-initiate forms)))
