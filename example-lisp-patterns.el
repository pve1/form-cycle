(setf form-cycle-lisp-patterns nil)

;; Any context
(form-cycle-define-pattern ((*)) ()
  ("#'_" (map-form (lambda (f)
                     (if (equal "" form-cycle-current-name) 
                         (form-cycle-skip)
                       f))))
  "(setf _@)"
  "(let ((_@)))"
  "(lambda (_)@)"
  "(make-instance '_@)"
  "(declare (ignore _))"
  ("(return-from %%% @_)"        
   (map-form (lambda (string) 
               (let ((name (form-cycle-toplevel-form-name)))
                 (if name
                     (replace-regexp-in-string "%%%" name string)
                   string))))))

;; Toplevel
(form-cycle-define-pattern () (toplevel)
  "(defun _ (@))" 
  "(defmethod _ (@))" 
  "(defgeneric _ (@))"
  "(defclass _ ()\n  (@))"
  "(defvar _@)"
  "(defparameter _@)"
  ("(defmethod initialize-instance :after ((%%% _) &key)@)"
   (map-form form-cycle-%%%-to-first-char-of-current-name))
  ("(defmethod print-object ((%%% _) stream)
  (print-unreadable-object (%%% stream :type t)
    (format stream@)))"
   (map-form form-cycle-%%%-to-first-char-of-current-name))
  "(defpackage #:_
  (:use #:cl)
  (:local-nicknames)
  (:export))\n\n(in-package #:_)")

;; Defclass
(form-cycle-define-pattern (defclass) (immediate)
  "(:metaclass@)"
  "(:default-initargs@)"
  "(:documentation \"@\")")

(form-cycle-define-pattern (defclass (*)) ()
  ("(%_ :initarg :_
    :accessor _
    :initform nil)" (after-cycle form-cycle-indent-defun))
  ("(%_ :initarg :_
    :reader _
    :initform nil)" (after-cycle form-cycle-indent-defun))
  ("(%_ :initarg :_
    :accessor %%%-_
    :initform nil)" 
   (map-form (lambda (string) 
               (replace-regexp-in-string "%%%" 
                                         (form-cycle-toplevel-form-name)
                                         string)))
   (after-cycle form-cycle-indent-defun))
  ("(%_ :initarg :_
    :reader %%%-_
    :initform nil)" 
   (map-form (lambda (string) 
               (replace-regexp-in-string "%%%" 
                                         (form-cycle-toplevel-form-name)
                                         string)))
   (after-cycle form-cycle-indent-defun)))

;; Defpackage
(form-cycle-define-pattern (defpackage) ()
  ("(option)" (map-form form-cycle-require-position >= 2))
  ("\"_\"" (map-string upcase))
  "#:_")

(form-cycle-define-pattern (defpackage (*)) ()
  ("\"_\"" (map-string upcase))
  "#:_")

(form-cycle-define-pattern (defpackage option) (up-list)
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

(form-cycle-define-pattern (defpackage :local-nicknames) ()
  ("\"_\"" (map-string upcase))
  "#:_"
  ("(#:%%% #:_)" (map-form form-cycle-%%%-to-first-char-of-current-name))
  ("(#:%%% #:_)" (map-form form-cycle-%%%-to-first-two-chars-of-current-name))
  ("(#:%%% #:_)" (map-form form-cycle-%%%-to-subseq-of-current-name 3)))

;; Various

(form-cycle-define-pattern (defgeneric) (immediate)
  "(:method (@)
    )"
  ("(:method %%%@
    )" (map-form (lambda (form)
                   (let ((defgeneric-args (form-cycle-read-nth-toplevel-form 2)))
                     (replace-regexp-in-string "%%%" 
                                               (prin1-to-string
                                                (mapcar (lambda (x)
                                                          (list x t)) 
                                                        defgeneric-args))
                                               form)))))
  "(:generic-function-class@)"
  "(:documentation \"@\")")

(form-cycle-define-pattern (format) (immediate)
  ("(format-recipe)" (map-form form-cycle-require-position = 2))
  (include (*)))

(form-cycle-define-pattern (format-recipe) (immediate up-list)
  "\"~S~%\"" 
  "\"~A~%\"" 
  "\"~{~A~^, ~}~%\""
  "\"~{~S~^, ~}~%\"")

(form-cycle-define-pattern (assert) (immediate)
  "(null @_)"
  "(not (null @_))"
  (include (*)))

(form-cycle-define-pattern (make-hash-table) (immediate up-list)
  "(make-hash-table :test 'eq)"
  "(make-hash-table :test 'equal)")

(form-cycle-define-pattern (asdf:defsystem) ()
  "(:file \"_\")"
  "(:module \"_\" :components (@))")

(form-cycle-define-pattern (λ) (immediate up-list)
  "(lambda ()@)"
  "(lambda (x)@)"
  "(lambda (x y)@)"
  "(lambda (x y z)@)"
  "(lambda (&key)@)"
  "(lambda (&rest rest)@)")

(form-cycle-define-pattern (loop) ()
  "(tricky-loop-clause)"
  (include (*)))

(form-cycle-define-pattern (loop tricky-loop-clause) (up-list)
  "(for-as-hash  @)"
  "(for-as-package  @)"
  "(for-as-on-list  @)"
  "(for-as-equals-then  @)"
  "(for-as-arithmetic  @")

(form-cycle-define-pattern (loop for-as-package) (up-list)
  ":for sym :being :each :symbol :in _"   
  ":for sym :being :each :present-symbol :in _" 
  ":for sym :being :each :external-symbol :in _")

(form-cycle-define-pattern (loop for-as-hash) (up-list)
  ":for _ :being :each :hash-key :using (:hash-value val) :in @"
  ":for _ :being :each :hash-value :using (:hash-key key) :in @")

(form-cycle-define-pattern (loop for-as-arithmetic) (up-list)
  ":for k :from 0 :by 1"   
  ":for k :from 0 :to _@ :by 1" 
  ":for k :from 0 :below _@ :by 1"
  ":for k :from _@ :downto 0 :by 1"
  ":for k :from _@ :above 0 :by 1")

(form-cycle-define-pattern (loop for-as-on-list) (up-list)
  (":for tail :on _@ :by #'cdr
:for head = (car tail)" 
   (after-cycle form-cycle-indent-defun))
  ":for (a b) :on _@ :by #'cddr")

(form-cycle-define-pattern (loop for-as-equals-then) (up-list)
  (":for line = (read-line _ nil _)
:until (eql line _)" (after-cycle form-cycle-indent-defun))
  (":for form = (read _ nil _)
:until (eql form _)" (after-cycle form-cycle-indent-defun)))








