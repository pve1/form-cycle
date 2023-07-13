(setf form-cycle-lisp-patterns nil)

;; Any context
(form-cycle-define-pattern ((*)) ()
  "(let ((_@)))" 
  "#'_"
  "(setf _@)"
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
  "(%_ :initarg :_ :initform nil)"
  "(%_ :initarg :_ :accessor _ :initform nil)"
  ("(%_ :initarg :_
    :accessor _
    :initform nil)" (after-cycle form-cycle-indent-defun))
  ("(%_ :initarg :_
    :accessor %%%-_
    :initform nil)" 
   (map-form (lambda (string) 
               (replace-regexp-in-string "%%%" 
                                         (form-cycle-toplevel-form-name)
                                         string)))
   (after-cycle form-cycle-indent-defun)))

;; Defpackage
(form-cycle-define-pattern (defpackage) ()
  ("\"_\"" (map-string upcase))
  "#:_"
  "(option)")

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
  ("(#:%%% #:_)" (map-form form-cycle-%%%-to-first-char-of-current-name))
  ("(#:%%% #:_)" (map-form form-cycle-%%%-to-first-two-chars-of-current-name))
  ("(#:%%% #:_)" (map-form (lambda (form)
                             (form-cycle-%%%-to-subseq-of-current-name form 3))))
  ("\"_\"" (map-string upcase))
  "#:_")

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
  "\"~S~%\""
  "\"~A~%\""
  "\"~{~A~^, ~}~%\""
  "\"~{~S~^, ~}~%\""
  (include (*)))

(form-cycle-define-pattern (assert) (immediate)
  "(null @_)"
  "(not (null @_))"
  (include (*)))

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
  "(for-as-hash @)"
  "(for-as-package @)")

(form-cycle-define-pattern (loop for-as-package) (up-list)
  ":for sym :being :each :symbol :in _"   
  ":for sym :being :each :present-symbol :in _" 
  ":for sym :being :each :external-symbol :in _")

(form-cycle-define-pattern (loop for-as-hash) (up-list)
  ":for key :being :each :hash-key :using (:hash-value val) :in _"
  ":for val :being :each :hash-value :using (:hash-key key) :in _")





