;; form-cycle.el demo

(load-file "form-cycle.el")

(define-key lisp-mode-map "\M-e" 'form-cycle-lisp-patterns)
(define-key lisp-mode-map "\M-E" 'form-cycle-lisp-patterns-ido)

(define-key slime-repl-mode-map "\M-e" 'form-cycle-lisp-patterns)
(define-key slime-repl-mode-map "\M-E" 'form-cycle-lisp-patterns-ido)

(load-file "example-lisp-patterns.el")

(find-file-other-window "demo.lisp")
