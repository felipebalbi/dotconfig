(in-package #:nyxt-user)

(dolist (file (list (nyxt-init-file "keybinds.lisp")
                    (nyxt-init-file "status.lisp")
                    (nyxt-init-file "style.lisp")))
  (load file))

(load "~/quicklisp/setup.lisp")
(ql:quickload :slynk)

;; (load-after-system :nx-kaomoji (nyxt-init-file "kaomoji.lisp"))
(load-after-system :slynk (nyxt-init-file "slynk.lisp"))

(define-configuration browser
    ((session-restore-prompt :never-restore)))

(define-configuration (buffer internal-buffer editor-buffer prompt-buffer)
    ((default-modes `(emacs-mode ,@%slot-default%))
     (download-engine :renderer)
     (current-zoom-ratio 1.25)))

(define-configuration browser
  ((external-editor-program '("emacsclient"))))

(define-configuration (web-buffer nosave-buffer)
  ((default-modes `(emacs-mode
                    blocker-mode force-https-mode reduce-tracking-mode
                    auto-mode
                    ,@%slot-default%))))

(define-configuration prompt-buffer
    ((hide-single-source-header-p t)))

(define-configuration nosave-buffer
    ((default-modes `(proxy-mode ,@%slot-default%))))

(define-configuration nyxt/web-mode:web-mode
    ;; QWERTY home row.
    ((nyxt/web-mode:hints-alphabet "FJDKSLA;GH")
     (glyph "ω")))

(define-configuration nyxt/auto-mode:auto-mode
    ((nyxt/auto-mode:prompt-on-mode-toggle t)
     (glyph "α")))

(define-command-global eval-expression ()
  "Prompt for the expression and evaluate it, echoing result to the `message-area'."
  (let ((expression-string
          ;; Read an arbitrary expression. No error checking, though.
          (first (prompt :prompt "Expression to evaluate"
                         :sources (list (make-instance 'prompter:raw-source))))))
    ;; Message the evaluation result to the message-area down below.
    (echo "~S" (eval (read-from-string expression-string)))))
