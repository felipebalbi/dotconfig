;; Change default prefix key
(set-prefix-key (kbd "s-x"))

(defun define-keys (map alist)
  (loop for (key . cmd) in alist
        do (define-key map (kbd key) cmd)))

(defparameter *fb/top-map*
  '(("s-RET" . "exec st")

    ("s-b"   . "move-focus left")
    ("s-f"   . "move-focus right")
    ("s-p"   . "move-focus up")
    ("s-n"   . "move-focus down")

    ("s-B"   . "move-window left")
    ("s-F"   . "move-window right")
    ("s-P"   . "move-window up")
    ("s-N"   . "move-window down")

    ("s-h"   . "hsplit")
    ("s-v"   . "vsplit")
    ("s-C-F" . "fullscreen")

    ("s-r"   . "resize")

    ("s-1"   . "gselect 1")
    ("s-2"   . "gselect 2")
    ("s-3"   . "gselect 3")
    ("s-4"   . "gselect 4")
    ("s-5"   . "gselect 5")
    ("s-6"   . "gselect 6")
    ("s-7"   . "gselect 7")
    ("s-8"   . "gselect 8")
    ("s-9"   . "gselect 9")
    ("s-0"   . "gselect 10")

    ("s-e"   . "exec emacs")
    ("s-B"   . "exec nyxt")

    ("s-!"   . "exec")
    ))

(define-keys *top-map* *fb/top-map*)

(defparameter *fb/root-map*
  '(("3" . "hsplit")
    ("2" . "vsplit")
    ("1" . "only")
    ("n" . "pull-hidden-next")
    ("p" . "pull-hidden-previous")
    ))

(define-keys *root-map* *fb/root-map*)
