(defcommand my/terminal (&optional prg) ()
  (run-shell-command
   (if prg
       (format nil "st -e ~a" prg)
       "st")))

(define-key *top-map* (kbd "s-RET") "my/terminal")
