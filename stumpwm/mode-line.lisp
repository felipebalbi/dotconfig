;; Set modeline format
(setf stumpwm:*screen-mode-line-format*
      (list "^5[%g]^n "                 ; Groups
            "%W"                        ; Windows
            "^>"                        ; Right Align
            "%S"                        ; Slynk Status
            "%B"                        ; Battery %
            "%d"                        ; time and Date
            ))

;; Mode line time format
(setf *time-modeline-string* "%H:%M")

;; Better modeline styling
(setf *mode-line-border-width* 1)
(setf *mode-line-border-color* *mode-line-border-color*)
(setf *mode-line-background-color* *mode-line-bg-color*)
(setf *mode-line-foreground-color* *mode-line-fg-color*)

;; Enable the mode line
(toggle-mode-line (current-screen) (current-head))
