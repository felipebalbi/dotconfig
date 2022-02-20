;; Wifi module
(setf wifi:*iwconfig-path* "/run/current-system/profile/sbin/iwconfig")
(setf wifi:*wifi-modeline-fmt* "%e: %p")

;; Show kernel version
(defparameter *kernel-version* nil)

(defun get-kernel-version ()
  (if (null *kernel-version*)
      (let ((version (string-trim
                      '(#\newline #\linefeed #\return)
                      (run-shell-command "uname -r" t))))
        (setf *kernel-version* (format nil "Kernel:^3 ~a^n" version)))))

(defun ml-fmt-kernel-version (ml)
  (declare (ignore ml))
  *kernel-version*)

(run-with-timer 0 nil #'get-kernel-version)
(add-screen-mode-line-formatter #\K #'ml-fmt-kernel-version)

;; Set modeline format
(setf stumpwm:*screen-mode-line-format*
      (list "^5[%g]^n "                 ; Groups
            "%W"                        ; Windows
            "^>"                        ; Right Align
            "%S | "                     ; Slynk Status
            "%K | "                     ; Kernel Version
            "%C | "                     ; CPU
            "%I | "                     ; Wifi
            "%B | "                     ; Battery %
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
