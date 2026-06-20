(in-package :fb-stumpwm)

;; Wifi module
;; (setf wifi:*iwconfig-path* "/run/current-system/profile/sbin/iwconfig")
(setf wifi:*wifi-modeline-fmt* " %e (%p)")

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

;; Brightness
(defparameter *brightness-level* 0)

(defun update-brightness-level ()
  (let* ((max (parse-integer (run-shell-command "brightnessctl m" t)))
         (cur (parse-integer (run-shell-command "brightnessctl g" t)))
         (level (/ (float cur) (float max)))
         (percentage (round (* 100 level))))
    (setf *brightness-level*
          (format nil "^2 ~d%^n" percentage))))

(defun ml-fmt-brightness-level (ml)
  (declare (ignore ml))
  *brightness-level*)

(run-with-timer 0 nil #'update-brightness-level)
(add-screen-mode-line-formatter #\Z #'ml-fmt-brightness-level)

;; Set group-format
(setf *group-format* "^[^(:fg \"#ff79c6\")%n%s%t^n^]")

;; Better mode line highlight template
(setf *mode-line-highlight-template* "^[^(:bg \"#44475a\")~A^(:bg 0)^]")

;; Set modeline format
(setf stumpwm:*screen-mode-line-format*
      (list "[%g] "                     ; Groups
            "%W"                        ; Windows
            "^>"                        ; Right Align
            "%S | "                     ; Slynk Status
            "%K | "                     ; Kernel Version
            "%C | "                     ; CPU
            ;; "%Z | "                     ; Brightness
            ;; "%I | "                     ; Wifi
            ;; "%B | "                     ; Battery %
            "%d"                        ; time and Date
            ))

;; Mode line time format
(setf *time-modeline-string* " %Y-%m-%d %H:%M")

;; Better modeline styling
(setf *mode-line-border-width* 1)
(setf *mode-line-border-color* *mode-line-border-color*)
(setf *mode-line-background-color* *mode-line-bg-color*)
(setf *mode-line-foreground-color* *mode-line-fg-color*)

;; Enable the mode line
(toggle-mode-line (current-screen) (current-head))
