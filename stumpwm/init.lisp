(in-package :stumpwm)

;; Change default prefix key
(set-prefix-key (kbd "s-x"))

;; Create 4 workspaces. First one selected by default
(grename "dev")
(gnewbg "web")
(gnewbg "chat")
(gnewbg "scratch")

;; Dracula Colors
(setf *colors*
      '("#282a36"                    ; Background
        "#ff5555"                    ; Red
        "#50fa7b"                    ; Green
        "#f1fa8c"                    ; Yellow
        "#6272a4"                    ; Comment
        "#ff79c6"                    ; Pink
        "#8be9fd"                    ; Cyan
        "#f8f8f2"                    ; Foreground
        "#bd93f9"                    ; Purple
        "#44475a"                    ; Current Line
        "#ffb86c"                    ; Orange
        ))

(update-color-map (current-screen))

(setf *window-format* "%m%s%50t")

(defparameter *mode-line-bg-color* (nth 0 *colors*))
(defparameter *mode-line-fg-color* (nth 7 *colors*))
(defparameter *mode-line-border-color* (nth 8 *colors*))

(defparameter *msg-bg-color* (nth 0 *colors*))
(defparameter *msg-fg-color* (nth 7 *colors*))
(defparameter *msg-border-color* (nth 8 *colors*))

;; Switch to regular mouse pointer
(run-shell-command "xsetroot -cursor_name left_ptr")

;; Display messages for 3 seconds
(setf *timeout-wait* 3)

;; Mouse focus policy: focus follows the mouse
(setf *mouse-focus-policy* :sloppy)

;; Gravity settings
(setf *message-window-gravity* :center)
(setf *input-window-gravity* :center)

(set-bg-color *msg-bg-color*)
(set-fg-color *msg-fg-color*)
(set-border-color *msg-border-color*)

;; Message and Input bar settings
(set-msg-border-width 3)
(setf *message-window-padding* 5)

;; Slynk Settings
(require :slynk)

(defparameter *port-number* 4040
  "My preferred slynk port number")

(defvar *slynk-server-p* nil
  "Keep track of state of slynk server")

(defcommand start-slynk () ()
  "Start Slynk if not already running"
  (if *slynk-server-p*
      (message "Slynk server is already active on Port^5 ~a^n" *port-number*)
      (progn
        (slynk:create-server :port *port-number*)
        (setf *slynk-server-p* t)
        (message "Slynk server is now active on Port^5 ~a^n.
Use^5 M-x sly-connect^n in emacs.
Type^2 (in-package :stumpwm)^n in Sly REPL." *port-number*))))

(defcommand stop-slynk () ()
  "Stop Slynk server"
  (slynk:stop-server *port-number*)
  (setf *slynk-server-p* nil)
  (message "Stopping Slynk serve! Closing Port^5 ~a^n." *port-number*))

(defcommand toggle-slynk () ()
  (if *slynk-server-p*
      (run-commands "stop-slynk")
      (run-commands "start-slynk")))

(define-key *top-map* (kbd "s-s") "toggle-slynk")

(defun get-slynk-status ()
  (if *slynk-server-p*
      (setf *slynk-ml-status* (format nil "Slynk Port:^5 ~a^n " *port-number*))
      (setf *slynk-ml-status* "")))

(defun ml-fmt-slynk-status (ml)
  (declare (ignore ml))
  (get-slynk-status))

(add-screen-mode-line-formatter #\S #'ml-fmt-slynk-status)

(defparameter *battery-percent* "")

(defun get-battery-status ()
  (let* ((capacity (string-trim
                    '(#\newline #\linefeed #\return)
                    (with-open-file (s "/sys/class/power_supply/BAT0/capacity")
                      (read-line s)))))
    (setf *battery-percent* (format nil "^2 ~a%^n " capacity))))

(defun ml-fmt-battery-percentage (ml)
  (declare (ignore ml))
  *battery-percent*)

(run-with-timer 0 10 #'get-battery-status)
(add-screen-mode-line-formatter #\B #'ml-fmt-battery-percentage)

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
