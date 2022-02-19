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
  (message "Stopping Slynk server! Closing Port^5 ~a^n." *port-number*))

(defcommand toggle-slynk () ()
  (if *slynk-server-p*
      (run-commands "stop-slynk")
      (run-commands "start-slynk")))

(define-key *top-map* (kbd "s-s") "toggle-slynk")

(defun get-slynk-status ()
  (if *slynk-server-p*
      (setf *slynk-ml-status* (format nil "Slynk:^5 ~a^n " *port-number*))
      (setf *slynk-ml-status* "")))

(defun ml-fmt-slynk-status (ml)
  (declare (ignore ml))
  (get-slynk-status))

(add-screen-mode-line-formatter #\S #'ml-fmt-slynk-status)
