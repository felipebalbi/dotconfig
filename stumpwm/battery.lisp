(in-package :fb-stumpwm)

(defparameter *battery-percent* "")

(defun get-battery-status ()
  (let* ((capacity (string-trim
                    '(#\newline #\linefeed #\return)
                    (with-open-file (s "/sys/class/power_supply/BAT0/capacity")
                      (read-line s)))))
    (setf *battery-percent* (format nil "^f1^f0^3 ~a%^n" capacity))))

(defun ml-fmt-battery-percentage (ml)
  (declare (ignore ml))
  *battery-percent*)

(run-with-timer 0 10 #'get-battery-status)
(add-screen-mode-line-formatter #\B #'ml-fmt-battery-percentage)
