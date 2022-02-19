(defparameter *battery-percent* "")

(defun get-battery-status ()
  (let* ((capacity (string-trim
                    '(#\newline #\linefeed #\return)
                    (with-open-file (s "/sys/class/power_supply/BAT0/capacity")
                      (read-line s)))))
    (setf *battery-percent* (format nil "BAT:^5 ~a%^n" capacity))))

(defun ml-fmt-battery-percentage (ml)
  (declare (ignore ml))
  *battery-percent*)

(run-with-timer 0 10 #'get-battery-status)
(add-screen-mode-line-formatter #\B #'ml-fmt-battery-percentage)
