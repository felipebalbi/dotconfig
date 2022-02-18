;; Switch to regular mouse pointer
(run-shell-command "xsetroot -cursor_name left_ptr")

;; Display messages for 3 seconds
(setf *timeout-wait* 3)

;; Mouse focus policy: focus follows the mouse
(setf *mouse-focus-policy* :sloppy)

;; Gravity settings
(setf *message-window-gravity* :center)
(setf *input-window-gravity* :center)
