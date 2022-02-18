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

(set-bg-color *msg-bg-color*)
(set-fg-color *msg-fg-color*)
(set-border-color *msg-border-color*)

;; Message and Input bar settings
(set-msg-border-width 3)
(setf *message-window-padding* 5)

(setf *window-border-style* :thin)
(setf *normal-border-width* 1)
