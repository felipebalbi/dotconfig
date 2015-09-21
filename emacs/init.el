(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'init-packages)
(require 'init-expand-region)

;; Keep this last since it might overwrite defaults from
;; any packages initialized above.
(require 'init-customizations)
