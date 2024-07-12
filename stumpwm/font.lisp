(in-package :fb-stumpwm)

(ql:quickload :clx-truetype)
(load-module "ttf-fonts")
(xft:cache-fonts)

;; (require :ttf-fonts)
;;
;; (setf xft:*font-dirs* '("/run/current-system/profile/share/fonts/"))
;; (setf clx-truetype:+font-cache-filename+ (concat (getenv "HOME")
;;                                                  "/.fonts/font-cache.sexp"))
;; (xft:cache-fonts)

(set-font (list
           ;; "-*-fira code-medium-r-normal-*-*-120-*-*-*-*-*"
           ;; "-*-fira code-bold-r-normal-*-*-120-*-*-*-*-*"
           ;; "-*-noto emoji-medium-r-normal-*-*-120-*-*-*-*-*"
           (make-instance 'xft:font :family "Fira Code"
                                    :subfamily "Regular" :size 11)
           (make-instance 'xft:font :family "Material Icons"
                                    :subfamily "Regular" :size 11)
           (make-instance 'xft:font :family "Fira Code"
                                    :subfamily "Bold" :size 11)
           (make-instance 'xft:font :family "Noto Emoji"
                                    :subfamily "Regular" :size 11)
           ))
