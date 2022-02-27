(in-package :fb-stumpwm)

(require :ttf-fonts)

(setf xft:*font-dirs* '("/run/current-system/profile/share/fonts/"))
(setf clx-truetype:+font-cache-filename+ (concat (getenv "HOME")
                                                 "/.fonts/font-cache.sexp"))
(xft:cache-fonts)

(set-font (list
           (make-instance 'xft:font :family "Fira Code"
                                    :subfamily "Regular" :size 11)
           (make-instance 'xft:font :family "Material Icons"
                                    :subfamily "Regular" :size 11)
           (make-instance 'xft:font :family "Noto Emoji"
                                    :subfamily "Regular" :size 11)))
