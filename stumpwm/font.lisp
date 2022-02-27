(in-package :fb-stumpwm)

(require :ttf-fonts)

(setf xft:*font-dirs* '("/run/current-system/profile/share/fonts/"))
(setf clx-truetype:+font-cache-filename+ (concat (getenv "HOME")
                                                 "/.fonts/font-cache.sexp"))
(xft:cache-fonts)

(set-font (make-instance 'xft:font :family "Fira Code"
                                   :subfamily "Regular" :size 11))
