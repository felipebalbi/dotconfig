(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'init-packages)
(require 'init-notmuch)
(require 'init-ace-link)
(require 'init-org-mode)
;(require 'init-expand-region) disabling expand region for now

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#839496"])
 '(c-basic-offset 8)
 '(c-block-comment-prefix "* ")
 '(c-default-style
   (quote
    ((c-mode . "linux")
     (awk-mode . "awk")
     (other . "linux"))))
 '(c-offsets-alist (quote ((arglist-intro . ++) (arglist-cont-nonempty . ++))))
 '(column-number-mode t)
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (wombat)))
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default)))
 '(fci-rule-color "#eee8d5")
 '(fill-column 80)
 '(global-linum-mode t)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(ido-decorations
   (quote
    ("\n-> "
     ""
     "\n   "
     "   ..."
     "["
     "]"
     " [No match]"
     " [Matched]"
     " [Not readable]"
     " [Too big]"
     " [Confirm]"
     "\n->  "
     "")))
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-ignore-buffers (quote ("\\` " "\\*")))
 '(ido-mode (quote both) nil (ido))
 '(indent-tabs-mode t)
 '(inhibit-startup-screen t)
 '(linum-format "%4d │ ")
 '(magit-diff-use-overlays nil)
 '(mail-specify-envelope-from t)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(message-kill-buffer-on-exit t)
 '(message-send-mail-function (quote message-send-mail-with-sendmail))
 '(notmuch-saved-searches
   (quote
    ((:name "unread" :query "tag:unread" :sort-order newest-first)
     (:name "flagged" :query "tag:flagged" :key "f" :sort-order newest-first)
     (:name "sent" :query "tag:sent" :key "t" :sort-order newest-first)
     (:name "drafts" :query "tag:draft" :key "d" :sort-order newest-first)
     (:name "linux-usb" :query "tag:linux-usb and tag:unread" :key "u" :sort-order newest-first)
     (:name "lakml" :query "tag:lakml and tag:unread" :sort-order newest-first)
     (:name "linux-arch" :query "tag:linux-arch and tag:unread" :sort-order newest-first)
     (:name "linux-embedded" :query "tag:linux-embedded and tag:unread" :sort-order newest-first)
     (:name "linux-nfc" :query "tag:linux-nfc and tag:unread" :sort-order newest-first)
     (:name "linux-omap" :query "tag:linux-omap and tag:unread" :key "o" :sort-order newest-first)
     (:name "lkml" :query "tag:lkml and tag:unread" :sort-order newest-first)
     (:name "u-boot" :query "tag:u-boot and tag:unread" :sort-order newest-first)
     (:name "openocd" :query "tag:openocd and tag:unread" :sort-order newest-first)
     (:name "linux-patch-review" :query "tag:linux-patch-review and tag:unread" :sort-order newest-first)
     (:name "lcpd" :query "tag:lcpd and tag:unread" :sort-order newest-first)
     (:name "lcpd-lt" :query "tag:lcpd-lt and tag:unread" :sort-order newest-first)
     (:name "lcpd-connectivity" :query "tag:lcpd-connectivity and tag:unread" :sort-order newest-first)
     (:name "usb-if" :query "tag:usb-if and tag:unread" :sort-order newest-first)
     (:name "mipi" :query "tag:mipi and tag:unread" :sort-order newest-first)
     (:name "work-stuff" :query "from:*@ti.com and not (tag:lakml or tag:linux-arch or tag:linux-embedded or tag:linux-nfc or tag:linux-omap or tag:linux-usb or tag:lkml or tag:u-boot or tag:openocd or tag:linux-patch-review or tag:lcpd or tag:lcpd-lt or tag:lcpd-connectivity or tag:usb-if or tag:mipi) and tag:unread" :key "w" :sort-order newest-first)
     (:name "TODO" :query "tag:todo and tag:unread" :key "t" :sort-order newest-first)
     (:name "important" :query "tag:important" :sort-order newest-first)
     (:name "inbox+unread" :query "tag:inbox and tag:unread" :key "i" :sort-order newest-first)
     (:name "linux-usb-to-me" :query "(to:balbi@ti.com or cc:balbi@ti.com) and tag:linux-usb and tag:unread" :key "m" :sort-order newest-first))))
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(ruby-indent-level 2)
 '(sendmail-program "/usr/bin/msmtp")
 '(show-paren-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(tab-width 8)
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(tool-bar-mode nil)
 '(user-full-name "Felipe Balbi")
 '(user-mail-address "balbi@ti.com")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#e47200")
     (60 . "#e4ab00")
     (80 . "#b58900")
     (100 . "#e4e400")
     (120 . "#e4e400")
     (140 . "#e4e400")
     (160 . "#e4e400")
     (180 . "#859900")
     (200 . "#98e44c")
     (220 . "#72e472")
     (240 . "#4ce498")
     (260 . "#26e4be")
     (280 . "#2aa198")
     (300 . "#00e4e4")
     (320 . "#00e4e4")
     (340 . "#00e4e4")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :foundry "default" :family "default")))))

;; Sign messages by default.
(add-hook 'message-setup-hook 'mml-secure-sign-pgpmime)

;; Set some environment variables
(setenv "ARCH" "arm")
(setenv "CROSS_COMPILE" "ccache arch-arm-gnueabihf-")
