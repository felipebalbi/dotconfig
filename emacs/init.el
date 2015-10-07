(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'init-packages)
(require 'init-notmuch)
;(require 'init-expand-region) disabling expand region for now

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 8)
 '(c-default-style
   (quote
    ((c-mode . "linux")
     (awk-mode . "awk")
     (other . "linux"))))
 '(c-offsets-alist (quote ((arglist-intro . ++) (arglist-cont-nonempty . ++))))
 '(column-number-mode t)
 '(compilation-message-face (quote default))
 '(custom-enabled-themes (quote (wombat)))
 '(fill-column 80)
 '(global-linum-mode t)
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
     (:name "all mail" :query "*" :key "a" :sort-order newest-first)
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
     (:name "inbox+unread" :query "tag:inbox and tag:unread" :key "i" :sort-order newest-first))))
 '(ruby-indent-level 2)
 '(sendmail-program "/usr/bin/msmtp")
 '(show-paren-mode t)
 '(tab-width 8)
 '(tool-bar-mode nil)
 '(user-full-name "Felipe Balbi")
 '(user-mail-address "balbi@ti.com"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :foundry "default" :family "default")))))

;; Sign messages by default.
(add-hook 'message-setup-hook 'mml-secure-sign-pgpmime)
