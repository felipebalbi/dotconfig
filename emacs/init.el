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
    ((:name "inbox" :query "tag:inbox" :key "i" :sort-order newest-first)
     (:name "unread" :query "tag:unread" :key "u" :sort-order newest-first)
     (:name "flagged" :query "tag:flagged" :key "f" :sort-order newest-first)
     (:name "sent" :query "tag:sent" :key "t" :sort-order newest-first)
     (:name "drafts" :query "tag:draft" :key "d" :sort-order newest-first)
     (:name "all mail" :query "*" :key "a" :sort-order newest-first)
     (:name "linux-usb" :query "(to:linux-usb@vger.kernel.org or cc:linux-usb@vger.kernel.org) and tag:unread" :sort-order newest-first)
     (:name "linux-omap" :query "(to:linux-omap@vger.kernel.org or cc:linux-omap@vger.kernel.org) and tag:unread" :sort-order newest-first)
     (:name "u-boot" :query "(to:u-boot@lists.denx.de or cc:u-boot@lists.denx.de) and tag:unread" :sort-order newest-first)
     (:name "lkml" :query "(to:linux-kernel@vger.kernel.org or cc:linux-kernel@vger.kernel.org) and tag:unread")
     (:name "to-me" :query "(to:balbi@ti.com or to:balbi@kernel.org) and tag:unread"))))
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
