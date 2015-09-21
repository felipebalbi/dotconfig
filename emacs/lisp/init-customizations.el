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
 '(linum-format "%4d │ ")
 '(magit-diff-use-overlays nil)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(ruby-indent-level 2)
 '(show-paren-mode t)
 '(tab-width 8)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :foundry "default" :family "default")))))

(pending-delete-mode t)

; add *.dts to c-mode
(add-to-list 'auto-mode-alist
	     '("\\.dts\\'" . c-mode))

; add some other ruby files to ruby-mode
(add-to-list 'auto-mode-alist
	     '("\\.\\(?:gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
(add-to-list 'auto-mode-alist
	     '("\\(Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode))

(provide 'init-customizations)
