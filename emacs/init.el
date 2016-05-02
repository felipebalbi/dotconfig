; Package initialization
(package-initialize)
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

; ORG Mode setup
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-capture-templates
      '(("t"
	 "TODO item"
	 entry
	 (file+headline "~/workspace/org/todo.org" "TODO")
	 "* TODO %?\n   %i\n   %a")
      ("s"
	 "Status Report"
	 entry
	 (file+datetree "~/workspace/org/status.org" "MSR")
	 "* %T\n* What was done?%?\n* Why this was done?\n* What is the impact to Intel?\n")
	))

;; Sign email messages by default.
(add-hook 'message-setup-hook 'mml-secure-message-sign-pgpmime)

;; Parens modes for emacs
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(show-paren-mode 1)
(electric-pair-mode 1)

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
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default)))
 '(diary-file "~/workspace/org/diary")
 '(fci-rule-color "#eee8d5")
 '(fill-column 80)
 '(global-linum-mode t)
 '(global-prettify-symbols-mode t)
 '(grep-command "git --no-pager grep -e ")
 '(grep-find-command
   (quote
    ("find . -type f -exec grep --color -nH -e  {} +" . 42)))
 '(grep-find-template "find <D> <X> -type f <F> -exec grep <C> -nH -e <R> {} +")
 '(grep-highlight-matches (quote auto))
 '(grep-template "grep <X> <C> -nH -e <R> <F>")
 '(grep-use-null-device nil)
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
 '(indent-tabs-mode t)
 '(inhibit-startup-screen t)
 '(linum-format "%4d ")
 '(magit-diff-use-overlays nil)
 '(mail-specify-envelope-from t)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(message-kill-buffer-on-exit t)
 '(message-send-mail-function (quote message-send-mail-with-sendmail))
 '(normal-erase-is-backspace 0)
 '(notmuch-saved-searches
   (quote
    ((:name "unread" :query "tag:unread" :sort-order newest-first)
     (:name "sent" :query "folder:sent and tag:unread" :key "t" :sort-order newest-first)
     (:name "drafts" :query "tag:draft" :key "d" :sort-order newest-first)
     (:name "linux-usb" :query "tag:linux-usb and tag:unread" :key "u" :sort-order newest-first)
     (:name "linux-arch" :query "tag:linux-arch and tag:unread" :sort-order newest-first)
     (:name "linux-embedded" :query "tag:linux-embedded and tag:unread" :sort-order newest-first)
     (:name "lkml" :query "tag:lkml and tag:unread" :sort-order newest-first)
     (:name "openocd" :query "tag:openocd and tag:unread" :sort-order newest-first)
     (:name "fixes" :query "tag:fixes and tag:unread" :key "f" :sort-order newest-first)
     (:name "important" :query "tag:important" :sort-order newest-first)
     (:name "inbox+unread" :query "tag:inbox and tag:unread" :key "i" :sort-order newest-first)
     (:name "linux-usb-to-me" :query "(to:balbi@kernel.org or cc:balbi@kernel.org or to:felipe.balbi@linux.intel.com or cc:felipe.balbi@linux.intel.com) and tag:linux-usb and tag:unread" :key "m" :sort-order newest-first)
     (:name "next" :query "tag:next and tag:unread" :key "n" :sort-order newest-first))))
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(package-selected-packages
   (quote
    (helm-ls-git helm helm-notmuch multi-term engine-mode org-bullets ledger-mode company solarized-theme org magit color-theme)))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(ruby-indent-level 2)
 '(scroll-bar-mode nil)
 '(sendmail-program "/usr/bin/msmtp")
 '(show-paren-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(tab-width 8)
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(tool-bar-mode nil)
 '(user-full-name "Felipe Balbi")
 '(user-mail-address "felipe.balbi@intel.com")
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
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#242424" :foreground "#f6f3e8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "unknown" :family "Inconsolata")))))

(defun insert-any-by (tag)
  (save-excursion
    (goto-char (point-min))
    (if (search-forward "Signed-off-by:" nil t nil)
	(progn
	  (beginning-of-line)
	  (next-line)
	  (newline 3)
	  (previous-line 2)
	  (insert (concat tag ": Felipe Balbi <balbi@kernel.org>")))
      (error "Missing Signed-off-by string"))))

(defun insert-signed-off-by ()
  "Inserts a Signed-off-by line to a patch"
  (interactive)
  (insert-any-by "Signed-off-by"))

(defun insert-reviewed-by ()
  "Inserts a Reviewed-by line to a patch"
  (interactive)
  (insert-any-by "Reviewed-by"))

(defun insert-tested-by ()
  "Inserts a Tested-by line to a patch"
  (interactive)
  (insert-any-by "Tested-by"))


(require 'tls)
(require 'erc)

(defun start-erc ()
  "Connect to Intel's IRC."
  (interactive)
  (erc-tls :server "otcirc.ostc.intel.com" :port 6697
	   :nick "balbi" :full-name "balbi"))
(put 'downcase-region 'disabled nil)

(when window-system
  (global-hl-line-mode))

(when window-system
  (setq solarized-use-variable-pitch nil)
  (setq solarized-height-plus-1 1.0)
  (setq solarized-height-plus-2 1.0)
  (setq solarized-height-plus-3 1.0)
  (setq solarized-height-plus-4 1.0)
  (setq solarized-high-contrast-mode-line t)
  (load-theme 'solarized-dark t))

(add-hook 'org-mode-hook
	  (lambda ()
	    (org-bullets-mode t)))

(setq org-ellipsis "⤵")
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-src-window-setup 'current-window)

(setq org-hide-leading-stars t)

(add-hook 'sh-mode-hook
	  (lambda ()
	    (setq sh-basic-offset 2
		  sh-indentation 2)))

(require 'engine-mode)

(defengine amazon
  "http://www.amazon.com/s/ref=nb_sb_noss?url=search-alias%3Daps&field-keywords=%s"
  :keybinding "a")

(defengine duckduckgo
  "https://duckduckgo.com/?q=%s"
  :keybinding "d")

(defengine github
  "https://github.com/search?ref=simplesearch&q=%s")

(defengine google
  "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
  :keybinding "g")

(defengine google-images
  "http://www.google.com/images?hl=en&source=hp&biw=1440&bih=795&gbv=2&aq=f&aqi=&aql=&oq=&q=%s")

(defengine google-maps
  "http://maps.google.com/maps?q=%s"
  :keybinding "m")

(defengine stack-overflow
  "https://stackoverflow.com/search?q=%s"
  :keybinding "s")

(defengine twitter
  "https://twitter.com/search?q=%s")

(defengine wikipedia
  "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
  :keybinding "w")

(defengine wiktionary
  "https://www.wikipedia.org/search-redirect.php?family=wiktionary&language=en&go=Go&search=%s")

(defengine wolfram-alpha
  "http://www.wolframalpha.com/input/?i=%s")

(defengine youtube
  "http://www.youtube.com/results?aq=f&oq=&search_query=%s")

(engine-mode t)

(engine/set-keymap-prefix (kbd "C-c s"))

;(require 'multi-term)
;(setq multi-term-program-switches "--login")
;(global-set-key (kbd "C-c t") 'multi-term)

;; Helm
(require 'helm-config)
(require 'helm-ls-git)
(require 'helm-git-grep)

(setq helm-split-window-in-side-p           t ; open helm buffer inside current
					      ; window, not occupy whole other
					      ; window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of
					      ; source when reaching top or
					      ; bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require'
					      ; and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window
					      ; using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)

(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x r b") 'helm-bookmarks)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(add-hook 'c-mode-hook
	  (lambda ()
	    (setq default-directory "/home/balbi/workspace/linux/")))

(global-set-key (kbd "C-x C-d") 'helm-browse-project)

(global-set-key (kbd "C-c g") 'helm-git-grep)
;; Invoke `helm-git-grep' from isearch.
(define-key isearch-mode-map (kbd "C-c g") 'helm-git-grep-from-isearch)
;; Invoke `helm-git-grep' from other helm.
(eval-after-load 'helm
  '(define-key helm-map (kbd "C-c g") 'helm-git-grep-from-helm))

(helm-mode 1)
