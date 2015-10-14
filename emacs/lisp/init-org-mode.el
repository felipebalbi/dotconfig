(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-capture-templates
      '(("t"
	 "Task item"
	 entry
	 (file+headline "~/workspace/org/notes.org" "Tasks")
	 "* TODO %?\n   %i\n   %a")
	))

(provide 'init-org-mode)
