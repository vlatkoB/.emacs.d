;;; package --- project-init
;;; Commentary:
;;; Code:


;; http://batsov.com/projectile/
(use-package projectile
  :ensure t
  :bind (("C-c f"   . projectile-find-file)
         ("C-c C-g" . projectile-regenerate-tags)
         ("C-c P"   . projectile-commander)
         ("C-c C-c" . projectile-compile-project)
				 ("C-c p w" . switch-desktop))
  :init  	(projectile-global-mode)
	:config
    (add-to-list 'projectile-other-file-alist '("hs" "hamlet" "lucius" "cassius" "julius"))
    (projectile-projects-to-ecb-source-path)
	:custom
    (projectile-keymap-prefix                  (kbd "C-c p"))
		(projectile-completion-system              'ido)
		(projectile-enable-caching                  t)
		(projectile-enable-idle-timer               t)
		(projectile-indexing-method                'alien)
		(projectile-sort-order                     'recentf)
    (projectile-tags-command                   "codex update")
		(projectile-mode-line                     '(:eval (format " [%s]" (projectile-project-name))))
 )

(setq projectile-globally-ignored-directories
	(append '("BU" "dist-newstyle")
		projectile-globally-ignored-directories)
)
(setq projectile-globally-ignored-files
	(append '(".project" "*.hi")
		projectile-globally-ignored-files)
 )


(provide 'project-init)


;;; project-init ends here
