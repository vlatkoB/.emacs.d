;;; package --- version-control-init
;;; Commentary:
;;; Code:

;; Define all version control and backup stuff

;; Prevent backup copies of saved files and set backup location
(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
			version-control         t    ;; Use version numbers for backups.
      kept-new-versions       10   ;; Number of newest versions to keep.
      kept-old-versions       0    ;; Number of oldest versions to keep.
      delete-old-versions     t    ;; Don't ask to delete excess backup versions.
      backup-by-copying       t    ;; Copy all files, don't rename them.
			;; make-backup-files nil        ;; disable making backup files
)

;; Git control
(use-package magit :ensure t
  :diminish magit-mode	"Git"
  :custom (magit-auto-revert-mode             nil)
;;  :diminish magit-auto-revert-mode
)

;; Hg control
(use-package monky :ensure t
  :custom (monky-process-type 'cmdserver)
)

(use-package git-gutter :ensure t
  :diminish git-gutter-mode
  :init (add-hook 'prog-mode-hook 'git-gutter-mode)
	      (git-gutter:linum-setup)
				(custom-set-variables  '(git-gutter:hide-gutter t)
															 '(git-gutter:verbosity   0))
  :custom (magit-status-buffer-switch-function 'switch-to-buffer)
	        (magit-completing-read-function      'magit-ido-completing-read)
					(vc-handled-backends                 '(git))
 )


(provide 'version-control-init)

;;; version-control-init ends here
