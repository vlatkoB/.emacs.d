;;; package --- sessions-init
;;; Commentary:
;;; Code:

;; Define all session preserving related stuff
(use-package desktop :ensure t
  :init   (desktop-save-mode t)
	:config (let ((last-project (get-last-used-desktop)))
						(when last-project
							(setq desktop-dirname  last-project
										desktop-path    (cons last-project '()))))
	        (add-to-list 'desktop-modes-not-to-save 'dired-mode)
					(add-to-list 'desktop-modes-not-to-save 'Info-mode)
					(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
					(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
	:custom (history-length                250)
	        (desktop-base-file-name       ".emacs-desktop")
					(desktop-dirname              "~/.emacs.d/")
					(desktop-path                 '("~/.emacs.d/"))
					(desktop-missing-file-warning  nil)
					(desktop-restore-eager         nil)
					(desktop-save                  t)
					(desktop-buffers-not-to-save
					 (concat "\\(" "^nn\\.a[0-9]+\\|\\.log\\|^tags\\|^TAGS" "\\)$"))
					(desktop-globals-to-save (append ' ((comint-input-ring				.  50)
																							(compile-history          .  30)
																							(dired-regexp-history			.  20)
																							(extended-command-history .  30)
																							(face-name-history				.  20)
																							(file-name-history        . 100)
																							(grep-history             .  30)
																							(grep-find-history				.  30)
																							(ido-buffer-history       . 100)
																							(ido-last-directory-list	. 100)
																							(ido-work-directory-list  . 100)
																							(ido-work-file-list				. 100)
																							(magit-read-rev-history   .  50)
																							(minibuffer-history				.  50)
																							(org-clock-history        .  50)
																							(org-refile-history				.  50)
																							(org-tags-history         .  50)
																							(query-replace-history		.  60)
																							(read-expression-history  .  60)
																							(regexp-history           .  60)
																							(regexp-search-ring				.  20)
																							(search-ring              .  20)
																							(shell-command-history		.  50)
																							desktop-missing-file-warning
																							kill-ring
																							smex-history
																							register-alist
																							tags-file-name
																							tags-table-list
																							)))
		)

;; Set auto-save location
(set-auto-saves)

(setq bookmark-save-flag t)


(provide 'sessions-init)

;;; sessions-init ends here
