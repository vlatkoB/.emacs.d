;;; package --- helm-init
;;; Commentary:
;;; Code:

;; Not used
(use-package helm :ensure t :disabled
  :diminish helm-mode
  :bind (("M-y"     . helm-show-kill-ring)
				 ("C-x b"   . helm-mini)
				 ;; ("C-c h"   . helm-command-prefix)
				 ;; ("C-x C-f" . helm-find-files) ;; using ido
				:map helm-map
				  ("<tab>" . 'helm-select-action)
					;; ("C-y"   . 'helm-execute-persistent-action)
					;; ("C-SPC" . 'helm-select-action)
    )
  :init (helm-mode 1)
  	    (when (executable-find "curl")
					(setq helm-google-suggest-use-curl-p t))
				(unbind-key "C-x c")
	:custom
    (helm-split-window-in-side-p           t)
		(helm-move-to-line-cycle-in-source     t)
		(helm-ff-search-library-in-sexp        t) ; search for library in `require' and `declare-function' sexp.
		(helm-scroll-amount                    8) ; scroll 8 lines other window using M-<next>/M-<prior>
		(helm-ff-file-name-history-use-recentf t)
		(helm-quick-update                     t)
		(helm-bookmark-show-location           t)
		(helm-buffers-fuzzy-matching           t)
 )

(use-package helm-swoop :ensure t
  :bind (("C-S-o"   . helm-multi-swoop-all)
				 ("C-M-o"   . helm-multi-swoop-projectile)
				 ;; ("C-c M-i" . helm-multi-swoop)
				 ;; ("C-M-i"   . helm-swoop)

				 ;; ("C-M-o"   . helm-multi-swoop-all)
				 ;; ("C-M-o"   . helm-multi-swoop-current-mode)
		     ;; :map helm-swoop-map   ("M-i" . 'helm-multi-swoop-all-from-helm-swoop)
		     ;; :map isearch-mode-map ("M-i" . 'helm-swoop-from-isearch)
		)
  :custom (helm-multi-swoop-edit-save             t)
	        (helm-swoop-split-with-multiple-windows t) ;; t = inside the current window
					;; (helm-swoop-split-direction            'split-window-vertically)
					(helm-swoop-split-direction            'split-window-horizontally)
					(helm-swoop-speed-or-color              nil)
					(helm-swoop-move-to-line-cycle          t)
					(helm-swoop-use-line-number-face        t)
					(helm-swoop-use-fuzzy-match             nil)
					(helm-swoop-pre-input-function				 (lambda () (thing-at-point 'symbol)))
 )


(provide 'helm-init)

;;; helm-init ends here
