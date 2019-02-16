;;; package --- misc-init
;;; Commentary:
;;; Code:

;; Some miscelenous/testing handy stuff

(use-package twittering-mode :ensure t :disabled)


;;:: From version-control-init
(use-package git-gutter-fringe+ :ensure t :disabled
  :init   (git-gutter+-toggle-fringe)
  :custom (git-gutter-fr:side 'right-fringe)
	        (left-fringe-width   10)
          (right-fringe-width  4)
 )


;;:: From layout-control-init

;; Hide minor modes by name
(use-package rich-minority :ensure t :disabled
  :demand
  :diminish rich-minority-mode
  :config	(if (not rich-minority-mode) (rich-minority-mode 1))
	:custom (rm-blacklist '(" Fly" " Hi"))
 )


;;:: From layout-control-init

;; Displays available key bindings
(use-package guide-key :ensure t :disabled   ;; messes up the layout
  :diminish guide-key-mode
  :init   (guide-key-mode t)
	:custom (guide-key/guide-key-sequence '("C-x r" "C-x 4"))
	        (guide-key/guide-key-sequence t)
					(guide-key/highlight-command-regexp "rectangle")
					(guide-key/highlight-command-regexp '("rectangle"
																								("register" . font-lock-type-face)
																								("bookmark" . font-lock-warning-face)))
					(guide-key/idle-delay 0.1)
					(guide-key/popup-window-position 'top)
 )


;; Ido/helm menu entries
(use-package lacarte :ensure t :disabled)

;; Show vertical lines to guide indentation
(use-package indent-guide :ensure t :disabled
  :custom (indent-guide-delay 0.1)
  :init	  (indent-guide-recursive t)
        	(indent-guide-global-mode)
 )

;; hippie expand is dabbrev expand on steroids
(use-package hippie-exp :ensure t :disabled  ;; not practical!? Or is it?
	:bind ("s-a" . hippie-expand)
  :custom (hippie-expand-try-functions-list
					 '(try-expand-dabbrev
						 try-expand-dabbrev-all-buffers
						 try-expand-dabbrev-from-kill
						 try-complete-file-name-partially
						 try-complete-file-name
						 try-expand-all-abbrevs
						 try-expand-list
						 try-expand-line
						 try-complete-lisp-symbol-partially
						 try-complete-lisp-symbol))
)

;; Shorten prompt answers
;; (defalias 'yes-or-no-p 'y-or-n-p)


;; From development-init

;; View/jump to fixme, todo, XXX. Priority with multiple 'e' fixmeeee
(use-package fixmee :ensure t :disabled
  :diminish fixmee-mode
  :diminish hi-lock-mode
  :diminish button-lock-mode
  :init (setq global-fixmee-mode 1)
 )

(provide 'misc-init)

;;; misc-init ends here
