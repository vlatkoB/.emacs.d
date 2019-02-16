;;; package --- ido-init
;;; Commentary:
;;; Code:

;; Configure ido for use everywhere

;; Ido setup
;; flx
(use-package ido :ensure t
  :bind   ("C-x C-f" . ido-find-file)
  :config (use-package flx-ido :ensure t)
  :custom (ido-mode                               1)
          (ido-ubiquitous-mode                    1)
          (ido-everywhere                         t)
          (ido-use-filename-at-point             'guess)
					(ido-use-url-at-point                   t)
          (ido-enable-flex-matching               t)
          (ido-use-faces                          t)
					(ido-enable-dot-prefix                  t)
      		(ido-enter-matching-directory           t)
          (ido-auto-merge-work-directories-length 0)
          (ido-use-virtual-buffers                nil)
					(ido-ignore-extensions                  t)
          (flx-ido-mode                           1)
          (ido-ignore-directories                '("\\.stack-work" "\\dist-newstyle \\.git \\.hg"))
          (ido-ignore-files                      '("\\.dyn_hi$"
																									 "\\.dyn_o$"
																									 "\\.hi$"
																									 "\\.o$"
																									 "\\.tags$"
																									 "^\\.ghci$"
																									 ))
		)

;; Use ido everywhere
(use-package ido-completing-read+ :ensure t	:custom (ido-ubiquitous-mode t))

;; Jump in where Ido can't
(use-package icomplete :ensure t :custom (icomplete-mode t))

(use-package ido-vertical-mode :ensure t
  :init   (ido-vertical-mode 1)
	:config (set-face-attribute 'ido-vertical-first-match-face nil :background "#e5b7c0")
	        (set-face-attribute 'ido-vertical-only-match-face nil  :background "#e52b50"
															                                   :foreground "white")
					(set-face-attribute 'ido-vertical-match-face nil		   :foreground "#b00000")
	:custom (ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
          (ido-vertical-show-count  t)
          (ido-use-faces            t)
 )

;; Smart change of SPACE key in ido to space or hypen char
(use-package ido-complete-space-or-hyphen :ensure t)

;; Answer YES/NO in ido
(use-package ido-yes-or-no :ensure t :custom (ido-yes-or-no-mode t))

;; Sort files in ido by modification time
(use-package ido-sort-mtime :ensure t :init (ido-sort-mtime-mode t))


(provide 'ido-init)

;;; ido-init ends here
