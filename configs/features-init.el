;;; package --- features-init
;;; Commentary:
;;; Code:

;; Define various features, "goodies"

(setq browse-url-browser-function 'browse-url-generic
      ;; browse-url-generic-program  "chromium-browser")
      browse-url-generic-program  "firefox"
)
(global-set-key (kbd "<f12>") 'browse-url-at-point)


;; Scroll with mouse 1-by-1
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))


;; Suppress some byte-compile warnings
(setq byte-compile-warnings
			'(not obsolete suspicious	free-vars unresolved callargs redefine noruntime
						constants	cl-functions interactive-only lexical make-local mapcar))
;; (setq warning-minimum-level :error) ;; Suppress all Warnings


;; Highlight word with point through the buffer
(use-package idle-highlight-mode :ensure t
  :init
    (add-hook 'prog-mode-hook 'idle-highlight-mode)
    (custom-set-faces '(idle-highlight  ((t (:background
     			                                   ;; "lavender"
																						 "dim gray"
																						 )))))
 )


;; Walk through CamelCase words as part are separate words
(use-package subword :ensure t
  :diminish subword-mode
  :bind (("C-<right>" . subword-right) ("C-<left>" . subword-left))
  :init (global-subword-mode)
 )


;; Not available from melpa. Use GitHub: https://github.com/emacsmirror/bookmark-plus
(use-package bookmark+
  :load-path "~/.emacs.d/elisp/bookmark-plus/"
	)

;; Show number of search matches and current position
(use-package anzu :ensure t
  :diminish anzu-mode
  :init   (global-anzu-mode t)
	:config (set-face-attribute 'anzu-mode-line nil :foreground "red" :weight 'bold)
 )

(use-package recentf :ensure t
  :bind  ("C-x C-r" . ido-recentf-open)
  :init (recentf-mode t)
        (defun ido-recentf-open ()
					(interactive)
					(find-file (ido-completing-read "Find recent file: " recentf-list)))
				(add-to-list 'recentf-exclude "~$")
				(add-to-list 'recentf-exclude "\\TAGS\\'")
				(add-to-list 'recentf-exclude "~/.emacs.d/elpa/")
	:custom (recentf-max-saved-items 50)
	        (recentf-auto-cleanup    300)
)


;; Use undo-tree
(use-package undo-tree :ensure t
  :diminish undo-tree-mode
  :bind (("C-z"   . undo)
				 ("C-S-z" . redo))
  :init (global-undo-tree-mode)
	      (defalias 'redo 'undo-tree-redo)
	:custom (undo-tree-auto-save-history       t)
	        (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo/")))
 )

(use-package google-translate :ensure t
  :bind (("M-C-S-t" . google-translate-at-point)
				 ("C-c t"   . google-translate-query-translate))
  :custom (google-translate-default-source-language "en")
					(google-translate-default-target-language "hr")
					(google-translate-enable-ido-completion    t)
					(google-translate-output-destination      'popup)
					(google-translate-pop-up-buffer-set-focus  t)
 )
(use-package popup :ensure t)

;; Use Smex, not Helm, Ido-style M-x
(use-package smex :ensure t
  :bind (("M-x"     . smex)
				 ("M-X"     . smex-major-mode-commands)
				 ("C-c M-x" . execute-extended-command))
  :init (smex-initialize)
  )


;; Describe keybindings for current major mode
(use-package discover-my-major :ensure t
  :bind ("C-M-?" . discover-my-major)
)


;; Add ignored dirs/files for `rgrep' to its defaults
(custom-set-variables
 '(grep-find-ignored-files
   '("TAGS" ".#*" "*.hi" "*.o" "*~" "*.bin" "*.lbin" "*.so" "*.a" "*.ln" "*.blg" "*.bbl" "*.elc" "*.lof" "*.glo" "*.idx" "*.lot" "*.fmt" "*.tfm" "*.class" "*.fas" "*.lib" "*.mem" "*.x86f" "*.sparcf" "*.dfsl" "*.pfsl" "*.d64fsl" "*.p64fsl" "*.lx64fsl" "*.lx32fsl" "*.dx64fsl" "*.dx32fsl" "*.fx64fsl" "*.fx32fsl" "*.sx64fsl" "*.sx32fsl" "*.wx64fsl" "*.wx32fsl" "*.fasl" "*.ufsl" "*.fsl" "*.dxl" "*.lo" "*.la" "*.gmo" "*.mo" "*.toc" "*.aux" "*.cp" "*.fn" "*.ky" "*.pg" "*.tp" "*.vr" "*.cps" "*.fns" "*.kys" "*.pgs" "*.tps" "*.vrs" "*.pyc" "*.pyo"))
 '(grep-find-ignored-directories
    '("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" ".stack-work" "dist-newstyle"))
 )

;; `rgrep' command without ugly command display at top of buffer
(use-package grep-no-header
  :load-path "~/.emacs.d/elisp/grep-no-header/"
  :bind ("M-f" . mgrep)
 )

;; World time in some cities
(use-package time
  :bind ("C-x w t" . display-time-world)
  :config
    (setq display-time-world-time-format "%d.%m %R %Z"
          display-time-world-list '(("Europe/Berlin"         "Zagreb")
		  															("America/New_York"      "New York")
																		("Europe/London"         "London")
																		("Asia/Tel_Aviv"         "Tel_Aviv")
																		("America/San_Francisko" "San Francisko")))
	)

(use-package memory-usage :ensure t)

(provide 'features-init)

;;; features-init ends here
