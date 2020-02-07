;;; package --- development-init
;;; Commentary:
;;; Code:

;; Define various features, "goodies"

(custom-set-variables
  '(tags-revert-without-query t)
  '(js-indent-level           2)
  '(css-indent-offset         2)
  '(company-backends '(company-etags
											 company-bbdb
											 company-capf
											 company-dabbrev-code
											 company-gtags
											 company-dabbrev
											 company-files
											 company-nxml
											 company-css
											 )
  )
 )


;; Highlighting rectangle areas C-RET start, C-? help
(cua-selection-mode 1)

;; Align code i.e. (add-to-list 'align-rules-list '(some-lang-types...
(bind-key "C-y a" 'align        prog-mode-map)
(bind-key "C-y r" 'align-regexp	prog-mode-map)

;; Jump to errors
(bind-key "s-n" 'next-error     prog-mode-map)
(bind-key "s-p" 'previous-error prog-mode-map)


;; Change face of chars beyond column 90
(use-package whitespace :ensure t
  :diminish whitespace-mode
  :custom (whitespace-line-column 90)
	        (whitespace-style      '(face lines-tail))
  :config
    (add-hook 'prog-mode-hook 'whitespace-mode)
    (custom-set-faces
     '(whitespace-line ((t (:background "gray20" :foreground "violet" :underline t))))))


;; Show nested parens in diff color
(use-package rainbow-delimiters :ensure t
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  )

;; SCSS mode
(defun scss-save-compile ()
"Save and compile SCSS file."
  (interactive)
  (save-buffer)
  (scss-compile)
  )
(use-package scss-mode :ensure t
  :bind   ("<f5>" . scss-save-compile)
  :config
    (setq scss-compile-at-save t
          scss-sass-options '("--sourcemap=none")
          )
    (add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
 )

;; Setup company
(use-package company :ensure t
  :diminish company-mode
  :bind   ("M-S-SPC" . company-complete)
  :init   (global-company-mode 1)
  :config (add-hook 'prog-mode-hook 'company-mode)
	        (add-hook 'haskell-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends)
                   (append '((company-dabbrev-code company-capf)) company-backends))))
 )

;; Configure FlyCheck
(use-package flycheck
  :ensure t
  :init
    (add-hook 'prog-mode-hook 'flycheck-mode)
    (fringe-mode '(4 . 0))
    (setq flycheck-highlighting-mode            'symbols
      	  flycheck-indication-mode              'left-fringe
          ;; flycheck-check-syntax-automatically   '(save idle-change new-line) ;; mode-enabled)
          ;; flycheck-check-syntax-automatically   '(save) ;; mode-enabled)
          flycheck-check-syntax-automatically   '(idle-change) ;; mode-enabled)
          flycheck-idle-change-delay            0.5
          flycheck-haskell-stack-ghc-executable "stack"
          flycheck-haskell-ghc-cache-directory
            (expand-file-name ".cache/flycheck-haskell" "~"))
   :config
     (bind-key "<f9>" 'list-flycheck-errors) ;; Must be here, otherwise Autoload error
)


(use-package flycheck-color-mode-line
  :ensure t
  :init (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
 )
(use-package flycheck-pos-tip
  :ensure t
  :init (setq flycheck-display-errors-function (function flycheck-pos-tip-error-messages))
 )
(use-package flycheck-status-emoji
  :ensure t
 )

;; Some handy edit modes
(use-package pkgbuild-mode :ensure t) ;; Arch pack manager
(use-package yaml-mode     :ensure t)

;; Curl in emacs buffer ?? obsolete
;; (use-package restclient :ensure t)


;; Disable prompt on exit when there are active processes
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))


;; Make the quotes in GHC error messages display nicely
(setq locale-coding-system   'utf-8)
(set-terminal-coding-system  'utf-8-unix)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system        'utf-8)


;;----------------------------------------------------------------------------
;; Add spell-checking in comments for all programming language modes
;;----------------------------------------------------------------------------
(use-package flyspell :ensure t
  :diminish flyspell-mode
  :config
    (if (fboundp 'prog-mode)
				(add-hook 'prog-mode-hook     'flyspell-prog-mode)
				(add-hook 'gfm-mode-hook      'flyspell-prog-mode)
				(add-hook 'markdown-mode-hook 'flyspell-prog-mode)
      (dolist (hook '(emacs-lisp-mode-hook yaml-mode shell-mode-hook haskell-mode-hook))
				(add-hook hook 'flyspell-prog-mode)))
    (add-to-list 'ispell-skip-region-alist '("^|" . "^|]"))
 )

;; JSON editing
(use-package json-mode :ensure t
  :custom
     (json-reformat:indent-width 2)
     )

;; DHALL editing
(use-package dhall-mode :ensure t
  :custom
    (dhall-use-header-line nil)
  )

;; Prepare Snippets
(use-package yasnippet :ensure t
  :diminish yas-minor-mode
  :custom (yas-verbosity 0)
  :config
    (setq-default yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt))
    (yas-reload-all)
 )

;; Emacs text web browser
(use-package w3m :ensure t ;; :disabled
	:custom (haskell-w3m-haddock-dirs '("~/.stack/docs_dir/"))
	        (w3m-mode-map              (make-sparse-keymap))
  :init (use-package w3m-haddock)
        (add-hook 'w3m-display-hook 'w3m-haddock-display)
	:bind (:map w3m-mode-map
					 ("<return>"    . 'w3m-view-this-url)
					 ("<mouse-1>"   . 'w3m-maybe-url)
					 ("<f5>"        . 'w3m-reload-this-page)
					 ("M-C-<left>"  . 'w3m-view-previous-page)
					 ("M-C-<right>" . 'w3m-view-next-page)
					 ("M-."         . 'w3m-haddock-find-tag)
					 ("q"           . 'bury-buffer)
					 ("C-c C-d"     . 'haskell-w3m-open-haddock)
			 )
  )

(defun w3m-maybe-url ()
	"Test if at point is url and open it if it is."
  (interactive)
  (if (or (equal '(w3m-anchor) (get-text-property (point) 'face))
          (equal '(w3m-arrived-anchor) (get-text-property (point) 'face)))
      (w3m-view-this-url)))


;; Some definitions
(setq compilation-scroll-output 'first-error
      require-final-newline      t)
(setq-default grep-scroll-output t) ;; Scroll as grep finds results

;; Revert buffer if external app modify it, unless it's changed
(progn
  (global-auto-revert-mode   t)
  (diminish                 'auto-revert-mode))

;;Automatically removes trailing whitespaces when file is saved
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; Run 'stop-server' located in project's root on every manual save of Haskell file
(defvar stop-server-script-name "stop-server"
  "Name of the script in project root directory." )
(defun stop-dev-server ()
  "Kill server so ghcid can reload with server already stopped from outside."
  (when (eq major-mode 'haskell-mode)                                     ;; if haskell mode
    (when (memq this-command '(save-buffer))                              ;; if manual save
      (let* ((project-root (projectile-project-root))                     ;; curr project root
             (script-path  (concat project-root stop-server-script-name)) ;; full path
             (script-exist (file-exists-p script-path))                   ;; does it exist
             (shell-cmd    (concat script-path " %s")))                   ;; make it accept param
        (if script-exist
            (progn (message "Stopping server with script: %s" script-path)
                   (shell-command-to-string (format shell-cmd buffer-file-name)))
          (message "Missing script %s. No action." script-path))))) )
(add-hook 'before-save-hook 'stop-dev-server)


;; Keep formating with space tabulation, i.e. inside records
(use-package keep-formation  :ensure t :disabled
  :load-path "~/.emacs.d/elisp/keep-formation/"
  :diminish keep-formation-mode
	:bind (:map haskell-mode-map
					 ("<delete>"    . 'keep-formation-delete-forward-char)
					 ("<backspace>" . 'keep-formation-delete-backward-char)
		   )
  :init  (add-hook 'haskell-mode-hook  'keep-formation-mode)
 )

;; Rules for automatic alignment
(eval-after-load "align"
  '(add-to-list 'align-rules-list '(haskell-types
     (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
     (modes quote (haskell-mode purescript-mode)))))
(eval-after-load "align"
  '(add-to-list 'align-rules-list '(haskell-assignment
     (regexp . "\\(\\s-+\\)=\\s-+")
     (modes quote (haskell-mode purescript-mode)))))
(eval-after-load "align"
  '(add-to-list 'align-rules-list '(haskell-arrows
     (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
     (modes quote (haskell-mode purescript-mode)))))
(eval-after-load "align"
  '(add-to-list 'align-rules-list '(haskell-left-arrows
     (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
     (modes quote (haskell-mode purescript-mode)))))
(eval-after-load "align"
  '(add-to-list 'align-rules-list '(haskell-comments
     (regexp . "\\(\\s-+\\)\\(--\\)\\s-+")
     (modes quote (haskell-mode purescript-mode)))))

;; Project managment
(use-package project-init)

;; Versioning control
(use-package version-control-init)

;; Programming langs
(use-package haskell-init)
(use-package purescript-init)


(diminish 'flyspell-mode)

(provide 'development-init)


;;; development-init ends here
