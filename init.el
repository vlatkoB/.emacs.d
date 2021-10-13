;;; package --- init
;;; Commentary:
;;; Code:

;; Start server
(require 'server)
(unless (server-running-p)
  (server-start)
  )


;; Identificate myself
(setq user-login-name   "vlatkoB")
(setq user-full-name    "Vlatko Bašić")
(setq user-mail-address "vlatko.basic@gmail.com")

;; Add package repos
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org"   . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;; (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; Bootstrap `use-package', has `bind-key' package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(use-package diminish	:ensure t)
(eval-when-compile (require 'use-package))
(setq use-package-verbose               nil ;; Show loading info of every package
			;; use-package-minimum-reported-time 0.2
			;; use-package-compute-statistics    t
			)

;; Suppress some byte-compile warnings
(setq byte-compile-warnings
			'(not obsolete suspicious	free-vars unresolved callargs redefine noruntime
						constants	cl-functions interactive-only lexical make-local mapcar))
;; (setq warning-minimum-level :error) ;; Suppress all Warnings


;; Set custom Customization file
(setq custom-file "~/.emacs.d/configs/dummy.el")
;; (load custom-file)

;; Ignore "func got redefined" messages
(setq ad-redefinition-action 'accept)

;; Set custom configs path
(add-to-list 'load-path (expand-file-name "configs" user-emacs-directory))

;; (use-package debug-init :disabled) ;; Debugging statements
;; (use-package helm-init)            ;; Helm config (only helm-swoop)
(use-package functions-init)       ;; Some functions used through out setup
(use-package theme-init)           ;; Set theme and visual appearence
(use-package ido-init)             ;; ido everywhere config
(use-package layout-init)          ;; GUI layout
(use-package sessions-init)        ;; Sessions perserving stuff
(use-package features-init)        ;; Various features
(use-package keybindings-init)     ;; Custom key bindings
(use-package org-init)             ;; Org-mode stuff
(use-package markdown-init)        ;; Writing markdown stuff
(use-package misc-init)            ;; Misc stuff
(use-package development-init)     ;; Setup development environment


;; Ignore some messages in mini-buffer
(defun my-command-error-function (data context caller)
"Ignore msgs in mini-buffer."
  (when (not (memq (car data) '(buffer-read-only
                                beginning-of-buffer
                                end-of-buffer)
									 ))
    (command-error-default-function data context caller)))
(setq command-error-function #'my-command-error-function)

(setq-default indent-tabs-mode nil)

;; disable safe question ion for these local vars
(put 'haskell-compile-stack-build-command 'safe-local-variable
   (lambda (x) t))

;; (global-set-key (kbd "<f9>") 'save-buffer)

(add-to-list 'auto-mode-alist '("\\.xlf\\'" . nxml-mode))

;; (byte-compile-disable-warning 'cl-functions)

(set-language-environment "utf-8")

(provide 'init)
;;; init ends here

;; (message "%s" major-mode)
;; (byte-recompile-directory "~/.emacs.d/elisp/psci/" 0)
;; (custom-set-variables'(package-load-list '(all (haskell-mode "20181122.823"))))
