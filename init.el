;;; package --- init
;;; Commentary:
;;; Code:

;; Start server
(server-start)

;; Identificate myself
(setq user-login-name   "vlatkoB")
(setq user-full-name    "Vlatko Bašić")
(setq user-mail-address "vlatko.basic@gmail.com")

;; Add Melpa repository
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("org"   . "https://orgmode.org/elpa/")          t)
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;; (add-to-list 'package-archives '("melpa.org" . "http://melpa.org/packages/"))
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

;; (custom-set-variables'(package-load-list '(all (haskell-mode "20181122.823"))))

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
  (when (not (memq (car data) '(buffer-read-only
                                beginning-of-buffer
                                end-of-buffer)
									 ))
    (command-error-default-function data context caller)))
(setq command-error-function #'my-command-error-function)

(setq-default indent-tabs-mode nil)


;; (global-set-key (kbd "<f9>") 'save-buffer)

(add-to-list 'auto-mode-alist '("\\.xlf\\'" . nxml-mode))

(byte-compile-disable-warning 'cl-functions)

(set-language-environment "utf-8")

(provide 'init)
;;; init ends here

;; (message "%s" major-mode)
;; (byte-recompile-directory "/home/vlatko/.emacs.d/elisp/flycheck/" 0)
;; (byte-recompile-directory "/home/vlatko/.emacs.d/elpa/swoop-20160120.915/" 0)
