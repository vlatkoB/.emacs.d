;;; package --- layout-init
;;; Commentary:
;;; Code:
;; Define all Layout-related stuff


;; Kill unwanted buffers after 15 seconds, except those defined in
;; `kill-unwanted-buffers-except-name', `kill-unwanted-buffers-except-mode',
;; starts with *ECB, are not visible, have active process
;;, but kill those with prefix defined in `kill-unwanted-buffers-prefixed',
(run-with-idle-timer 15 t 'kill-unwanted-buffers)

;; Manually resize window
(use-package move-border
  :load-path "~/.emacs.d/elisp/move-border/"
  :bind (("M-S-<up>"    . move-border-up)
         ("M-S-<down>"  . move-border-down)
         ("M-S-<left>"  . move-border-left)
         ("M-S-<right>" . move-border-right))
  )

;; Alt+arrow moves focus to up,down,left or right window
(use-package windmove
  :ensure t
  :config (windmove-default-keybindings 'meta)
)


;; Enable TabBar
(use-package tabbar
  :ensure t
  :bind (("C-<tab>"           . tabbar-forward-tab)    ;; Switch tabs with Ctrl-tab
         ("C-S-<iso-lefttab>" . tabbar-backward-tab)   ;; Switch tabs with Ctrl-Shift-tab
         ("s-<tab>"           . tabbar-backward-group) ;; Switch groups with Ctrl-Alt-tab
         ("s-S-<tab>"         . tabbar-forward-group)) ;; Switch groups with Ctrl-Alt--Shift-tab
  :init (tabbar-mode t)
	:config
    (custom-set-faces
     '(tabbar-button
       ((t (:inherit tabbar-default))))
     '(tabbar-selected
       ((t (:background "#f2f2f6" :foreground "black" :box nil :weight bold :height 1.0))))
     '(tabbar-unselected
       ((t (:inherit tabbar-default :background "gray95" :foreground "grey30" :weight normal  :height 1.2)))))
    (setq tabbar-separator '(0.5) tabbar-buffer-groups-function 'tabbar-grouping)
    (defun tabbar-grouping () ;; show all source files in Develop group
      (list (cond
             ((eq major-mode 'emacs-lisp-mode)		      "Development")
             ((eq major-mode 'hamlet-mode)		          "Development")
             ((eq major-mode 'haskell-mode)		          "Development")
             ((eq major-mode 'haskell-cabal-mode)	      "Development")
             ((eq major-mode 'magit-status-mode)	      "Development")
             ((eq major-mode 'nxml-mode)		            "Development")
             ((eq major-mode 'org-mode)		              "Development")
             ((eq major-mode 'shakespeare-hamlet-mode)	"Development")
             ((eq major-mode 'shakespeare-julius-mode)	"Development")
             ((eq major-mode 'shakespeare-lucius-mode)	"Development")
             ((eq major-mode 'text-mode)		            "Development")
             ((eq major-mode 'yaml-mode)		            "Development")
             ((eq major-mode 'fundamental-mode)		      "Fundamental")
             ((eq major-mode 'tags-table-mode)		      "Fundamental")
             ((eq major-mode 'lisp-interaction-mode)	  "Misc")
             ((eq major-mode 'package-menu-mode)	      "Misc")
             ((string-equal "*haskell-process-log*" (buffer-name)) "user")
             ((string-equal "*Google Translate*"    (buffer-name)) "Development")
             ;; ((eq major-mode 'special-mode)          "Special")
             ;; ((eq major-mode 'dired-mode)            "emacs")
             ;; ((eq major-mode 'haskell-interactive-mode) "GHCi")
             ;; ((string-equal "*" (substring (buffer-name) 0 1)) "starred")
             (t "user")))) ;; All other modes go to user group
 )

(setq split-height-threshold 1200)


;; Back-button provides an alternative method for navigation by analogy with the "back" button in a web browser
(use-package back-button
  :ensure t
  :diminish back-button-mode
  :bind (("C-x   <left>"  . back-button-local-backward)
				 ("C-x   <right>" . back-button-local-forward)
				 ("C-x C-<left>"  . back-button-global-backward)
				 ("C-x C-<right>" . back-button-global-forward)
				)
  :init (back-button-mode t)
 )


;; Set buffer file name to Emacs title bar, abbrev if in projects dir
(defvar wrk-path "/home/vlatko/dev/Haskell")
(setq frame-title-format
  '(:eval
     (if buffer-file-name
       (if (string-prefix-p wrk-path buffer-file-name)
         (file-relative-name  (file-name-directory  buffer-file-name) wrk-path)
         (file-name-directory (abbreviate-file-name buffer-file-name)))
       "%b")))


(custom-set-variables
  '(column-number-mode                t)
  '(delete-selection-mode             t)
  '(global-hl-line-mode               t)
  '(global-linum-mode                 t)
  '(line-number-mode                  t)
  '(menu-bar-mode                     t)
  '(normal-erase-is-backspace-mode    t)
  '(scroll-bar-mode                   'right)
  '(scroll-step                       1)
  '(setq-default                      indent-tabs-mode nill)
  '(show-paren-mode                   t)
  '(size-indication-mode              nil)
  '(tool-bar-mode                     nil)
  '(transient-mark-mode               nil)
  '(uniquify-ask-about-buffer-names-p t)
  '(uniquify-buffer-name-style        'post-forward nil (uniquify))
  '(use-dialog-box nil)
  '(tab-width 2)
  )

;; Enable folding of indented text
(use-package origami :ensure t
  :bind (("M-s-<left>"  	  . origami-close-node-recursively)
         ("M-s-<right>"	    . origami-open-node-recursively)
         ("  <Scroll_Lock>" . origami-close-all-nodes)
         ("S-<Scroll_Lock>" . origami-open-all-nodes))
  :init (global-origami-mode)
 )


;; Use ECB for layouting windows
(use-package ecb-init)   ;; Initialize ECB


(provide 'layout-init)

;;; layout-init ends here
