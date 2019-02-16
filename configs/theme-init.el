;;; package --- theme-init
;;; Commentary:
;;; Code:

;; Define all customization and visual appearence

;; Set font and font size (also with C-x C-+ and C-x C--)
(set-face-attribute 'default nil :height 130)
;; (set-frame-parameter nil 'font "Droid Sans Mono")
(set-frame-parameter nil 'font "Monospace")
;; (set-frame-parameter nil 'font "Bitstream Vera Sans Mono")
;; (set-frame-parameter nil 'font "DejaVu Sans Mono")
;; (set-frame-font "Source Code Pro-12" nil t)

(custom-set-variables
 '(custom-safe-themes '("26d49386a2036df7ccbe802a06a759031e4455f07bda559dcf221f53e8850e69" default)))

;; Theme
(use-package moe-theme :ensure t
  :config (load-theme 'moe-dark)
          (custom-set-faces
					 '(idle-highlight ((t (:background "dim gray"))))
					 '(font-lock-comment-face ((t (:foreground "SlateBlue1" :slant italic))))
					 '(powerline-active2 ((t (:background "grey40" :foreground "white smoke")))))
  )

(use-package monokai-theme                  :ensure t :config (load-theme 'monokai)                  :disabled)
(use-package zenburn-theme                  :ensure t :config (load-theme 'zenburn)                  :disabled)
(use-package color-theme-sanityinc-tomorrow :ensure t :config (load-theme 'sanityinc-tomorrow-night) :disabled)
(use-package solarized-theme                :ensure t :config (load-theme 'solarized-dark)           :disabled)


;; Mode line
(use-package powerline :ensure t :init (powerline-center-theme))


(provide 'theme-init)

;;; theme-init ends here
