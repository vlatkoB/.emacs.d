;;; package --- keybindings-init
;;; Commentary:
;;; Code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Windows/frame/buffer related
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Close current windows and kill its buffer
(bind-key "C-w"   'kill-this-buffer)  ;; Close current editor
(bind-key "C-S-w" 'delete-window)     ;; Leave its buffer open

;; Switch to previous window
(bind-key "<f2>" 'switch-to-previous-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing related
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Un/comment / duplicate line/region
(bind-key "C-7"         'comment-dwim-line)
(bind-key "C-M-<prior>" 'duplicate-line-or-region)

;; Select all
(bind-key "C-a" 'mark-whole-buffer)

;; Replace
(bind-key "C-h" 'query-replace)

;; Autocompletion M-RET
(bind-key "M-RET" 'dabbrev-expand)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Esc key escapes from everything
(use-package escapeKey-init)

;; global key for `multi-occur-in-this-mode'
(bind-key "C-<f2>" 'multi-occur-in-this-mode)

;; Context menu key acts like M-x
(bind-key "<menu>" 'smex)

;; Format with brittany
(bind-key "M-ž" 'format-haskell-with-brittany_and_stylish-haskell)



(provide 'keybindings-init)

;;; keybindings-init ends here
