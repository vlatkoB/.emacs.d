;;; package --- escapeKey-init
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make Esc quits everything
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defadvice keyboard-escape-quit (around my-keyboard-escape-quit activate)
  "Disable Esc to delete other windows"
  (let (orig-one-window-p)
    (fset 'orig-one-window-p (symbol-function 'one-window-p))
    (fset 'one-window-p (lambda (&optional nomini all-frames) t))
    (unwind-protect
        ad-do-it
      (fset 'one-window-p (symbol-function 'orig-one-window-p)))))

;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(bind-key "<escape>" 'keyboard-escape-quit)  ;; also closes other windows if pressed in window
(bind-key "<escape>" 'minibuffer-keyboard-quit minibuffer-local-map)
(bind-key "<escape>" 'minibuffer-keyboard-quit minibuffer-local-ns-map)
(bind-key "<escape>" 'minibuffer-keyboard-quit minibuffer-local-completion-map)
(bind-key "<escape>" 'minibuffer-keyboard-quit minibuffer-local-must-match-map)
(bind-key "<escape>" 'minibuffer-keyboard-quit minibuffer-local-isearch-map)
(bind-key "<escape>" 'isearch-abort            isearch-mode-map)
;;(eval-after-load 'helm-mode (bind-key "<escape>" 'helm-keyboard-quit       helm-map))
;; (bind-key "<escape>" 'keyboard-quit)         ;; everywhere else



(provide 'escapeKey-init)

;;; escapeKey-init ends here
