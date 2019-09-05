;;; package --- markdown-init
;;; Commentary:
;;; Code:


(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "markdown")
  )

;; Markdown preview helper
(use-package grip-mode
  :ensure  t
  :defer t
  ;; :hook   ((markdown-mode org-mode) . grip-mode)
  :custom (grip-github-user "vlatkoB")
  :init   (load-file        "~/.emacs.d/configs/DO_NOT_TRACK_git-pwd.el")
  )


;; Markdown ToC generator
(use-package markdown-toc :ensure t)

(provide 'markdown-init)

;;; markdown-init.el ends here
