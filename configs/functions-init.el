;;; package --- functions-init
;;; Commentary:
;;; Code:

;; Define various functions

;; Kill unwanted buffers after x seconds
(defvar kill-unwanted-buffers-except-name
  '("\\` \\*Minibuf-[[:digit:]]+\\*\\'" "\\` \\*Echo Area [[:digit:]]+\\*\\'"  "\\`\\*Messages\\*\\'" ))
(defvar kill-unwanted-buffers-prefixed '("*helm"))
(defvar kill-unwanted-buffers-except-mode '("haskell-interactive-mode"))

(defun buffer-visible-and-focused (buf)
"Check if BUF bufffer is visible and/or focused."
  (cond
    ((eq buf (window-buffer nil)) 1)  ;; Visible and focused
    ((get-buffer-window buf)      2)  ;; Visible and unfocused
    (t                            3)  ;; Not visible
  )
)


(defun kill-unwanted-buffers ()
"Kill following buffers:
- all star buffers except those in `kill-star-buffers-except-name'
- those with prefix in `kill-buffers-prefixed'
- except those that are not visible or focused"
  (interactive)
  (mapc
    (lambda (buf)
     (let ((buf-name    (buffer-name buf))
	   (buf-vis     (buffer-visible-and-focused buf))
	   (buf-mode    (buffer-local-value 'major-mode buf))
	   (buf-par-mod (get (buffer-local-value 'major-mode buf) 'derived-mode-parent))
	   )
			  ;; (message buf-name)
        (when
          (or
	   (some (lambda (elt) (string-prefix-p elt buf-name))
	  	    kill-unwanted-buffers-prefixed)
            (and
              (string-match-p "\\` *\\*.*\\*\\'" buf-name)   ;; take start buffers
              (eq buf-vis 3)                                 ;; ignore if buffer visible
              (not (string-prefix-p " *ECB" buf-name))       ;; ignore ECB buffers
              (null (get-buffer-process buf))                ;; ignore buffer with process
              (notany (lambda (except) (string-match-p except buf-name))
                         kill-unwanted-buffers-except-name)  ;; ignore these
              (notany (lambda (except) (string-match-p except (symbol-name buf-mode)))
                         kill-unwanted-buffers-except-mode)) ;; ignore these
	      )
          (kill-buffer buf)
	  ;; (princ (concat buf-name " -> " (symbol-name buf-mode)
	  ;; 		 " -> (" (symbol-name buf-par-mod)
	  ;; 		 " -> " (symbol-name (get buf-par-mod 'derived-mode-parent))
	  ;; 		 ")\n" ))
	  ;; (print (concat "=== " (symbol-name (derived-mode-parents buf-par-mod)) " ==="))
          (tabbar-display-update))))
		(buffer-list)))

(defun derived-mode-parents (mode)
	"A MODE param - name of the mode."
  (and mode (cons mode (derived-mode-parents (get mode 'derived-mode-parent)))))


;; Switch to previous window
(defun switch-to-previous-buffer () "Switch to previous buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))


;; Un/comment line/region   ;; from Chris Done
(defun comment-dwim-line (&optional arg)
  "Do-what-I-mean commenting the current line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))


;; Duplicate current line or region
(defun duplicate-line-or-region (&optional n)
  "Duplicate current line, or region if active.
With argument N, make N copies.
With negative N, comment out original line and use the absolute value."
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region        ;Get region if active, otherwise line
                      (buffer-substring (region-beginning) (region-end))
                    (prog1 (thing-at-point 'line)
                      (end-of-line)
                      (if (< 0 (forward-line 1)) ;Go to beginning of next line, or make a new one
                          (newline))))))
        (dotimes (i (abs (or n 1)))     ;Insert N times, or once if not specified
          (insert text))))
    (if use-region nil                  ;Only if we're working with a line (not a region)
      (let ((pos (- (point) (line-beginning-position)))) ;Save column
        (if (> 0 n)                             ;Comment out original with negative arg
            (comment-region (line-beginning-position) (line-end-position)))
        (forward-line 1)
        (forward-char pos))))
	)

;; Add projectile's project name to ECB directories
(defun projectile-projects-to-ecb-source-path ()
"Add projectile's paths to ECB directories."
  (interactive)
  (dolist (project-path projectile-known-projects)
    (let ((project-name (file-name-nondirectory (directory-file-name project-path))))
      (unless (assoc project-path ecb-source-path)
	(add-to-list 'ecb-source-path (list project-path project-name))))))

;; (defvar default-ecb-source-path nil)
;; (defun add-projectile-project-to-ecb ()
;;   (interactive)
;;   (when (functionp 'projectile-get-project-directories)
;;     (when (projectile-project-p)
;;       (dolist (path-dir (projectile-get-project-directories))
;;         (let ((project-name (file-name-nondirectory (directory-file-name path-dir))))
;;           (unless (member (list path-dir project-name) default-ecb-source-path)
;;             (push (list path-dir project-name) default-ecb-source-path)
;; 	    (customize-set-variable 'ecb-source-path default-ecb-source-path)
;;    ))))))
;; (add-hook 'ecb-basic-buffer-sync-hook 'add-projectile-project-to-ecb)



;; Set auto-save location
(defun set-auto-saves ()
  "Put autosave files (ie #foo#) in one place!"
  (defvar autosave-dir (concat "/tmp/emacs_autosaves/" (user-login-name) "/"))
  (make-directory autosave-dir t)
  (defun auto-save-file-name-p (filename) (string-match "^#.*#$" (file-name-nondirectory filename)))
  (defun make-auto-save-file-name ()
    (concat autosave-dir
       (if buffer-file-name
          (concat "#" (file-name-nondirectory buffer-file-name) "#")
          (expand-file-name (concat "#%" (buffer-name) "#"))))))


;; Automatically scroll buffers that receive messages
(defadvice message (after message-tail activate)
  "Goto point max after a message."
  (with-current-buffer "*Messages*"
    (goto-char (point-max))
    (walk-windows
      (lambda (window)
        (if (string-equal (buffer-name (window-buffer window)) "*Messages*")
            (set-window-point window (point-max))))
      nil
      t)))



;; Switch to desktop from project root
(defvar save-last-project-path "~/.emacs.d/.last-desktop"
  "File name where to store last project info.")
(defun switch-desktop (new-desktop)
  "Switch to other desktop/workspace/project.  NEW-DESKTOP is the path to new desktop."
  (interactive
   (list
    (let ((projects-list (copy-sequence projectile-known-projects)))
      (ido-completing-read "New workspace: " (sort projects-list 'string<)))))
  (desktop-save-in-desktop-dir)
  (ecb-deactivate)
  (desktop-change-dir new-desktop)
  (ecb-activate)
  (write-region new-desktop nil save-last-project-path)
  )

(defun switch-to-last-desktop ()
"Switch to last used desktop/workspace/project."
  (interactive)
  (let ((last-project (get-last-used-desktop)))
    (if last-project
	(progn
	  (message (concat "Activating last project: " last-project)
      (switch-desktop last-project)))
      (message (concat "No last project found in " save-last-project-path)))))

(defun get-last-used-desktop ()
"Return last used desktop/workspace/project."
  (interactive)
  (if (file-readable-p save-last-project-path)
      (with-temp-buffer
        (insert-file-contents save-last-project-path)
        (buffer-string))))


;; Disable spelling inside QQ block, i.e. between "|" and "|]"
(defun flyspell-ignore-quasiquotes ()
  "Function used for `flyspell-generic-check-word-predicate' to ignore QuasiQuote blocks."
  (save-excursion
    (widen)
    (let ((p (point))
	  (f (get-text-property (- (point) 1) 'face)))
      (and (flyspell-generic-progmode-verify)                         ;; is code
	   (not (and (re-search-backward "|"    nil t) (> p (point)) ;; in QQ
		     (re-search-forward  "|\\]" nil t) (< p (point))))))))
      ;; (not (and (not (memq f flyspell-prog-text-faces))
      ;; 	        (and (re-search-backward "|"    nil t) (> p (point))
      ;; 		     (re-search-forward  "|\\]" nil t) (< p (point))))))))


;; Workaround for suggestions blocks if haskell-doc mode is active
(defun haskell-process-trigger-suggestions-ad (orig-fun &rest args)
	"Disable haskell-doc activity.  ORIG-FUN will be executed with specified ARGS."
  (turn-off-haskell-doc)
  (apply orig-fun args)
  (turn-on-haskell-doc))

;; Some usefufl haskell-mode functions. From ChrisDone
(defun haskell-who-calls (&optional prompt)
  "Grep the codebase to see who uses the symbol at point."
  (interactive "P")
  (let ((sym (if prompt
                 (read-from-minibuffer "Look for: ")
               (haskell-ident-at-point))))
    (let ((existing (get-buffer "*who-calls*")))
      (when existing
        (kill-buffer existing)))
    (let ((buffer
           ;; (grep-find (format "cd %s && find . -name '*.hs' -exec grep -inH -e %s {} +"
           (grep-find (format "cd %s && find . -path ./.stack-work -prune -o -name '*.hs' -exec grep -inH -e %s {} +"
                              (haskell-session-current-dir (haskell-session))
                              sym))))
      (with-current-buffer buffer
        (rename-buffer "*who-calls*")
        (switch-to-buffer-other-window buffer)))))

(defun haskell-process-all-types ()
  "List all types in a 'grep-mode' buffer."
  (interactive)
  (let ((session (haskell-session)))
    (switch-to-buffer (get-buffer-create
      (format "*%s:all-types*" (haskell-session-name (haskell-session)))))
    (setq haskell-session session)
    (cd (haskell-session-current-dir session))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (let ((haskell-process-log nil))
        (insert (haskell-process-queue-sync-request (haskell-process) ":all-types")))
      (unless (eq major-mode  'compilation-mode)
        (compilation-mode)
        (setq compilation-error-regexp-alist haskell-compilation-error-regexp-alist)))))


;; Auto-accept default values on interactive mode
;; (fset 'haskell-session-target 'haskell-session-target-no-prompt)


(defun hsession-active ()
"Check if there is an active haskell-session."
  (and (boundp 'haskell-session) haskell-session))


;; Execute main function in REPL buffer with specified ARGS
(defun haskell-session-exec-main (args)
  "Execute main function in REPL buffer with specified ARGS."
  (interactive)
  (let ((buffer (haskell-session-interactive-buffer (haskell-session))))
    (with-current-buffer buffer
      (progn
	(haskell-interactive-mode-clear)
	(insert (concat ":main" " " args))
	(haskell-interactive-mode-return)
	(goto-char (point-max))
	(set-window-point nil (point-max))
	))))



(defvar server-buffers nil)
(defun show-server-buffers ()
"Show buffers with clients."
  (interactive)
  (setq server-buffers nil)
  (let ((original-buffer (current-buffer)))
    (loop for buf in (buffer-list)
      do
			(defvar server-buffer-clients)
      (progn
        (switch-to-buffer buf)
        (if (and
         server-buffer-clients
         (buffer-live-p buf))
        (add-to-list 'server-buffers buf))))
    (switch-to-buffer original-buffer)
    (message "server-buffers: %s" server-buffers)))

;; Multi occur string at point from all buffers with same major mode
(defun get-buffers-matching-mode (mode)
  "Return a list of buffers where their 'major-mode' is equal to MODE."
  (let ((buffer-mode-matches '()))
   (dolist (buf (buffer-list))
     (with-current-buffer buf
       (if (eq mode major-mode)
           (add-to-list 'buffer-mode-matches buf))))
   buffer-mode-matches))

(defun multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (multi-occur
   (get-buffers-matching-mode major-mode)
   (car (occur-read-primary-args))))

(provide 'functions-init)

;;; functions-init ends here
