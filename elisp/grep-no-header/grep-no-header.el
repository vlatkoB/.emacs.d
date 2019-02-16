;;; package --- grep-no-header
;;; Commentary:
;;; Code:

;; `rgrep' command without displaying command at the top of buffer

(defun new-compilation-start (command &optional mode name-function highlight-regexp)
  "Run compilation command COMMAND (low level interface).
If COMMAND starts with a cd command, that becomes the `default-directory'.
The rest of the arguments are optional; for them, nil means use the default.

MODE is the major mode to set in the compilation buffer.  Mode
may also be t meaning use `compilation-shell-minor-mode' under `comint-mode'.

If NAME-FUNCTION is non-nil, call it with one argument (the mode name)
to determine the buffer name.  Otherwise, the default is to
reuses the current buffer if it has the proper major mode,
else use or create a buffer with name based on the major mode.

If HIGHLIGHT-REGEXP is non-nil, `next-error' will temporarily highlight
the matching section of the visited source line; the default is to use the
global value of `compilation-highlight-regexp'.

Returns the compilation buffer created."
  (or mode (setq mode 'compilation-mode))
  (let* ((name-of-mode
	  (if (eq mode t)
	      "compilation"
	    (replace-regexp-in-string "-mode\\'" "" (symbol-name mode))))
	 (thisdir default-directory)
	 (thisenv compilation-environment)
	 outwin outbuf)
    (with-current-buffer
	(setq outbuf
	      (get-buffer-create
               (compilation-buffer-name name-of-mode mode name-function)))
      (let ((comp-proc (get-buffer-process (current-buffer))))
      (if comp-proc
          (if (or (not (eq (process-status comp-proc) 'run))
                  (eq (process-query-on-exit-flag comp-proc) nil)
                  (yes-or-no-p
                   (format "A %s process is running; kill it? "
                           name-of-mode)))
              (condition-case ()
                  (progn
                    (interrupt-process comp-proc)
                    (sit-for 1)
                    (delete-process comp-proc))
                (error nil))
            (error "Cannot have two processes in `%s' at once"
                   (buffer-name)))))
      ;; first transfer directory from where M-x compile was called
      (setq default-directory thisdir)
      ;; Make compilation buffer read-only.  The filter can still write it.
      ;; Clear out the compilation buffer.
      (let ((inhibit-read-only t)
	    (default-directory thisdir))
	;; Then evaluate a cd command if any, but don't perform it yet, else
	;; start-command would do it again through the shell: (cd "..") AND
	;; sh -c "cd ..; make"
	(cd (cond
             ((not (string-match "\\`\\s *cd\\(?:\\s +\\(\\S +?\\|'[^']*'\\|\"\\(?:[^\"`$\\]\\|\\\\.\\)*\"\\)\\)?\\s *[;&\n]"
                                 command))
              default-directory)
             ((not (match-end 1)) "~")
             ((eq (aref command (match-beginning 1)) ?\')
              (substring command (1+ (match-beginning 1))
                         (1- (match-end 1))))
             ((eq (aref command (match-beginning 1)) ?\")
              (replace-regexp-in-string
               "\\\\\\(.\\)" "\\1"
               (substring command (1+ (match-beginning 1))
                          (1- (match-end 1)))))
             ;; Try globbing as well (bug#15417).
             (t (let* ((substituted-dir
                        (substitute-env-vars (match-string 1 command)))
                       ;; FIXME: This also tries to expand `*' that were
                       ;; introduced by the envvar expansion!
                       (expanded-dir
                        (file-expand-wildcards substituted-dir)))
                  (if (= (length expanded-dir) 1)
                      (car expanded-dir)
                    substituted-dir)))))
	(erase-buffer)
	;; Select the desired mode.
	(if (not (eq mode t))
            (progn
              (buffer-disable-undo)
              (funcall mode))
	  (setq buffer-read-only nil)
	  (with-no-warnings (comint-mode))
	  (compilation-shell-minor-mode))
        ;; Remember the original dir, so we can use it when we recompile.
        ;; default-directory' can't be used reliably for that because it may be
        ;; affected by the special handling of "cd ...;".
        ;; NB: must be done after (funcall mode) as that resets local variables
        (set (make-local-variable 'compilation-directory) thisdir)
	(set (make-local-variable 'compilation-environment) thisenv)
	(if highlight-regexp
	    (set (make-local-variable 'compilation-highlight-regexp)
		 highlight-regexp))
        (if (or compilation-auto-jump-to-first-error
		(eq compilation-scroll-output 'first-error))
            (set (make-local-variable 'compilation-auto-jump-to-next) t))
	;; Output a mode setter, for saving and later reloading this buffer.
	(insert ;; "-*- mode: " name-of-mode
		"directory: "
                (prin1-to-string (abbreviate-file-name default-directory))
		;; " -*-\n"
		;; (format "%s started at %s\n\n"
		;; 	mode-name
		;; 	(substring (current-time-string) 0 19))
		;; command "\n")
		"\n")
	(setq thisdir default-directory))
      (set-buffer-modified-p nil))
    ;; Pop up the compilation buffer.
    ;; http://lists.gnu.org/archive/html/emacs-devel/2007-11/msg01638.html
    (setq outwin (display-buffer outbuf '(nil (allow-no-window . t))))
    (with-current-buffer outbuf
      (let ((process-environment
	     (append
	      compilation-environment
	      (if (if (boundp 'system-uses-terminfo);`If' for compiler warning.
		      system-uses-terminfo)
		  (list "TERM=dumb" "TERMCAP="
			(format "COLUMNS=%d" (window-width)))
		(list "TERM=emacs"
		      (format "TERMCAP=emacs:co#%d:tc=unknown:"
			      (window-width))))
	      ;; Set the EMACS variable, but
	      ;; don't override users' setting of $EMACS.
	      (unless (getenv "EMACS")
		(list "EMACS=t"))
	      (list "INSIDE_EMACS=t")
	      (copy-sequence process-environment))))
	(set (make-local-variable 'compilation-arguments)
	     (list command mode name-function highlight-regexp))
	(set (make-local-variable 'revert-buffer-function)
	     'compilation-revert-buffer)
	(and outwin
	     ;; Forcing the window-start overrides the usual redisplay
	     ;; feature of bringing point into view, so setting the
	     ;; window-start to top of the buffer risks losing the
	     ;; effect of moving point to EOB below, per
	     ;; compilation-scroll-output, if the command is long
	     ;; enough to push point outside of the window.  This
	     ;; could happen, e.g., in `rgrep'.
	     (not compilation-scroll-output)
	     (set-window-start outwin (point-min)))

	;; Position point as the user will see it.
	(let ((desired-visible-point
	       ;; Put it at the end if `compilation-scroll-output' is set.
	       (if compilation-scroll-output
		   (point-max)
		 ;; Normally put it at the top.
		 (point-min))))
	  (goto-char desired-visible-point)
	  (when (and outwin (not (eq outwin (selected-window))))
	    (set-window-point outwin desired-visible-point)))

	;; The setup function is called before compilation-set-window-height
	;; so it can set the compilation-window-height buffer locally.
	(if compilation-process-setup-function
	    (funcall compilation-process-setup-function))
	(and outwin (compilation-set-window-height outwin))
	;; Start the compilation.
	(if (fboundp 'start-process)
	    (let ((proc
		   (if (eq mode t)
		       ;; comint uses `start-file-process'.
		       (get-buffer-process
			(with-no-warnings
			  (comint-exec
			   outbuf (downcase mode-name)
			   (if (file-remote-p default-directory)
			       "/bin/sh"
			     shell-file-name)
			   nil `("-c" ,command))))
		     (start-file-process-shell-command (downcase mode-name)
						       outbuf command))))
              ;; Make the buffer's mode line show process state.
              (setq mode-line-process
                    '(:propertize ":%s" face compilation-mode-line-run))

              ;; Set the process as killable without query by default.
              ;; This allows us to start a new compilation without
              ;; getting prompted.
              (when compilation-always-kill
                (set-process-query-on-exit-flag proc nil))

              (set-process-sentinel proc 'compilation-sentinel)
              (unless (eq mode t)
                ;; Keep the comint filter, since it's needed for proper
		;; handling of the prompts.
		(set-process-filter proc 'compilation-filter))
	      ;; Use (point-max) here so that output comes in
	      ;; after the initial text,
	      ;; regardless of where the user sees point.
	      (set-marker (process-mark proc) (point-max) outbuf)
	      (when compilation-disable-input
		(condition-case nil
		    (process-send-eof proc)
		  ;; The process may have exited already.
		  (error nil)))
	      (run-hook-with-args 'compilation-start-hook proc)
              (setq compilation-in-progress
		    (cons proc compilation-in-progress)))
	  ;; No asynchronous processes available.
	  (message "Executing `%s'..." command)
	  ;; Fake mode line display as if `start-process' were run.
	  (setq mode-line-process
		'(:propertize ":run" face compilation-mode-line-run))
	  (force-mode-line-update)
	  (sit-for 0)			; Force redisplay
	  (save-excursion
	    ;; Insert the output at the end, after the initial text,
	    ;; regardless of where the user sees point.
	    (goto-char (point-max))
	    (let* ((inhibit-read-only t) ; call-process needs to modify outbuf
		   (compilation-filter-start (point))
		   (status (call-process shell-file-name nil outbuf nil "-c"
					 command)))
	      (run-hooks 'compilation-filter-hook)
	      (cond ((numberp status)
		     (compilation-handle-exit
		      'exit status
		      (if (zerop status)
			  "finished\n"
			(format "exited abnormally with code %d\n" status))))
		    ((stringp status)
		     (compilation-handle-exit 'signal status
					      (concat status "\n")))
		    (t
		     (compilation-handle-exit 'bizarre status status)))))
	  (set-buffer-modified-p nil)
	  (message "Executing `%s'...done" command)))
      ;; Now finally cd to where the shell started make/grep/...
      (setq default-directory thisdir)
      ;; The following form selected outwin ever since revision 1.183,
      ;; so possibly messing up point in some other window (bug#1073).
      ;; Moved into the scope of with-current-buffer, though still with
      ;; complete disregard for the case when compilation-scroll-output
      ;; equals 'first-error (martin 2008-10-04).
      (when compilation-scroll-output
	(goto-char (point-max))))

    ;; Make it so the next C-x ` will use this buffer.
    (setq next-error-last-buffer outbuf)))

(defun mgrep (regexp &optional files dir confirm)
  "Recursively grep for REGEXP in FILES in directory tree rooted at DIR.
The search is limited to file names matching shell pattern FILES.
FILES may use abbreviations defined in `grep-files-aliases', e.g.
entering `ch' is equivalent to `*.[ch]'.

With \\[universal-argument] prefix, you can edit the constructed shell command line
before it is executed.
With two \\[universal-argument] prefixes, directly edit and run `grep-find-command'.

Collect output in a buffer.  While the recursive grep is running,
you can use \\[next-error] (M-x next-error), or \\<grep-mode-map>\\[compile-goto-error] \
in the grep output buffer,
to visit the lines where matches were found.  To kill the job
before it finishes, type \\[kill-compilation].

This command shares argument histories with \\[lgrep] and \\[grep-find].

When called programmatically and FILES is nil, REGEXP is expected
to specify a command to run."
  (interactive
   (progn
     (grep-compute-defaults)
     (cond
      ((and grep-find-command (equal current-prefix-arg '(16)))
       (list (read-from-minibuffer "Run: " grep-find-command
				   nil nil 'grep-find-history)))
      ((not grep-find-template)
       (error "grep.el: No `grep-find-template' available"))
      (t (let* ((regexp (grep-read-regexp))
		(files (grep-read-files regexp))
		(dir (read-directory-name "Base directory: "
					  nil default-directory t))
		(confirm (equal current-prefix-arg '(4))))
	   (list regexp files dir confirm))))))
  (when (and (stringp regexp) (> (length regexp) 0))
    (unless (and dir (file-directory-p dir) (file-readable-p dir))
      (setq dir default-directory))
    (if (null files)
	(if (not (string= regexp (if (consp grep-find-command)
				     (car grep-find-command)
				   grep-find-command)))
	    (compilation-start regexp 'grep-mode))
      (setq dir (file-name-as-directory (expand-file-name dir)))
      (require 'find-dired)		; for `find-name-arg'
      (let ((command (grep-expand-template
		      grep-find-template
		      regexp
		      (concat (shell-quote-argument "(")
			      " " find-name-arg " "
			      (mapconcat
			       #'shell-quote-argument
			       (split-string files)
			       (concat " -o " find-name-arg " "))
			      " "
			      (shell-quote-argument ")"))
		      dir
		      (concat
		       (and grep-find-ignored-directories
			    (concat "-type d "
				    (shell-quote-argument "(")
				    ;; we should use shell-quote-argument here
				    " -path "
				    (mapconcat
				     #'(lambda (ignore)
					 (cond ((stringp ignore)
						(shell-quote-argument
						 (concat "*/" ignore)))
					       ((consp ignore)
						(and (funcall (car ignore) dir)
						     (shell-quote-argument
						      (concat "*/"
							      (cdr ignore)))))))
				     grep-find-ignored-directories
				     " -o -path ")
				    " "
				    (shell-quote-argument ")")
				    " -prune -o "))
		       (and grep-find-ignored-files
			    (concat (shell-quote-argument "!") " -type d "
				    (shell-quote-argument "(")
				    ;; we should use shell-quote-argument here
				    " -name "
				    (mapconcat
				     #'(lambda (ignore)
					 (cond ((stringp ignore)
						(shell-quote-argument ignore))
					       ((consp ignore)
						(and (funcall (car ignore) dir)
						     (shell-quote-argument
						      (cdr ignore))))))
				     grep-find-ignored-files
				     " -o -name ")
				    " "
				    (shell-quote-argument ")")
				    " -prune -o "))))))
	(when command
	  (if confirm
	      (setq command
		    (read-from-minibuffer "Confirm: "
					  command nil nil 'grep-find-history))
	    (add-to-history 'grep-find-history command))
	  (let ((default-directory dir))
	    (new-compilation-start command 'grep-mode))
	  ;; Set default-directory if we started rgrep in the *grep* buffer.
	  (if (eq next-error-last-buffer (current-buffer))
	      (setq default-directory dir)))))))

(provide 'grep-no-header)

;;; grep-no-header ends here
