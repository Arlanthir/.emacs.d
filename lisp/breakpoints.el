;; ------------------------
;; Toggle breakpoint on line number click
;; ------------------------

(defface linum-breakpoint
    `((t :inherit linum
	 :foreground "gray80"
	 :background "RoyalBlue3"))
  "Face for displaying breakpoints in line numbers."
  :group 'linum)

(defvar linum-number-length 1 "Number width for linum.")

(add-hook 'linum-before-numbering-hook
	  (lambda ()
	    (setq linum-number-length
		  (number-to-string (1+ (length (number-to-string (count-lines (point-min) (point-max)))))))))

(defun linum-highlight-breakpoint (line-number)
  (let ((break-text (cond ((or (string= major-mode "js-mode") (string= major-mode "typescript-mode"))
			   "debugger;")
			  ((string= major-mode "lisp-mode")
			   "(break)"))))
    (save-excursion
     (goto-char (point-min))
     (forward-line (1- line-number))
     (let ((line-text (replace-regexp-in-string "\\`[ \t\n]*" "" (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
       (if (and break-text (string= line-text break-text))
	   (propertize (format (concat "%" linum-number-length "d") line-number) 'face 'linum-breakpoint)
	   (propertize (format (concat "%" linum-number-length "d") line-number) 'face 'linum))))))

(setq linum-format 'linum-highlight-breakpoint)

(defun linum-toggle-breakpoint (e)
  (interactive "e")
  (let ((line-number (line-number-at-pos (posn-point (event-end e))))
	(break-text (cond ((or (string= major-mode "js-mode") (string= major-mode "typescript-mode"))
			   "debugger;")
			  ((string= major-mode "lisp-mode")
			   "(break)"))))
    (when break-text
      (save-excursion
       (goto-char (point-min))
       (forward-line (1- line-number))
       (let ((needs-save (buffer-modified-p))
	     (line-text (replace-regexp-in-string "\\`[ \t\n]*" "" (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
	 (if (string= line-text break-text)
	     (delete-region (line-beginning-position) (1+ (line-end-position)))
	     (progn
	       (insert break-text)
	       (reindent-then-newline-and-indent)))
	 (unless needs-save
	   (save-buffer)))))))

(global-set-key (kbd "<left-margin> <mouse-1>") 'linum-toggle-breakpoint)

(provide 'breakpoints)
