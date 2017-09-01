;; ------------------------
;; Shortcuts
;; ------------------------

(defvar closed-files-list '())

(defun track-closed-file ()
  (and buffer-file-name
       (or (delete buffer-file-name closed-files-list) t)
       (push buffer-file-name closed-files-list)))

(add-hook 'kill-buffer-hook 'track-closed-file)

(defun find-last-killed-file ()
  (interactive)
  (let ((active-files (loop for buf in (buffer-list)
                            when (buffer-file-name buf) collect it)))
    (loop for file in closed-files-list
          unless (member file active-files) return (find-file file))))


(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))


;; TODO Improve this
(defun interactive-ediff-file-with-original ()
  (interactive)
  (if (string-match (format "%s/\\(.+\\)" *default-src-dir*) (buffer-file-name))
      (let ((xfile (format "%s/%s" *default-org-dir* (match-string 1 (buffer-file-name)))))
	(if (file-exists-p xfile)
	    (ediff-file-with-original)
	    (let ((variant (replace-regexp-in-string "\\.ts$" ".js" xfile)))
	      (if (file-exists-p variant)
		  (ediff-files (buffer-file-name) variant)
		  (ediff-file-with-original)))))
      (ediff-file-with-original)))


(defun edit-selected-file ()
  (interactive)
  (flet ((try-to-open-file (path)
	   (if (file-exists-p path)
	       (switch-to-buffer-other-window (find-file-noselect path))
	       (let ((path2 (format "%s/%s" (getenv "SISCOG_DIR") path)))
		 (if (file-exists-p path2)
		     (switch-to-buffer-other-window (find-file-noselect path2))
		     (message (format "ERROR: %s is not a file" path)))))))
    (if (region-active-p)
	(try-to-open-file (buffer-substring (region-beginning) (region-end)))
	(try-to-open-file (thing-at-point 'filename)))))


;; TODO Don't hardcode last versions
(defun ediff-frozen ()
  (interactive)
  (if (string-match (format "%s/\\([^/]+\\)/\\(.+\\)" *default-src-dir*) (buffer-file-name))
      (let* ((version (match-string 1 (buffer-file-name)))
	     (frozen-version (cond ((string= version "siscog-util-vdev")
				    "siscog-util-v2-0-0")
				   ((string= version "scs-vdev")
				    "scs-v9-0-0")
				   ((string= version "scs-siscog-vdev")
				    "scs-siscog-v9-0-0")
				   (t
				    frozen-version)))
	     (frozen-file (format "%s/%s/%s" "x:/crews" frozen-version (match-string 2 (buffer-file-name)))))
	(ediff-files (buffer-file-name) frozen-file))))


;; Searching for modification signature (magic date)

(defun search-mod-date-forward ()
  (interactive)
  (if (search-forward *default-mod-date* nil t)
      (forward-line 2)
      (message "Modification date %s not found forward in buffer." *default-mod-date*)))


;;; Search backward modification date.
(defun search-mod-date-backward ()
  (interactive)
  (unless (search-backward *default-mod-date* nil t)
    (message "Modification date %s not found backward in buffer." *default-mod-date*)))


;; -- / Shortcut functions ---


(global-set-key [remap dabbrev-expand] 'hippie-expand)      ; Use hippie expand instead of dabbrev-expand

(global-set-key (kbd "RET") 'newline-and-indent)            ; Auto-indent

(global-set-key (kbd "C-S-t") 'find-last-killed-file)       ; Reopen last closed filed



(global-set-key (kbd "C-7") 'comment-or-uncomment-region-or-line) ; Toggle comment
(global-set-key (kbd "C-/") 'comment-or-uncomment-region-or-line) ; Toggle comment

(global-set-key (kbd "<C-prior>") 'tabbar-backward)         ; Previous tab
(global-set-key (kbd "<C-next>") 'tabbar-forward)           ; Next tab
(global-set-key (kbd "<C-S-prior>") 'tabbar-move-tab-backward)         ; Move tab left
(global-set-key (kbd "<C-S-next>") 'tabbar-move-tab-forward)           ; Move tab right

(global-set-key [f9] 'ediff-frozen)				; Ediff with frozen version (instead of vdev)
(global-set-key [f10] 'edit-selected-file)			; Open file under mouse cursor, complete path if needed
(global-set-key [f11] 'interactive-ediff-file-with-original)	; Merge with original

(global-set-key [M-down] 'search-mod-date-forward)        ; Next magic date
(global-set-key [M-up] 'search-mod-date-backward)         ; Previous magic date

;; -- Mode-dependent shortcuts --

;; (setq fi:lisp-mode-hook
;;       (function
;;        (lambda ()
;; 	(let ((map (current-local-map)))
;; 	  ;; (define-key map "\C-c." 'find-tag)
;; 	  ;; (define-key map "\C-c," 'tags-loop-continue)
;; 	  (define-key map (kbd "M-.") 'fi:lisp-find-definition)
;; 	  (define-key map ("M-,") 'fi:lisp-find-next-definition)))))


;; -- Ediff keyboard shortcuts --
;;
;; d: copies the current difference region from buffer A to buffer C, and then appends to C the same region from buffer B
;; e: copies the current difference region from buffer B to buffer C, and then appends to C the same region from buffer A
;;
(defun ediff-copy-both-to-C (first second)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference first ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference second ediff-control-buffer))))
(add-hook 'ediff-keymap-setup-hook
	  (lambda ()
	    (define-key ediff-mode-map "d" (lambda () (interactive) (ediff-copy-both-to-C 'A 'B)))
	    (define-key ediff-mode-map "e" (lambda () (interactive) (ediff-copy-both-to-C 'B 'A)))))

(provide 'shortcuts)
