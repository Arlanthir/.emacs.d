;; ------------------------
;; Normal Shortcuts Mode
;; ------------------------

(defun ns-simulate-prefix (key)
  (setq this-command last-command)
  (setq emulation-mode-map-alists
	(delq 'ns-map-alist emulation-mode-map-alists))
  ;;(prefix-command-preserve-state)
  (setq unread-command-events
	(cons key unread-command-events)))

(defun ns-close ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
      (kill-buffer)))

(defun ns-copy ()
  (interactive)
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
      (ns-simulate-prefix ?\C-c)))

(defun ns-cut ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
      (ns-simulate-prefix ?\C-x)))

(defun ns-delete-line ()
  (interactive)
  (delete-region (line-beginning-position) (min (1+ (line-end-position)) (point-max))))

(defun ns-new ()
  (interactive)
  (switch-to-buffer (generate-new-buffer-name "new")))

(defun ns-save ()
  (interactive)
  (if (buffer-modified-p)
      (save-buffer)
      (isearch-forward)))

(defvar ns-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-a") 'mark-whole-buffer)
    (define-key map (kbd "C-c") 'ns-copy)
    (define-key map (kbd "C-d") 'ns-delete-line)
    (define-key map (kbd "C-f") 'isearch-forward)
    (define-key map (kbd "C-n") 'ns-new)
    (define-key map (kbd "C-s") 'ns-save)
    (define-key map (kbd "C-t") 'ns-new)
    (define-key map (kbd "C-v") 'yank)
    (global-set-key (kbd "C-w") 'ns-close)
    (define-key map (kbd "C-x") 'ns-cut)
    (define-key map (kbd "C-z") 'undo)
    (define-key map (kbd "<C-tab>") 'other-window)
    (define-key map (kbd "<C-S-tab>") #'(lambda () (interactive) (other-window -1)))
    (define-key map (kbd "C-S-c") #'(lambda () (interactive) (ns-simulate-prefix ?\C-c)))
    (define-key map (kbd "C-S-x") #'(lambda () (interactive) (ns-simulate-prefix ?\C-x)))
    (define-key map [f5] 'revert-buffer)
    (define-key map (kbd "<mouse-3>") 'mouse-major-mode-menu) ; Enable right click menu without Ctrl key
    map))

(defvar ns-map-alist
  `((normal-shortcuts-mode . ,ns-mode-map)))

(defun ns-post-command-handler ()
  (when normal-shortcuts-mode
    (unless (input-pending-p)
      (pushnew 'ns-map-alist emulation-mode-map-alists))))

(define-minor-mode normal-shortcuts-mode
  "Enable normal shortcut keys like C-c to copy, without CUA timers."
  :lighter " NS"
  :global t
  (if normal-shortcuts-mode
      (progn
	(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
	(define-key isearch-mode-map (kbd "C-v") 'isearch-yank-kill)
	(add-hook 'post-command-hook 'ns-post-command-handler)
	(pushnew 'ns-map-alist emulation-mode-map-alists))
      (progn
	(define-key isearch-mode-map (kbd "C-f") nil)
	(define-key isearch-mode-map (kbd "C-v") nil)
	(remove-hook 'post-command-hook 'ns-post-command-handler)
	(setq emulation-mode-map-alists
	      (delq 'ns-map-alist emulation-mode-map-alists)))))

;; Add menu checkbox
(define-key global-map [menu-bar options normal-shortcuts-mode]
  (menu-bar-make-toggle
   toggle-normal-shortcuts-mode normal-shortcuts-mode
    "Normal Shortcuts"
    "Normal Shortcuts %s"
    "Shortcuts like CUA but without timers."))

(provide 'normal-shortcuts)
