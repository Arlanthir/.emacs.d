;; ------------------------
;; Behavior
;; ------------------------

(defconst my-conf-backups-dir "~/.emacs.d/backups/")

(require 'server)
(unless (server-running-p)
  (server-start))


(setq-default indent-tabs-mode t)                 ; Use tabs to indent
(setq-default tab-width 8)                        ; Replace 8 spaces with tabs
;;(defvaralias 'c-basic-offset 'tab-width)
(setq-default fill-column 80)                     ; Set M-q fill width to 80
(setq-default word-wrap t)                        ; Word wrap
;;(setq-default truncate-lines nil)               ; (old?) Word wrap
;;(setq truncate-partial-width-windows nil)       ; (old?) Word wrap on split windows
(setq mouse-drag-copy-region nil)                 ; Don't copy when text is selected with mouse
(delete-selection-mode t)                         ; Delete selected text on 'delete' key
;;(setq initial-major-mode 'emacs-lisp-mode)      ; Force lisp mode
(setq echo-keystrokes 0.1)                        ; Immediately show keyboard shortcuts in the mini-buffer
(electric-pair-mode)                              ; Auto-close parenthesis when they're open
(show-paren-mode t)                               ; Highlight matching bracket
(setq show-paren-delay 0)                         ; Highlight with no delay
(setq show-paren-style 'parenthesis)              ; Highlight just brackets
;(setq show-paren-style 'expression)              ; Highlight entire bracket expression
(setq ediff-split-window-function 'split-window-horizontally) ; Use side-by-side buffers in ediff, comment out if using small screen
(prefer-coding-system 'utf-8)

;; Indentation in specific languages
(add-hook 'html-mode-hook
	  (lambda ()
	    ;; Default indentation is usually 2 spaces, changing to 4.
	    (set (make-local-variable 'sgml-basic-offset) 4)))

(custom-set-variables '(handlebars-basic-offset 4))

;; File backups and auto-saves
(setq make-backup-files t       ; Make backup files
      backup-by-copying t       ; Copy files instead of renaming them
      delete-old-versions t     ; Clean up old backups
      kept-new-versions 6       ; Keep last 6 versions
      kept-old-versions 2       ; Keep first 2 versions
      version-control t         ; Control file versions with numbers
      backup-directory-alist `(("." . ,my-conf-backups-dir))) ; Set backups dir

(setq auto-save-default t            ; Auto save files periodically
      auto-save-interval 1000        ; The number of characters typed before triggering a save (0 = disabled)
      auto-save-timeout 30           ; The number of seconds on idle before triggering a save (0 = disabled)
      delete-auto-save-files t       ; Delete auto saves on manual save
      auto-save-list-file-name (concat my-conf-backups-dir (format ".auto-save-%s" (emacs-pid))) ; List of auto save files
      auto-save-file-name-transforms `((".*" ,my-conf-backups-dir t))) ; Set auto-save dir


;; GREP
(setq null-device "/dev/null") ;; Hide grep unsuccessful attempts
;; Grep options: line numbers, filenames, case insensitive, recursive
;; Only search in .lisp, .bil, .cl. Exclude .git directory.
;; Regex: ignore comments, place your query string after the * symbol in the regex
(custom-set-variables '(grep-command "grep -nHir --include=*.{el,lisp,bil,cl,html,js,ts,css,scss} --exclude-dir={.git,lib,doc} -e \"^[^;]*YOUR_QUERY\" z:/siscog/scs-vdev/scs"))

(require 'recentf) ; Recent Files - Build a list of recent files
(recentf-mode 1)

(require 'ido) ; Ido - Interactively do things
(ido-mode t)   ; Offers suggestions when a command is ran in the mini-buffer

;; Enable additional commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;(require 'uniquify)
;;;; from: http://trey-jackson.blogspot.pt/2008/01/emacs-tip-11-uniquify.html
;;(setq uniquify-buffer-name-style 'reverse)
;;(setq uniquify-separator "/")
;;(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
;;(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers


;; ------------------------
;; Mouse wheel behavior
;; ------------------------

(setq mouse-wheel-scroll-amount '(3 ((shift) . 6)))  ; 3 lines at a time, 6 when holding shift
(setq mouse-wheel-progressive-speed nil)             ; Don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't)                   ; Scroll window under mouse
(setq scroll-step 1)                                 ; Keyboard scroll one line at a time
(setq scroll-conservatively 10000)                   ; Don't become choppy on scroll down after window bottom


;; ------------------------
;; Minibuffer
;; ------------------------

(fset 'yes-or-no-p 'y-or-n-p) ; Ask "y or n" instead of "yes or no"

(provide 'behavior)
