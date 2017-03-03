;; -----------------------------------------------------------------------------
;; .emacs configuration
;; Version 0.9
;; Miguel Branco
;;
;; Major customizations:
;;
;;   - Appearance changes
;;   - Save open files between sessions
;;   - Backup and auto-save inside .emacs.d
;;   - Add MELPA packages:
;;     - autocomplete
;;     - ac-slime (autocomplete for slime)
;;     - atom dark theme
;;     - flycheck
;;     - handlebars-mode
;;     - hideshowviz (code folding indicators)
;;     - markdown-mode
;;     - multiple-cursors
;;     - neotree (file browser pane)
;;     - scss-mode
;;     - tabbar (tabbed windows)
;;     - typescript-mode
;;     - undo-tree
;;     - web-mode
;;   - Add numerous shortcuts
;;
;; TODO:
;;   - Test ac-js2 (autocomplete for javascript)
;;   - Draw smarter whitespace: tabs and spaces before first word; trailing whitespace
;;   - "Save file as" using the GUI (without ido-write-file)
;;   - Move tabs Rato (parece complicado)
;;     - Melhorar o atalho de move quando queremos passar o ultimo file para o inicio da lista
;;   - Adicionar entrada de menu para editar o .emacs
;;
;; -----------------------------------------------------------------------------

(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'cl) ; Common Lisp functions
(load-library "ediff")


;; ------------------------
;; Configuration Constants
;; ------------------------

(defconst my-conf-load-siscog t)                      ; Whether to load SISCOG libs
(defconst my-conf-backups-dir "~/.emacs.d/backups/")




;; ------------------------
;; Startup
;; ------------------------

(setq inhibit-startup-message t)                       ; Hide startup message
;;(setq initial-scratch-message nil)                     ; Hide first scratch message
;;(setq initial-frame-alist '((fullscreen . maximized))) ; Maximize (Win 8)
(defun maximize-frame ()                               ; Maximize (Win 7)
  (interactive)
  (w32-send-sys-command #xf030)) ; f030 = 61488
(add-hook 'window-setup-hook 'maximize-frame t)        ; /Maximize (Win 7)

(require 'desktop)                              ; Save desktop between sessions (package)
(setq desktop-save t                            ; Save desktop between sessions
      desktop-load-locked-desktop t             ; Load locked desktop files
      desktop-path '("~/.emacs.d/")             ; Desktop sessions directory
      desktop-dirname "~/.emacs.d/"             ; Desktop sessions directory
      desktop-base-file-name ".desktop"         ; Desktop file
      desktop-base-lock-name ".desktop.lock")   ; Locked desktop file
(desktop-save-mode)




;; ------------------------
;; SISCOG
;; ------------------------

(when my-conf-load-siscog
  ;; keywords --> https://wiki/siscog/DictionaryPhrasingRulesforApplications
  ;; https://wiki/siscog/ECL

  (setenv "CYGWIN" "nodosfilewarning") ; for grep
  (setenv "CREWS_DIR" "y:/siscog")
  (setenv "SISCOG_DIR" "z:/siscog")
  ;; (setenv "SISCOG_EMACS_DIR" "z:/siscog/sc-slime")
  ;; (setenv "SISCOG_EMACS_DIR_LOCAL" "w:/home")
  (setenv "SISCOG_UTIL_DIR" "z:/siscog/siscog-util-vdev")

  ;; ------------------------
  ;; SC-Emacs
  ;; ------------------------

  (setenv "SISCOG_EMACS_DIR" "z:/siscog/sc-emacs")

  (load (expand-file-name "init.el" (getenv "SISCOG_EMACS_DIR")))
  (load (expand-file-name "~/sc-user-param.el"))
  ;; (load (format "z:/home/sc-user-param-example.el"))
  ;;(load (format "%s/init.el" (getenv "SISCOG_EMACS_DIR")))

  ;; ------------------------
  ;; Slime
  ;; ------------------------

  (add-to-list 'load-path "z:/siscog/slime")
  (require 'slime-autoloads)
  (add-to-list 'slime-contribs 'slime-fancy)
  (add-to-list 'slime-contribs 'slime-indentation)

  ;; ------------------------
  ;; SC-Sly
  ;; ------------------------

  ;; Setup simple do sc-sly, a cola entre SC-Emacs e Slime
  (add-to-list 'load-path "z:/siscog/sc-sly")
  (require 'siscog-sly)

  ;; And now other stuff
  (remove-hook 'sc-startup-hook (first (last sc-startup-hook)))
  (add-hook 'sc-startup-hook
	    (lambda () (sc-set-acl-version :v9-0-64 :no-registry))
	    :append))

(global-set-key "\C-cs" 'slime-selector)




;; ------------------------
;; Appearance
;; ------------------------

(require 'linum)                       ; Show line numbers (package)
(global-linum-mode t)                  ; Show line numbers
(setq frame-title-format "%b - %f")    ; Show filename in frame title
(setq split-height-threshold 100)       ; Minimum window height when splitting
(setq split-width-threshold 180)       ; Minimum window width when splitting
(setq-default cursor-type 'bar)        ; Change cursor to a line instead of full character

(require 'whitespace)                                 ; Show whitespace
;; Default options: '(face tabs spaces trailing lines space-before-tab newline
;;                    indentation empty space-after-tab space-mark tab-mark newline-mark))
(setq whitespace-style '(face trailing))  ; Show only some whitespace
(global-whitespace-mode t)             ; Enable globally for all buffers

;; (defvar epo1-whitespace 'epo1-whitespace)
;; (defface epo1-whitespace
;;   '((t :foreground "#444444"))
;;   "Face for whitespace.")

;; (dotimes (i 8)
;;   (font-lock-add-keywords 'emacs-lisp-mode
;; 			  `((,(concat "^" (make-string i ?\s) "\\( \\)")
;; 			     (1 epo1-whitespace t)
;; 			     (1 (compose-region
;; 				 (match-beginning 1)
;; 				 (match-end 1)
;; 				 (char-to-string ?\xB7) nil))))))


;; (defvar epo1-error 'epo1-error)
;; (defface epo1-error
;;   '((t :foreground "red2"))
;;   "Face for error text.")


(mapc (lambda (mode)
	(font-lock-add-keywords mode '(("TODO" (0 font-lock-warning-face t)))))
      '(js-mode emacs-lisp-mode lisp-mode web-mode))



;; Hide non-text UI
;; (menu-bar-mode -1)
(tool-bar-mode -1)
;; (scroll-bar-mode -1)
;; (tooltip-mode -1)
;; (blink-cursor-mode 0)

(global-hl-line-mode 1)                            ; Highlight current line
(defconst my-conf-override-cur-line-bg "grey10")   ; Current line bg color (nil for theme default)
(defconst my-conf-override-region-bg "SteelBlue4") ; Marked region bg color (nil for theme default)

(defconst my-conf-tab-fg-color "grey85")          ; Tabbed windows fg color, suggestion: black
(defconst my-conf-tab-bg-color "grey40")          ; Tabbed windows bg color, suggestion: grey80
(defconst my-conf-tab-fg-hover "white")           ; Tabbed windows hover fg color, suggestion: black
(defconst my-conf-tab-bg-hover "grey25")          ; Tabbed windows hover bg color, suggestion: grey75
(defconst my-conf-tab-current-fg-color "white")   ; Tabbed windows current fg color, suggestion: grey20
(defconst my-conf-tab-current-bg-color "#1d1f21") ; Tabbed windows current bg color, suggestion: grey95
(defconst my-conf-tab-separator-color "#1d1f21")  ; Tabbed windows separator color, suggestion: grey56
(defconst my-conf-tab-padding 5)                  ; Tabbed windows padding, suggestion: 3

;; Other Emacs colors: http://web.ics.purdue.edu/~cs240/misc/emacs_colors.html
;;                     http://raebear.net/comp/emacscolors.html




;; ------------------------
;; Behavior
;; ------------------------

(server-mode) ; Allow git to request a commit message without launching a new process

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
(custom-set-variables '(grep-command "grep -nHir --include=*.{el,lisp,bil,cl,html,js,css,scss} --exclude-dir={.git,lib,doc} -e \"^[^;]*YOUR_QUERY\" z:/siscog/scs-vdev/scs"))

(require 'recentf) ; Recent Files - Build a list of recent files
(recentf-mode 1)

(require 'ido) ; Ido - Interactively do things
(ido-mode t)   ; Offers suggestions when a command is ran in the mini-buffer

;; Enable additional commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;; for grep...
;;(setq null-device "/dev/null") ; see also: http://jpstup.blogspot.pt/2011/02/getting-emacss-rgrep-working-in-windows.html
;;(defadvice grep-compute-defaults (around grep-compute-defaults-advice-null-device)
;;  "Use cygwin's /dev/null as the null-device."
;;  (let ((null-device "/dev/null"))
;;    ad-do-it))
;;(ad-activate 'grep-compute-defaults)

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
;; Mode Line
;; ------------------------

(line-number-mode 1)               ; Show line number
(column-number-mode 1)             ; Show column number
(display-time)                     ; Show time
(setq display-time-24hr-format t)  ; Show time in 24h format


;; ------------------------
;; Minibuffer
;; ------------------------

(fset 'yes-or-no-p 'y-or-n-p) ; Ask "y or n" instead of "yes or no"




;; ------------------------
;; Packages
;; ------------------------

(require 'package)
;; (add-to-list 'package-archives
;; 	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(defvar my-packages
  '(ac-slime
    auto-complete
    atom-dark-theme
    flycheck
    ;;handlebars-mode
    hideshowvis
    markdown-mode
    multiple-cursors
    neotree
    scss-mode
    tabbar
    ;;typescript-mode
    undo-tree
    web-mode))
    ;; tabbar-ruler

(let ((fresh-packages nil))
  (unless package-archive-contents
    (package-refresh-contents)
    (setf fresh-packages t))
  (dolist (package my-packages)
    (unless (package-installed-p package)
      (unless fresh-packages
	(package-refresh-contents)
	(setf fresh-packages t))
      (ignore-errors (package-install package)))))




;; ------------------------
;; Theme
;; ------------------------
(load-theme 'atom-dark t)

;; Override some colors
(when my-conf-override-cur-line-bg
  (set-face-background 'hl-line my-conf-override-cur-line-bg)) ; Current line bg color
(when my-conf-override-region-bg
  (set-face-background 'region my-conf-override-region-bg))  ; Selected region bg color

;; Fix ediff colors
(set-face-background ediff-fine-diff-face-A "#331111")
(set-face-background ediff-fine-diff-face-B "#113311")
(set-face-background ediff-current-diff-face-C "#665022")
(set-face-foreground ediff-odd-diff-face-A "black")
(set-face-foreground ediff-even-diff-face-B "black")
(set-face-foreground ediff-odd-diff-face-C "black")

;; Set font
;;(set-face-attribute 'default nil :family "Courier New" :height 100) ;; Default
(set-face-attribute 'default nil :family "Consolas" :height 100)

;(set-face-foreground 'show-paren-mismatch-face "darkseagreen2")
;(set-face-background 'show-paren-mismatch-face "red")
;(set-face-background 'show-paren-match-face "wheat1")

;; Whitespace colors
(set-face-attribute whitespace-tab nil :foreground "#444444" :background (face-attribute 'default :background))
(set-face-attribute 'whitespace-space nil :foreground "#444444" :background (face-attribute 'default :background))
(set-face-attribute 'whitespace-hspace nil :foreground "#444444" :background (face-attribute 'default :background))
(set-face-attribute 'whitespace-trailing nil :foreground "#ff0000" :background (face-attribute 'default :background))




;; ------------------------
;; Auto-complete
;; ------------------------

(require 'auto-complete-config)
(ac-config-default)
(ac-set-trigger-key "C-SPC")        ; Change trigger key
;;(setq ac-auto-start nil)          ; Don't auto suggest




;; ------------------------
;; Auto-complete Slime
;; ------------------------

;; From: https://github.com/purcell/ac-slime
(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
                 '(add-to-list 'ac-modes 'slime-repl-mode))




;; ------------------------
;; Flycheck
;; ------------------------

;; Remember to install eslint:
;; npm install -g eslint

(eval-after-load 'flycheck
  '(progn

    (flycheck-def-config-file-var flycheck-sass-lintyml sass-lint ".sass-lint.yml"
     :safe #'stringp)

    (flycheck-define-checker sass-lint
     "A SASS (SCSS) checker using Sass Lint (on Node.js).
See URL `https://github.com/sasstools/sass-lint'."
     :command ("sass-lint"
	       "--verbose"
	       "--format" "checkstyle"
	       (config-file "--config" flycheck-sass-lintyml)
	       source)
     :error-parser flycheck-parse-checkstyle
     :modes (sass-mode scss-mode))

    (pushnew 'sass-lint flycheck-checkers)

    (setq flycheck-eslintrc "Z:/siscog/scs-vdev/task-runner/.eslintrc")
    (setq flycheck-javascript-eslint-executable "Z:/siscog/scs-vdev/task-runner/node_modules/.bin/eslint.cmd")

    (setq flycheck-sass-lintyml "Z:/siscog/scs-vdev/task-runner/.sass-lint.yml")
    (setq flycheck-sass-lint-executable "Z:/siscog/scs-vdev/task-runner/node_modules/.bin/sass-lint.cmd")

    (setq flycheck-xml-parser 'flycheck-parse-xml-region)
    (setq-default flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc))))


(add-hook 'after-init-hook #'global-flycheck-mode)




;; ------------------------
;; Hideshow / Hideshowvis
;; ------------------------

(require 'hideshowvis)
(hideshowvis-symbols)
(add-hook 'prog-mode-hook #'hs-minor-mode)
(add-hook 'prog-mode-hook #'hideshowvis-minor-mode)
(set-face-attribute 'hs-face nil :background (face-attribute 'match :background))



;; ------------------------
;; Multiple Cursors
;; ------------------------

(require 'multiple-cursors)

(global-set-key (kbd "<C-S-up>") 'mc/mmlte--up)                ; Add cursor above
(global-set-key (kbd "<C-S-down>") 'mc/mmlte--down)            ; Add cursor below
(global-set-key (kbd "<mouse-1>") #'(lambda (e) (interactive "e") (mc/keyboard-quit) (mouse-set-point e)))  ; Cancel cursors on mouse click
(global-unset-key (kbd "C-<down-mouse-1>"))                    ; Disable buffer menu and instead use multiple cursors
(global-set-key (kbd "C-<mouse-1>") 'mc/add-cursor-on-click)   ; Add cursor at click
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-beginnings-of-lines)  ; Cursor for each line in marked region
;;(global-set-key (kbd "C-S-c C-S-c") 'mc/mark-all-in-region)  ; Search in region for something and place cursors in each




;; ------------------------
;; Neotree - File Browser
;; ------------------------

(require 'neotree)
(setq neo-smart-open t)
(global-set-key (kbd "C-\\") 'neotree-toggle)




;; ------------------------------------------------------------------------------
;; Tabbed Windows
;; ------------------------------------------------------------------------------

(require 'tabbar)

(set-face-attribute 'tabbar-default nil           ; Default tab fonts and colors
		    :inherit nil
		    :family "Consolas"
		    :height 98
		    :width 'normal
		    :weight 'ultra-light
		    :slant 'normal
		    :underline nil
		    :overline my-conf-tab-bg-color
		    :strike-through nil
		    ;; :inverse-video nil         ; Inherit from frame
		    :foreground my-conf-tab-fg-color
		    :background my-conf-tab-bg-color
		    :box nil
		    :stipple nil)

(set-face-attribute 'tabbar-unselected nil        ; Unselected tabs fonts and colors
 		    :inherit 'tabbar-default
 		    :foreground my-conf-tab-fg-color
 		    :background my-conf-tab-bg-color
 		    :box `(:line-width ,my-conf-tab-padding :color ,my-conf-tab-bg-color :style nil))

(defface tabbar-unselected-highlight              ; Unselected tabs mouse hover
  `((t :inherit tabbar-unselected
       :foreground ,my-conf-tab-fg-hover
       :background ,my-conf-tab-bg-hover
       :box (:line-width ,my-conf-tab-padding :color ,my-conf-tab-bg-hover :style nil)))
  ""
  :group 'tabbar)

(defface tabbar-unselected-modified               ; Unselected tabs when modified
  `((t :inherit tabbar-unselected
       :weight ultra-bold
       :foreground ,my-conf-tab-fg-hover))
  ""
  :group 'tabbar)

(set-face-attribute 'tabbar-selected nil          ; Current tab fonts and colors
		    :inherit 'tabbar-default
		    :overline my-conf-tab-current-bg-color
		    :foreground my-conf-tab-current-fg-color
		    :background my-conf-tab-current-bg-color
		    :box `(:line-width ,my-conf-tab-padding :color ,my-conf-tab-current-bg-color :style nil))

(defface tabbar-selected-highlight                ; Current tab mouse hover
  '((t :inherit tabbar-selected))
  ""
  :group 'tabbar)

(defface tabbar-selected-modified                 ; Current tab when modified
  '((t :inherit tabbar-selected
       :overline t
       :weight ultra-bold))
  ""
  :group 'tabbar)

(set-face-attribute 'tabbar-button nil            ; Home / Left / Right buttons
		    :inherit 'tabbar-default
		    :box nil)

(set-face-attribute 'tabbar-button-highlight nil  ; Buttons mouse hover
		    :inherit 'tabbar-button
		    :background my-conf-tab-fg-hover
		    :background my-conf-tab-bg-hover)

(set-face-attribute 'tabbar-separator nil         ; Separator between tabs
		    :inherit 'tabbar-default
		    :foreground my-conf-tab-separator-color
		    :background my-conf-tab-separator-color
		    :height 1.0)

(setq tabbar-buffer-home-button '(("") ""))       ; Hide home button
(setq tabbar-scroll-left-button '(("") ""))       ; Hide left button
(setq tabbar-scroll-right-button '(("") ""))      ; Hide right button

(defsubst tabbar-line-tab (tab)
  "Returns the display representation of a tab.
	That is, a propertized string used as an `header-line-format' template element.
    Call `tabbar-tab-label-function' to obtain a label for the tab."
  (let* ((selected-p (tabbar-selected-p tab (tabbar-current-tabset)))
	 (modified-p (buffer-modified-p (tabbar-tab-value tab)))
	 (label (if tabbar-tab-label-function (funcall tabbar-tab-label-function tab) tab))
	 (face (if selected-p
		   (if modified-p 'tabbar-selected-modified 'tabbar-selected)
		 (if modified-p 'tabbar-unselected-modified 'tabbar-unselected)))
	 (mouse-face (if selected-p 'tabbar-selected-highlight 'tabbar-unselected-highlight))
 	 (display-label
 	  (propertize label
 		      'tabbar-tab tab
 		      'face face
 		      'mouse-face mouse-face
 		      'pointer 'arrow
 		      'help-echo 'tabbar-help-on-tab
 		      'local-map (tabbar-make-tab-keymap tab))))
    (concat display-label tabbar-separator-value)))


(defun tabbar-line-button (name)
  "Returns the display representation of a button.
    That is, a propertized string used as an `header-line-format' template element.
    Call `tabbar-button-label-function' to obtain a label for the button."
  (let ((label (if tabbar-button-label-function (funcall tabbar-button-label-function name) (cons name name)))) ; (enabled . disabled)
    ;; cache the display value of the enabled/disabled buttons in variable `tabbar-NAME-button-value'
    (set (intern (format "tabbar-%s-button-value" name))
 	 (cons
 	  ;; enabled
 	  (propertize (car label)
 		      'tabbar-button name
		      'face 'tabbar-button
 		      'mouse-face 'tabbar-button-highlight
 		      'pointer 'arrow
 		      'help-echo 'tabbar-help-on-button
 		      'local-map (tabbar-make-button-keymap name))
 	  ;; disabled
	  (propertize (cdr label)
		      'face 'tabbar-button
 		      'pointer 'arrow)))))

;; (defun tabbar-buffer-list ()
;;   "Returns the list of buffers to show in the tab bar.
;;     Exclude all buffers whose name starts with ' ' or '*'.
;;     The current buffer is always included."
;;   (remove-if (lambda (buffer)
;;  	       (if (eq buffer (current-buffer))
;;  		   nil
;;  		 (find (aref (buffer-name buffer) 0) " *")))
;;  	     (buffer-list)))

(defun tabbar-buffer-list ()
  "Returns the list of buffers to show in the tab bar.
    Exclude some special buffers beginning with *.
    The current buffer is always included."
  (remove-if (lambda (buffer)
 	       (if (eq buffer (current-buffer))
 		   nil
 		 (or (string= (buffer-name buffer) "*Messages*")
		     (string= (buffer-name buffer) "*scratch*")
		     (string= (buffer-name buffer) "*slime-events*")
		     (string-prefix-p " *" (buffer-name buffer)))))
 	     (buffer-list)))

(defun tabbar-buffer-groups ()
  "Returns the list of groups the current buffer belongs to.
    Use the same group for all buffers."
  '("all"))


(defun tabbar-help-on-tab (window object position)
  "Returns the help string shown when mouse-hovering a tab.
    Return the absolute file name of the file the buffer is visiting."
  (with-selected-window window
    (let ((tab (get-text-property position 'tabbar-tab object)))
      (buffer-file-name (get-buffer (buffer-name (tabbar-tab-value tab)))))))


(defun tabbar-help-on-button (window object position)
  "Returns the help string shown when mouse-hovering a button.
    No help available."
  (with-selected-window window
    nil))


(defun update-tab-bar (&rest args)
  "Updates the tab bar."
  (tabbar-set-template tabbar-current-tabset nil)
  (tabbar-display-update))

(defun tabbar-move-tab-backward ()
  "Moves current tab to a previous position."
  (interactive)
  (let* ((current-tabset (funcall tabbar-current-tabset-function))
	 (current-tabs (tabbar-tabs current-tabset))
	 (current-tab (tabbar-selected-tab current-tabset))
	 (current-index (position current-tab (tabbar-tabs current-tabset)))
	 (other-index (1- (if (> current-index 0) current-index (length current-tabs)))))
    (rotatef (nth current-index current-tabs)
	     (nth other-index current-tabs))
    ;; Refresh tabset
    (tabbar-set-template current-tabset nil)))

(defun tabbar-move-tab-forward ()
  "Moves current tab to a previous position."
  (interactive)
  (let* ((current-tabset (funcall tabbar-current-tabset-function))
	 (current-tabs (tabbar-tabs current-tabset))
	 (current-tab (tabbar-selected-tab current-tabset))
	 (current-index (position current-tab (tabbar-tabs current-tabset)))
	 (other-index (if (< current-index (1- (length current-tabs))) (1+ current-index) 0)))
    (rotatef (nth current-index current-tabs)
	     (nth other-index current-tabs))
    ;; Refresh tabset
    (tabbar-set-template current-tabset nil)))

;; (defun tabbar-my-mouse-down-callback (event)
;;   (interactive "@e")
;;   (message "woooow entrei no handler")
;;   (message (format "%s" event)))

;; (defun tabbar-make-mouse-keymap (callback)
;;   "Return a keymap that call CALLBACK on mouse events.
;; CALLBACK is passed the received mouse event."
;;   (message "DENTRO DO MEU TABBAR-MAKE-MOUSE-KEYMAP")
;;   (let ((keymap (make-sparse-keymap)))
;;     ;; Pass mouse-1, mouse-2 and mouse-3 events to CALLBACK.
;;     (define-key keymap [header-line down-mouse-1] 'tabbar-my-mouse-down-callback)
;;     (define-key keymap [header-line mouse-1] callback)
;;     (define-key keymap [header-line up-mouse-1] 'tabbar-my-mouse-down-callback)
;;     (define-key keymap [header-line down-mouse-2] 'ignore)
;;     (define-key keymap [header-line mouse-2] callback)
;;     (define-key keymap [header-line down-mouse-3] 'ignore)
;;     (define-key keymap [header-line mouse-3] callback)
;;     keymap))

;; ;; Refresh click handlers
;; (defconst tabbar-default-button-keymap
;;   (tabbar-make-mouse-keymap 'tabbar-select-button-callback)
;;   "Default keymap of a button.")

;; (defconst tabbar-header-line-format '(:eval (tabbar-line))
;;   "The tab bar header line format.")

;; ;; TODO check why tabbar-line is not getting updated

;; Update the tab bar whenever the current buffer is modified or saved
(add-hook 'after-change-functions 'update-tab-bar t)
(add-hook 'after-save-hook 'update-tab-bar t)

;;(setq tabbar-use-images nil)       ; Don't use images for the buttons, speed up

(tabbar-mode t)

;; -- / Tabbed windows ----------------------------------------------------------




;; ------------------------
;; Undo Tree
;; ------------------------

(require 'undo-tree)
(global-undo-tree-mode)
(global-set-key (kbd "C-z") 'undo-tree-undo)    ; Undo
(global-set-key (kbd "C-S-z") 'undo-tree-redo)  ; Redo
;; Remember: C-x u to visualize tree (undo-tree-visualize)




;; ------------------------
;; Web-mode
;; ------------------------
(require 'web-mode)

(setq web-mode-enable-current-element-highlight t)
(set-face-attribute 'web-mode-html-tag-face nil :foreground (face-attribute 'font-lock-function-name-face :foreground))
(set-face-attribute 'web-mode-html-attr-name-face nil :foreground (face-attribute 'font-lock-keyword-face :foreground))
(set-face-attribute 'web-mode-current-element-highlight-face nil :foreground "Pink")
;;(set-face-attribute 'web-mode-current-element-highlight-face nil :background (face-attribute 'show-paren-match :background))
(set-face-attribute 'web-mode-current-element-highlight-face nil :background (face-attribute 'match :background))

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))




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
  (let ((break-text (cond ((string= major-mode "js-mode")
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
	(break-text (cond ((string= major-mode "js-mode")
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




;; ------------------------------------------------
;; Shortcuts
;; ------------------------------------------------

(require 'normal-shortcuts)
(normal-shortcuts-mode)


;; ------------------------
;; Shortcut Functions
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


(defun interactive-ediff-file-with-original ()
  (interactive)
  (ediff-file-with-original))


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







;; ----------------------------------------------
;; Additional - Leftovers from Rodrigo's Config
;; ----------------------------------------------

;; (global-set-key (kbd "C-c c") 'comment-or-uncomment-region-or-line)
;; (global-set-key (kbd "C-z") 'browse-url-at-point)
;; (global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)
;; (global-set-key (kbd "C-a") 'beginning-of-line-no-blanks)

;; (global-set-key [f5] 'revert-buffer-no-confirm)
;; (global-set-key [f10] 'toggle-truncate-lines)
;; (global-set-key [f11] 'prelude-copy-file-name-to-clipboard)
;; (global-set-key [f12] 'open-buffer-path)

;; ;; -- code ---------------------------------------------------------------------

;; ;; from: https://gitorious.org/gnu-emacs-config/mainline/blobs/a3fe6e69d9a752ef094448bfdf1794ce39916f4d/dotemacs.el
;; (defun comment-or-uncomment-region-or-line ()
;;   "Like comment-or-uncomment-region, but if there's no mark \(that means no 194 region\) apply comment-or-uncomment to the current line"
;;   (interactive)
;;   (if (not mark-active)
;;       (comment-or-uncomment-region
;;        (line-beginning-position) (line-end-position))
;;       (if (< (point) (mark))
;;           (comment-or-uncomment-region (point) (mark))
;;           (comment-or-uncomment-region (mark) (point)))))

;; ;; from: http://tuxicity.se/emacs/elisp/2010/03/11/duplicate-current-line-or-region-in-emacs.html
;; (defun duplicate-current-line-or-region (arg)
;;   "Duplicates the current line or region ARG times.
;; If there's no region, the current line will be duplicated. However, if
;; there's a region, all lines that region covers will be duplicated."
;;   (interactive "p")
;;   (let (beg end (origin (point)))
;;     (if (and mark-active (> (point) (mark)))
;;         (exchange-point-and-mark))
;;     (setq beg (line-beginning-position))
;;     (if mark-active
;;         (exchange-point-and-mark))
;;     (setq end (line-end-position))
;;     (let ((region (buffer-substring-no-properties beg end)))
;;       (dotimes (i arg)
;;         (goto-char end)
;;         (newline)
;;         (insert region)
;;         (setq end (point)))
;;       (goto-char (+ origin (* (length region) arg) arg)))))

;; ;; from: http://www.emacswiki.org/emacs/RevertBuffer
;; (defun revert-buffer-no-confirm (&optional force-reverting)
;;   "Interactive call to revert-buffer. Ignoring the auto-save
;;  file and not requesting for confirmation. When the current buffer
;;  is modified, the command refuses to revert it, unless you specify
;;  the optional argument: force-reverting to true."
;;   (interactive "P")
;;   ;;(message "force-reverting value is %s" force-reverting)
;;   (if (or force-reverting (not (buffer-modified-p)))
;;       (revert-buffer :ignore-auto :noconfirm)
;;       (error "The buffer has been modified")))

;; ;; from: http://stackoverflow.com/questions/2416655/file-path-to-clipboard-in-emacs
;; ;; original: https://github.com/bbatsov/prelude
;; (defun prelude-copy-file-name-to-clipboard ()
;;   "Copy the current buffer file name to the clipboard."
;;   (interactive)
;;   (let ((filename (if (equal major-mode 'dired-mode)
;;                       default-directory
;;                       (buffer-file-name))))
;;     (when filename
;;       (kill-new filename)
;;       (message "%s" filename)))) ; "Copied buffer file name '%s' to the clipboard."

;; ;; from: http://zhangda.wordpress.com/2010/02/03/open-the-path-of-the-current-buffer-within-emacs/
;; ;; from: http://wiki/siscog/MetodosDeTrabalhoNaoOficiais#A.5BLO.4020121212.5D_Gitorious
;; (defun open-buffer-path ()
;;   (interactive)
;;   (shell-command (concat "explorer " (replace-regexp-in-string "/" "\\" (file-name-directory (buffer-file-name)) t t)))
;;   t)

;; ;; from: https://wiki/siscog/MetodosDeTrabalhoNaoOficiais#A.5BRAR.4020090818.5D_Posicionamento_em_in.2BAO0-cio_de_linha
;; (defun beginning-of-line-no-blanks (&optional provided-blanks)
;;   (interactive)
;;   (beginning-of-line)
;;   (let ((blanks (or (ensure-list provided-blanks)
;;                     (list ?\ ?\t)))
;;         (blanks.string ""))
;;     (dolist (blank blanks)
;;       (setf blanks.string (concatenate 'string blanks.string (format "%c" blank))))
;;     (let ((search.string (format "[^%s]" blanks.string)))
;;       (search-forward-regexp search.string)
;;       (backward-char))))

;; ;; based on: files.el
;; ;; based on: http://keypod.net/wordpress/2008/08/30/emacs-kill-matching-buffers-by/
;; (defun kill-lisp-buffers ()
;;   (interactive)
;;   (let ((count-killed 0)
;;         (count-modified 0)
;; 	(current-name (buffer-name (current-buffer))))
;;     (dolist (buffer (buffer-list))
;;       (let ((name (buffer-name buffer)))
;;         (when (and name
;; 		   (not (string-equal name ""))
;; 		   (not (string-equal name current-name))
;;                    (string-match "[.]lisp\\|[.]cl\\|[.]dic\\|[.]bil" name))
;;           (cond ((buffer-modified-p buffer)
;;                  (incf count-modified))
;;                 (t
;;                  (kill-buffer buffer)
;;                  (incf count-killed))))))
;;     (message "%d killed, %d modified" count-killed count-modified)))

;; (defun kill-slime-buffers ()
;;   (interactive)
;;   (let ((count-killed 0))
;;     (dolist (buffer (buffer-list))
;;       (let ((name (buffer-name buffer)))
;; 	(when (and name (not (string-equal name ""))
;; 		   (or (string-match "slime" name)
;; 		       (string-match "xref" name)))
;; 	  (kill-buffer buffer)
;; 	  (incf count-killed))))
;;     (message "%d killed" count-killed)))

;; (defun kill-garbage-buffers ()
;;   (interactive)
;;   (let ((count-killed 0))
;;     (dolist (buffer (buffer-list))
;;       (let ((name (buffer-name buffer)))
;; 	(when (and name (not (string-equal name ""))
;; 		   (or (string-match "REMOTE" name)
;; 		       (string-match "LOCAL" name)
;; 		       (string-match "BASE" name)
;; 		       (string-match "Ediff Registry" name)
;; 		       (string-match "Shell Command Output" name)
;; 		       (string-match "*Completions*" name)
;; 		       (string-match "magit" name)))
;; 	  (kill-buffer buffer)
;; 	  (incf count-killed))))
;;     (message "%d killed" count-killed)))

;; ;; -- code navigation ----------------------------------------------------------

;; (defvar *comment-signature* (format "%s" (upcase (user-login-name))))

;; (defun insert-comment-signature () ; based on: https://wiki/siscog/MetodosDeTrabalhoNaoOficiais#A.5BRAR.4020090818.5D_Coment.2BAOE-rio_com_assinatura
;;   (interactive)
;;   (flet ((insert-signature (x)
;;            (insert (format "%s; %s @ %s -- "
;;                            x
;;                            *comment-signature*
;;                            (format-time-string "%Y-%m-%d %a")))))
;;     (cond ((= (current-column) 0)
;;            (insert-signature ";")
;;            (newline-and-indent))
;;           (t
;;            (let ((start (point)))
;;              (end-of-line)
;;              (let ((point (point)))
;;                (beginning-of-line)
;;                (search-forward-regexp "[^ \t]")
;;                (let ((empty? (> (point) point)))
;;                  (goto-char point)
;;                  (insert-signature (if empty? ";" " "))
;;                  (if empty?
;;                      (newline-and-indent)
;;                      (goto-char start)))))))))

;; (defun search-mod-date-forward ()
;;   (interactive)
;;   (let ((text (concat *default-mod-date* "	" *default-author*)))
;;     (if (search-forward text nil t)
;;       (forward-line 2)
;;       (message "Modification date '%s' not found forward in buffer."
;;         text))))

;; (defun search-mod-date-backward ()
;;   (interactive)
;;   (let ((text (concat *default-mod-date* "	" *default-author*)))
;;     (unless (search-backward text nil t)
;;     (message "Modification date '%s' not found backward in buffer."
;;       text))))

;; (defun search-comment-signature-forward ()
;;   (interactive)
;;   (if (search-forward *comment-signature* nil t)
;;       (forward-line 1)
;;       (message "Comment signature %s not found forward in buffer."
;;                *comment-signature*)))

;; (defun search-comment-signature-backward ()
;;   (interactive)
;;   (unless (search-backward *comment-signature* nil t)
;;     (message "Comment signature %s not found backward in buffer."
;;              *comment-signature*)))


;; ;; -- code navigation ------------------

;; (global-set-key [M-down] 'search-mod-date-forward)
;; (global-set-key [M-up] 'search-mod-date-backward)

;; (global-set-key '[M-S-up] 'sacha/search-word-backward)
;; (global-set-key '[M-S-down] 'sacha/search-word-forward)

;; (global-set-key (kbd "<C-return>") 'insert-comment-signature)
;; (global-set-key [C-M-down] 'search-comment-signature-forward)
;; (global-set-key [C-M-up] 'search-comment-signature-backward)


