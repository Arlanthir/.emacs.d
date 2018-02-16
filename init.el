;; -----------------------------------------------------------------------------
;; .emacs configuration
;; Version 0.92
;; Miguel Branco
;; -----------------------------------------------------------------------------

(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'cl) ; Common Lisp functions
(load-library "ediff")


;; ------------------------
;; Dependencies
;; ------------------------

(prefer-coding-system 'utf-8) ; Defined early to support asian chars on melpa packages

(require 'package)
;; (add-to-list 'package-archives
;; 	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(defvar my-packages
  '(
    ac-slime
    all-the-icons
    atom-dark-theme
    atom-one-dark-theme
    auto-complete
    company
    ;; fill-column-indicator
    flycheck
    goto-last-change
    ;; handlebars-mode
    ;; hc-zenburn-theme
    ;; hideshowvis
    multiple-cursors
    neotree
    ;; nlinum
    powerline
    tabbar
    tide
    typescript-mode
    undo-tree
    web-mode
    ;; yascroll
    ;; tabbar-ruler
    ))

(if (>= emacs-major-version 25)
    (setq my-packages (append my-packages
			      '(magit markdown-mode))))

(if (<= emacs-major-version 24)
    (push 'scss-mode my-packages))


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
;; Startup
;; ------------------------

(setq inhibit-startup-message t)                       ; Hide startup message
;;(setq initial-scratch-message nil)                     ; Hide first scratch message
;;(setq initial-frame-alist '((fullscreen . maximized))) ; Maximize (Win 8+)
(defun maximize-frame ()                               ; Maximize (Win 7)
  (interactive)
  (w32-send-sys-command #xf030)) ; f030 = 61488
(add-hook 'window-setup-hook 'maximize-frame t)        ; /Maximize (Win 7)


;; ------------------------
;; SISCOG
;; ------------------------

(require 'siscog-configuration)


;; ------------------------
;; Appearance
;; ------------------------

(require 'appearance)


;; ------------------------
;; Behavior
;; ------------------------

(require 'behavior)


;; ------------------------
;; Packages
;; ------------------------


;; ------------------------
;; All the icons
;; ------------------------

(require 'all-the-icons)
(setq inhibit-compacting-font-caches t)

;; ------------------------
;; Auto-complete
;; ------------------------

(require 'auto-complete-config)
(ac-config-default)
(setq ac-modes (delete 'css-mode ac-modes))
(setq ac-modes (delete 'scss-mode ac-modes))
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
;; Company
;; ------------------------

(add-hook 'css-mode-hook #'(lambda () (company-mode 1)))
(add-hook 'scss-mode-hook #'(lambda () (company-mode 1)))
(setq company-minimum-prefix-length 1)

(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "TAB") 'company-complete-selection)
     (define-key company-active-map (kbd "<tab>") 'company-complete-selection)))

;; ------------------------
;; Linting
;; ------------------------

(require 'linting)


;; ------------------------
;; Fill (80) column indicator
;; ------------------------

;; (require 'fill-column-indicator)
;; (setq fci-always-use-textual-rule t)
;; (add-hook 'prog-mode-hook #'fci-mode)


;; ------------------------
;; Hideshow / Hideshowvis - Commented out due to performance issues
;; ------------------------

;; Shift + middle click in a line hides/shows the entire block

;; (require 'hideshowvis)
;; (hideshowvis-symbols)
;; ;; (setq hideshowvis-ignore-same-line nil) ;; Uncomment if slow
;; (set-face-attribute 'hs-face nil :background (face-attribute 'match :background))
;; (add-hook 'prog-mode-hook #'hs-minor-mode)
;; (add-hook 'prog-mode-hook #'hideshowvis-minor-mode)


;; DEBUG SLOWNESS IN FCI AND HS:
;; (setq gc-cons-threshold (* 511 1024 1024))
;; (setq gc-cons-percentage 0.5)
;; (run-with-idle-timer 5 t #'garbage-collect)
;; (setq garbage-collection-messages t)


;; ------------------------
;; Magit
;; ------------------------

;; Rebind C-w to close magit
(eval-after-load "magit"
  '(define-key magit-mode-map (kbd "C-w") #'(lambda () (interactive) (magit-mode-bury-buffer t))))


;; ------------------------
;; Multiple Cursors
;; ------------------------

(require 'multiple-cursors)

(global-set-key (kbd "<C-S-up>") 'mc/mmlte--up)                ; Add cursor above
(global-set-key (kbd "<C-S-down>") 'mc/mmlte--down)            ; Add cursor below
(if (>= emacs-major-version 25)
  (global-set-key (kbd "<mouse-1>") #'(lambda (e) (interactive "e") (mc/keyboard-quit) (mouse-set-point e t)))  ; Cancel cursors on mouse click - Emacs 25.1+
  (global-set-key (kbd "<mouse-1>") #'(lambda (e) (interactive "e") (mc/keyboard-quit) (mouse-set-point e))))  ; Cancel cursors on mouse click

(global-unset-key (kbd "C-<down-mouse-1>"))                    ; Disable buffer menu and instead use multiple cursors
(global-set-key (kbd "C-<mouse-1>") 'mc/add-cursor-on-click)   ; Add cursor at click
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-beginnings-of-lines)  ; Cursor for each line in marked region
;;(global-set-key (kbd "C-S-c C-S-c") 'mc/mark-all-in-region)  ; Search in region for something and place cursors in each
(set-face-attribute 'mc/cursor-bar-face nil :height 4)         ; Reduce cursor size


;; ------------------------
;; Neotree - File Browser
;; ------------------------

(require 'neotree)
(setq neo-theme 'icons)
(setq neo-autorefresh t)
(setq neo-confirm-change-root 'off-p)
(setq neo-smart-open t)
;;(setq neo-show-updir-line t)
(setq neo-banner-message nil)
(setq neo-mode-line-type 'none)
(setq neo-window-width 35)
(setq neo-window-fixed-size nil)


(set-face-attribute 'neo-root-dir-face nil :foreground (face-attribute 'default :foreground) :weight 'bold)
(set-face-attribute 'neo-file-link-face nil :foreground (face-attribute 'default :foreground))


;; Change icon color
;; TODO change only the icon color (and not the entire line)
(defun neo-buffer--insert-fold-symbol (name &optional node-name)
  "Write icon by NAME, the icon style affected by neo-theme.
`open' write opened folder icon.
`close' write closed folder icon.
`leaf' write leaf icon.
Optional NODE-NAME is used for the `icons' theme"
  (let ((n-insert-image (lambda (n)
                          (insert-image (neo-buffer--get-icon n))))
        (n-insert-symbol (lambda (n)
                           (neo-buffer--insert-with-face
                            n 'neo-expand-btn-face))))
    (cond
     ((and (display-graphic-p) (equal neo-theme 'classic))
      (or (and (equal name 'open)  (funcall n-insert-image "open"))
          (and (equal name 'close) (funcall n-insert-image "close"))
          (and (equal name 'leaf)  (funcall n-insert-image "leaf"))))
     ((equal neo-theme 'arrow)
      (or (and (equal name 'open)  (funcall n-insert-symbol "▾"))
          (and (equal name 'close) (funcall n-insert-symbol "▸"))))
     ((equal neo-theme 'nerd)
      (or (and (equal name 'open)  (funcall n-insert-symbol "▾ "))
          (and (equal name 'close) (funcall n-insert-symbol "▸ "))
          (and (equal name 'leaf)  (funcall n-insert-symbol "  "))))
     ((and (display-graphic-p) (equal neo-theme 'icons))
      (unless (require 'all-the-icons nil 'noerror)
        (error "Package `all-the-icons' isn't installed"))
      (setq-local tab-width 1)
      (or (and (equal name 'open)  (insert (propertize (all-the-icons-icon-for-dir node-name "down" "  ") 'face `(:family ,(all-the-icons-octicon-family) :foreground ,(face-attribute 'link :foreground) :height 120))))
          (and (equal name 'close) (insert (propertize (all-the-icons-icon-for-dir node-name "right" "  ") 'face `(:family ,(all-the-icons-octicon-family) :foreground ,(face-attribute 'link :foreground) :height 120))))
          (and (equal name 'leaf)  (insert (format "\t\t\t%s\t" (all-the-icons-icon-for-file node-name))))))
     (t
      (or (and (equal name 'open)  (funcall n-insert-symbol "- "))
          (and (equal name 'close) (funcall n-insert-symbol "+ ")))))))


;; Remove '/' from directory names
(defun neo-buffer--insert-dir-entry (node depth expanded)
  (let ((node-short-name (neo-path--file-short-name node)))
    (insert-char ?\s (* (- depth 1) 2)) ; indent
    (when (memq 'char neo-vc-integration)
      (insert-char ?\s 2))
    (neo-buffer--insert-fold-symbol
     (if expanded 'open 'close) node)
    (insert-button node-short-name
                   'follow-link t
                   'face neo-dir-link-face
                   'neo-full-path node
                   'keymap neotree-dir-button-keymap
                   'help-echo (neo-buffer--help-echo-message node-short-name))
    (neo-buffer--node-list-set nil node)
    (neo-buffer--newline-and-begin)))


(global-set-key (kbd "C-\\") 'neotree-toggle)

;;(add-hook 'buffer-list-update-hook #'(lambda () (neotree-refresh))) ;; INFINITE LOOP

;; Show window divider
(when (facep 'window-divider)
    (set-face-attribute 'window-divider nil :foreground "#181a1f" :background "#181a1f"))
(when (facep 'vertical-border)
    (set-face-attribute 'vertical-border nil :foreground "#181a1f" :background "#181a1f"))
(add-to-list 'default-frame-alist '(right-divider-width . 1))

;; Hide scroll bars in neotree
(add-hook 'neo-after-create-hook #'(lambda (window) (set-window-scroll-bars neo-global--window 0 nil)))

;; Open by default
;; (neotree-show)



;; ------------------------
;; Powerline
;; ------------------------

(require 'powerline)
(setq powerline-height 22)
(powerline-default-theme)

;; Powerline overrides
(set-face-attribute 'powerline-active0 nil :background (face-attribute 'link :foreground) :foreground (face-attribute 'mode-line :background))
(set-face-attribute 'powerline-active1 nil :foreground (face-attribute 'default :foreground) :background (face-attribute 'region :background))
(set-face-attribute 'powerline-active2 nil :foreground (face-attribute 'mode-line :foreground) :background (face-attribute 'mode-line :background))


;; ------------------------
;; Tabbed Windows
;; ------------------------

(require 'tabs)
(tabbar-mode t)


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

(setq web-mode-enable-auto-indentation nil)
(setq web-mode-enable-current-element-highlight t)

(set-face-attribute 'web-mode-html-tag-face nil :foreground (face-attribute 'font-lock-function-name-face :foreground))
(set-face-attribute 'web-mode-html-attr-name-face nil :foreground (face-attribute 'font-lock-keyword-face :foreground))
(set-face-attribute 'web-mode-current-element-highlight-face nil :foreground "Pink")
;;(set-face-attribute 'web-mode-current-element-highlight-face nil :background (face-attribute 'show-paren-match :background))
(set-face-attribute 'web-mode-current-element-highlight-face nil :background (face-attribute 'match :background))

;; (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . web-mode))

(when (>= emacs-major-version 25)
  (eval-after-load 'flycheck
    '(progn
       (flycheck-add-mode 'javascript-eslint 'web-mode)
       (flycheck-add-mode 'sass-lint 'web-mode)))

  (defun setup-web-mode-linting ()
    (let ((checker (cond ((string= web-mode-content-type "javascript")
			  'javascript-eslint)
			 ((string= web-mode-content-type "css")
			  'sass-lint))))
      (flycheck-mode (if checker 1 -1))
      (when checker
	(flycheck-select-checker checker))))

  (add-hook 'web-mode-hook #'setup-web-mode-linting))


;; ------------------------
;; Toggle breakpoint on line number click
;; ------------------------

;; Commented out because it's slow
;; (require 'breakpoints)


;; ------------------------
;; Normal Shortcuts Mode
;; ------------------------

(require 'normal-shortcuts)
(normal-shortcuts-mode)


;; ------------------------
;; Shortcuts
;; ------------------------

(require 'shortcuts)


;; ------------------------
;; TypeScript
;; ------------------------

;; Note: A SISCOG-specific fallback tsconfig.json is created in siscog-configuration.el

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  ;; (flycheck-mode +1)
  ;; (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(add-hook 'typescript-mode-hook #'setup-tide-mode)


;; ------------------------
;; Add menu entry to edit this file
;; ------------------------

(define-key-after global-map [menu-bar options customize initel]
  (cons "Edit init.el" #'(lambda () (interactive) (find-file "~/.emacs.d/init.el"))))


;; ------------------------
;; Re-open files
;; ------------------------

(require 'desktop)                              ; Save desktop between sessions (package)
(setq desktop-save t                            ; Save desktop between sessions
      desktop-load-locked-desktop t             ; Load locked desktop files
      desktop-path '("~/.emacs.d/")             ; Desktop sessions directory
      desktop-dirname "~/.emacs.d/"             ; Desktop sessions directory
      desktop-base-file-name ".desktop"         ; Desktop file
      desktop-base-lock-name ".desktop.lock")   ; Locked desktop file
(desktop-save-mode)



