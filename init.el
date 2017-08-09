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
    typescript-mode
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
;; Startup
;; ------------------------

(setq inhibit-startup-message t)                       ; Hide startup message
;;(setq initial-scratch-message nil)                     ; Hide first scratch message
;;(setq initial-frame-alist '((fullscreen . maximized))) ; Maximize (Win 8+)
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
;; Linting
;; ------------------------

(require 'linting)


;; ------------------------
;; Hideshow / Hideshowvis - Commented out due to performance issues
;; ------------------------

(require 'hideshowvis)
(hideshowvis-symbols)
;;(setq hideshowvis-ignore-same-line nil)
(set-face-attribute 'hs-face nil :background (face-attribute 'match :background))
(add-hook 'prog-mode-hook #'hs-minor-mode)
(add-hook 'prog-mode-hook #'hideshowvis-minor-mode)


;; ------------------------
;; Multiple Cursors
;; ------------------------

(require 'multiple-cursors)

(global-set-key (kbd "<C-S-up>") 'mc/mmlte--up)                ; Add cursor above
(global-set-key (kbd "<C-S-down>") 'mc/mmlte--down)            ; Add cursor below
(global-set-key (kbd "<mouse-1>") #'(lambda (e) (interactive "e") (mc/keyboard-quit) (mouse-set-point e)))  ; Cancel cursors on mouse click
;;(global-set-key (kbd "<mouse-1>") #'(lambda (e) (interactive "e") (mc/keyboard-quit) (mouse-set-point e t)))  ; Cancel cursors on mouse click - Emacs 25.1+
(global-unset-key (kbd "C-<down-mouse-1>"))                    ; Disable buffer menu and instead use multiple cursors
(global-set-key (kbd "C-<mouse-1>") 'mc/add-cursor-on-click)   ; Add cursor at click
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-beginnings-of-lines)  ; Cursor for each line in marked region
;;(global-set-key (kbd "C-S-c C-S-c") 'mc/mark-all-in-region)  ; Search in region for something and place cursors in each
(set-face-attribute 'mc/cursor-bar-face nil :height 4)         ; Reduce cursor size


;; ------------------------
;; Neotree - File Browser
;; ------------------------

(require 'neotree)
(setq neo-smart-open t)
(global-set-key (kbd "C-\\") 'neotree-toggle)


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

(require 'breakpoints)


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
;; Add menu entry to edit this file
;; ------------------------

(define-key-after global-map [menu-bar options customize initel]
  (cons "Edit init.el" #'(lambda () (interactive) (find-file "~/.emacs.d/init.el"))))
