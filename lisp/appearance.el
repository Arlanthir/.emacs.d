;; ------------------------
;; Appearance
;; ------------------------

;; Emacs colors: http://web.ics.purdue.edu/~cs240/misc/emacs_colors.html
;;               http://raebear.net/comp/emacscolors.html

(require 'linum)                       ; Show line numbers (package)
(global-linum-mode t)                  ; Show line numbers

;; Alternative: supposedly faster but breaks our own breakpoints.el
;; (require 'nlinum)                       ; Show line numbers (package)
;; (global-nlinum-mode t)                  ; Show line numbers

(setq frame-title-format "%b - %f")    ; Show filename in frame title
(setq split-height-threshold 100)       ; Minimum window height when splitting
(setq split-width-threshold 180)       ; Minimum window width when splitting
(setq-default cursor-type 'bar)        ; Change cursor to a line instead of full character

(require 'whitespace)                                 ; Show whitespace
;; Default options: '(face tabs spaces trailing lines space-before-tab newline
;;                    indentation empty space-after-tab space-mark tab-mark newline-mark))
(setq whitespace-style '(face trailing))  ; Show only some whitespace
;; (setq whitespace-style '(face trailing space-mark tab-mark))  ; Show more whitespace
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
      '(js-mode emacs-lisp-mode lisp-mode web-mode typescript-mode))


;; Hide non-text UI
;; (menu-bar-mode -1)
(tool-bar-mode -1)
;; (scroll-bar-mode -1)
;; (tooltip-mode -1)
;; (blink-cursor-mode 0)

(global-hl-line-mode 1)                            ; Highlight current line


;; ------------------------
;; Scroll bar
;; ------------------------

;; (require 'yascroll)
;; (scroll-bar-mode -1)
;; (setq yascroll:delay-to-hide nil) ;; Never hide scrollbar
;; (set-face-background 'yascroll:thumb-fringe (face-attribute 'font-lock-function-name-face :foreground))
;; (set-face-foreground 'yascroll:thumb-fringe (face-attribute 'font-lock-function-name-face :foreground))
;; (global-yascroll-bar-mode 1)


;; ------------------------
;; Mode Line
;; ------------------------

(line-number-mode 1)               ; Show line number
(column-number-mode 1)             ; Show column number
(display-time)                     ; Show time
(setq display-time-24hr-format t)  ; Show time in 24h format



;; ------------------------
;; Theme
;; ------------------------

;; (load-theme 'atom-dark t)
(load-theme 'atom-one-dark t)

;; Override some colors
;; (set-face-background 'hl-line "grey10") ; Current line bg color
;; (set-face-background 'region "SteelBlue4")  ; Selected region bg color
;; (set-face-attribute 'font-lock-warning-face nil :foreground "#E5C07B")
(set-face-attribute 'font-lock-warning-face nil :foreground "#D19A66")


;; Fix ediff colors
(set-face-background ediff-fine-diff-face-A "#331111")
(set-face-background ediff-fine-diff-face-B "#113311")
(set-face-background ediff-current-diff-face-C "#665022")
(set-face-foreground ediff-odd-diff-face-A "black")
(set-face-foreground ediff-even-diff-face-A "black")
(set-face-foreground ediff-odd-diff-face-B "black")
(set-face-foreground ediff-even-diff-face-B "black")
(set-face-foreground ediff-odd-diff-face-C "black")
(set-face-foreground ediff-even-diff-face-C "black")

;; Set font
;;(set-face-attribute 'default nil :family "Courier New" :height 100) ;; Default
(set-face-attribute 'default nil :family "Consolas" :height 100)
;; (set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 100)

;(set-face-foreground 'show-paren-mismatch-face "darkseagreen2")
;(set-face-background 'show-paren-mismatch-face "red")
;(set-face-background 'show-paren-match-face "wheat1")

;; Whitespace colors
(set-face-attribute whitespace-tab nil :foreground "#444444" :background (face-attribute 'default :background))
(set-face-attribute 'whitespace-space nil :foreground "#444444" :background (face-attribute 'default :background))
(set-face-attribute 'whitespace-hspace nil :foreground "#444444" :background (face-attribute 'default :background))
(set-face-attribute 'whitespace-trailing nil :foreground "#ff0000" :background "red4")


(provide 'appearance)
