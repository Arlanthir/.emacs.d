;; ------------------------------------------------------------------------------
;; Tabbed Windows
;; ------------------------------------------------------------------------------

(require 'tabbar)

;; TODO Grab some colors from the current theme

;; (defconst my-conf-tab-fg-color "grey85")          ; Tabbed windows fg color, suggestion: black
;; (defconst my-conf-tab-bg-color "grey40")          ; Tabbed windows bg color, suggestion: grey80
;; (defconst my-conf-tab-fg-hover "white")           ; Tabbed windows hover fg color, suggestion: black
;; (defconst my-conf-tab-bg-hover "grey25")          ; Tabbed windows hover bg color, suggestion: grey75
;; (defconst my-conf-tab-current-fg-color "white")   ; Tabbed windows current fg color, suggestion: grey20
;; (defconst my-conf-tab-current-bg-color "#1d1f21") ; Tabbed windows current bg color, suggestion: grey95
;; (defconst my-conf-tab-separator-color "#1d1f21")  ; Tabbed windows separator color, suggestion: grey56
;; (defconst my-conf-tab-padding 5)                  ; Tabbed windows padding, suggestion: 3

(defconst my-conf-tab-fg-color (face-attribute 'mode-line :foreground))       ; Tabbed windows fg color, suggestion: black
(defconst my-conf-tab-bg-color (face-attribute 'mode-line :background))       ; Tabbed windows bg color, suggestion: grey80
(defconst my-conf-tab-fg-hover (face-attribute 'region :foreground))          ; Tabbed windows hover fg color, suggestion: black
(defconst my-conf-tab-bg-hover (face-attribute 'region :background))          ; Tabbed windows hover bg color, suggestion: grey75
(defconst my-conf-tab-current-fg-color "white")                               ; Tabbed windows current fg color, suggestion: grey20
(defconst my-conf-tab-current-bg-color (face-attribute 'default :background)) ; Tabbed windows current bg color, suggestion: grey95
(defconst my-conf-tab-separator-color "#181a1f")                              ; Tabbed windows separator color, suggestion: grey56
(defconst my-conf-tab-padding 7)                                              ; Tabbed windows padding, suggestion: 3


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

(set-face-attribute 'tabbar-selected-modified nil             ; Current tab when modified
		    :inherit 'tabbar-selected
		    :overline t
		    :weight 'ultra-bold
		    :foreground my-conf-tab-current-fg-color
		    :box `(:line-width ,my-conf-tab-padding :color ,my-conf-tab-current-bg-color :style nil)
		    )

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
		     (string= (buffer-name buffer) "*tide-documentation*")
		     (string-prefix-p "*tide-server*" (buffer-name buffer))
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
    (update-tab-bar)))

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
    (update-tab-bar)))

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

(provide 'tabs)
