;; ------------------------
;; SISCOG
;; ------------------------

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

(setq sc-legacy-mode nil)

(load (expand-file-name "init.el" (getenv "SISCOG_EMACS_DIR")))
(load (expand-file-name "~/sc-user-param.el"))
;; (load (format "z:/home/sc-user-param-example.el"))
;;(load (format "%s/init.el" (getenv "SISCOG_EMACS_DIR")))

;; Use some legacy mode shortcuts
(define-key global-map "\M-;" 'sc-insert-commas)
(define-key global-map "\M-:" 'sc-delete-commas)
(global-set-key [f2] 'move-to-next-tab)
(global-set-key [f3] 'f3-insert-tab)
(global-set-key [f4] 'f4-insert-tabs)
(global-set-key [C-S-down-mouse-3] 'buffer-menu)
(global-set-key [C-S-down-mouse-1] 'select-file-menu)

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
	  :append)

(global-set-key "\C-cs" 'slime-selector)

(provide 'siscog-configuration)
