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
	  :append)

(global-set-key "\C-cs" 'slime-selector)

(provide 'siscog-configuration)
