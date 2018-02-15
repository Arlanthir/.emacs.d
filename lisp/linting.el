;; ------------------------
;; Linting - Flycheck
;; ------------------------

(eval-after-load 'flycheck
  '(progn

     ;; SCSS
    (flycheck-def-config-file-var flycheck-sass-lintyml sass-lint ".sass-lint.yml"
     :safe #'stringp)

    (flycheck-define-checker sass-lint
     "A SASS (SCSS) checker using Sass Lint (on Node.js).
See URL `https://github.com/sasstools/sass-lint'."
     :command ("sass-lint"
	       "--verbose" "-q"
	       "--format" "checkstyle"
	       (config-file "--config" flycheck-sass-lintyml)
	       source)
     :error-parser flycheck-parse-checkstyle
     :modes (sass-mode scss-mode))

    (pushnew 'sass-lint flycheck-checkers)

    (setq flycheck-sass-lintyml "Z:/siscog/scs-vdev/task-runner/.sass-lint.yml")
    (setq flycheck-sass-lint-executable "Z:/siscog/scs-vdev/task-runner/node_modules/.bin/sass-lint.cmd")


    ;; Javascript
    (setq flycheck-eslintrc "Z:/siscog/scs-vdev/task-runner/.eslintrc")
    (setf (flycheck-checker-get 'javascript-eslint 'command) `("eslint" "--format=json"
							       "--config" ,flycheck-eslintrc
							       (option-list "--rulesdir" flycheck-eslint-rules-directories)
							       "--stdin" "--stdin-filename" source-original))
    ;; (defun flycheck-eslint-config-exists-p ()
    ;;   "Whether there is an eslint config for the current buffer."
    ;;   t)

    (defun flycheck-eslint-config-exists-p ()
      "Whether there is an eslint config for the current buffer."
      (let* ((executable (flycheck-find-checker-executable 'javascript-eslint))
	     (exitcode (and executable (call-process executable nil nil nil
						     "--config" flycheck-eslintrc "--print-config" "."))))
	(eq exitcode 0)))

    ;; (setf (flycheck-checker-get 'javascript-eslint 'enabled) (lambda () t))
    (setq flycheck-javascript-eslint-executable "Z:/siscog/scs-vdev/task-runner/node_modules/.bin/eslint.cmd")

    ;; Typescript
    (setq flycheck-typescript-tslint-config "Z:/siscog/scs-vdev/task-runner/tslint.json")
    (setq flycheck-typescript-tslint-executable "Z:/siscog/scs-vdev/task-runner/node_modules/.bin/tslint.cmd")


    ;; Other configuration
    (setq flycheck-xml-parser 'flycheck-parse-xml-region)
    (setq-default flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc))))


(add-hook 'after-init-hook #'global-flycheck-mode)

(provide 'linting)
