(eval-when-compile
  (require 'use-package))

(require 'rx)
(require 'general)



(use-package zc-web-modes
  :defer t
  :mode (("\\.json\\'"     . zc-web-json-mode)
         ("\\.eslintrc\\'" . zc-web-json-mode)
         ("\\.babelrc\\'"  . zc-web-json-mode)
         ("\\.css\\'"      . zc-web-css-mode)
         ("\\.scss\\'"     . zc-web-css-mode)
         ("\\.html\\'"     . zc-web-html-mode)))



(use-package web-mode
  :straight t
  :defer t
  :defines (web-mode-css-indent-offset
            web-mode-markup-indent-offset)

  :general
  (:keymaps 'web-mode "C-c C-r" nil) ; Disable web-mode-reload binding
  (:keymaps 'web-mode "TAB" 'yas-expand "<tab>" 'yas-expand)

  :hook
  (js-mode         . #'zc-web/set-js-indent-level)
  (json-mode       . #'zc-web/set-js-indent-level)
  (zc-web-css-mode . #'zc-web/add-node-modules-bin-to-path)

  :preface
  (defun zc-web/set-js-indent-level ()
    (make-local-variable 'js-indent-level)
    (setq js-indent-level 2))

  :config
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-enable-auto-quoting nil)

  ;; Use line comments when commenting in JS
  (setf (cdr (assoc "javascript" web-mode-comment-formats)) "//")

  ;; Change default indentation behaviour
  (setf (cdr (assoc "lineup-args" web-mode-indentation-params)) nil)
  (setf (cdr (assoc "lineup-concats" web-mode-indentation-params)) nil)
  (setf (cdr (assoc "lineup-calls" web-mode-indentation-params)) nil)

  ;; Treat es6 files as JS files
  (add-to-list 'web-mode-content-types '("javascript" . "\\.es6\\'"))
  (add-to-list 'web-mode-content-types '("jsx" . "\\.jsx?\\'"))
  (add-to-list 'web-mode-content-types '("tsx" . "\\.tsx\\'"))

  (let ((tidy-bin "/usr/local/bin/tidy"))
    (when (file-exists-p tidy-bin)
      (setq flycheck-html-tidy-executable tidy-bin)))

  (flycheck-add-mode 'html-tidy     'zc-web-html-mode)
  (flycheck-add-mode 'css-csslint   'zc-web-css-mode)
  (flycheck-add-mode 'json-jsonlint 'zc-web-json-mode)

  (zc-flycheck/disable-checkers 'json-jsonlint 'css-csslint))



(use-package prettier-js
  :straight t
  :after (:any zc-web-modes typescript-mode graphql-mode)
  :commands (prettier-js prettier-js-mode)
  :hook ((graphql-mode    . zc-web/maybe-enable-prettier)
         (typescript-mode . zc-web/maybe-enable-prettier)
         (zc-web-css-mode . zc-web/maybe-enable-prettier))
  :preface
  (defun zc-web/maybe-enable-prettier ()
    (unless (or (not buffer-file-name) ; maybe scratch
                (s-contains-p "/github/" buffer-file-name)
                (s-contains-p "/node_modules/" buffer-file-name))
      (prettier-js-mode)))
  :config
  ;; NOTE: If the prettier version seems outdated, check .nvmrc
  (setq prettier-js-args '("--single-quote" "--trailing-comma" "es5")))



(use-package nvm
  :disabled t ; use nodenv
  :straight t
  :functions (nvm-use-for-buffer)
  :preface
  (defun zc-web/maybe-use-nvm ()
    ;; NOTE: `nvm use' command doesn't update .nvmrc
    (if (locate-dominating-file default-directory ".nvmrc")
        (progn (nvm-use-for-buffer) t)
      (message "Looks like [.nvmrc] is missing!"))))



(provide 'zc-web)
