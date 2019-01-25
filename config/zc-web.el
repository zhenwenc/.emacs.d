(eval-when-compile
  (require 'use-package))

(require 'rx)
(require 'general)



(use-package web-mode
  :straight t
  :defer t
  :defines (web-mode-css-indent-offset
            web-mode-markup-indent-offset)

  :general
  (:keymaps 'web-mode "C-c C-r" nil) ; Disable web-mode-reload binding
  (:keymaps 'web-mode "TAB" 'yas-expand "<tab>" 'yas-expand)

  :config
  (progn
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-enable-auto-quoting nil)

    (add-hook 'json-mode-hook
              (lambda ()
                (make-local-variable 'js-indent-level)
                (setq js-indent-level 2)))

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

    (flycheck-add-mode 'css-csslint 'zc-web-css-mode)
    (flycheck-add-mode 'json-jsonlint 'zc-web-json-mode)
    (flycheck-add-mode 'html-tidy 'zc-web-html-mode)

    (add-to-list 'flycheck-disabled-checkers 'json-jsonlint)
    (add-to-list 'flycheck-disabled-checkers 'css-csslint)

    (add-hook 'zc-web-css-mode-hook #'zc-web/add-node-modules-bin-to-path)))



(use-package zc-web-modes
  :defer t
  :mode (("\\.json\\'"     . zc-web-json-mode)
         ("\\.eslintrc\\'" . zc-web-json-mode)
         ("\\.babelrc\\'"  . zc-web-json-mode)
         ("\\.css\\'"      . zc-web-css-mode)
         ("\\.scss\\'"     . zc-web-css-mode)
         ("\\.html\\'"     . zc-web-html-mode)))



(provide 'zc-web)
