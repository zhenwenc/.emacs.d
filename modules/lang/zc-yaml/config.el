;;; lang/zc-yaml/config.el -*- lexical-binding: t; -*-

(after! yaml-mode
  (setq yaml-imenu-generic-expression
        '((nil "^\\(:?[[:space:]]\\{2\\}??[a-zA-Z_-]+\\):" 1))))

(add-hook! 'yaml-mode-hook
  (defun zc-yaml/override-font-lock-faces ()
    (face-remap-add-relative 'font-lock-variable-name-face
                             :foreground (doom-color 'violet))))

(add-hook! 'yaml-mode-hook
  (defun zc-yaml/disable-work-wrap-mode ()
    "See `+word-wrap--calc-extra-indent'"
    (+word-wrap-mode -1)))
