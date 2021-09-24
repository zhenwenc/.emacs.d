;;; zc-prisma-mode.el --- Major mode for prisma files.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Frederick Cai

;; Author: Frederick Cai <frederick.cai@gmail.com>

;; Inspired: https://github.com/pimeys/emacs-prisma-mode

(eval-and-compile
  (require 'cc-mode)
  (require 'font-lock)
  (require 'rx))

(require 'web-mode)

(setq zc-prisma-font-lock-keywords
      (let* (;; We match `model Album {', and highlight `model' as keyword and `Album' as type.
             ;; Same rules for `enum`, `datasource` and `type'.
             (x-keywords-regexp "^\s*\\(model\\|enum\\|datasource\\|generator\\|type\\)\s+\\([a-zA-Z0-9_-]+\\)\s*{")
             ;; Mathces the column name and type, hilighting the type.
             (x-scalar-types-regexp "^\s+[a-zA-Z0-9_-]+\s+\\(Int\\|String\\|Boolean\\|DateTime\\|Float\\|Decimal\\|Json\\|[a-zA-Z0-9_-]+\\)")
             ;; A field attribute, such as `@id' or `@map', comes after the column type.
             (x-field-attributes-regexp "\@\\(id\\|map\\|default\\|relation\\|unique\\|index\\|ignore\\)")
             ;; A block attribute, usually at the end of a block such as model definition.
             ;; Example: `@@id([user_name, email])'
             (x-block-attributes-regexp "\@@\\(id\\|map\\|default\\|relation\\|unique\\|index\\|ignore\\)")
             ;; A native type definition, such as `@db.VarChar(255)'
             (x-native-types-regexp "\@[a-zA-Z0-9_-]+\.[a-zA-Z]+")
             ;; Properties in an attribute, e.g. `fields: [MediaTypeId]'.
             (x-properties-regexp "[a-zA-Z_-]+:")
             ;; Builtin functions. E.g. `autoincrement()'
             (x-attribute-functions-regexp "\\(autoincrement\\|cuid\\|uuid\\|now\\|env\\|dbgenerated\\)\(\.*\)")
             ;; Constants
             (x-constants-regexp "\\(true\\|false\\|null\\)")
             )
        `(;; order matters
          (,x-block-attributes-regexp    . font-lock-preprocessor-face)
          (,x-field-attributes-regexp    . font-lock-preprocessor-face)
          (,x-attribute-functions-regexp . (1 font-lock-function-name-face))
          (,x-native-types-regexp        . font-lock-preprocessor-face)
          (,x-keywords-regexp              (1 font-lock-keyword-face) (2 font-lock-type-face))
          (,x-properties-regexp          . font-lock-variable-name-face)
          (,x-scalar-types-regexp        . (1 font-lock-type-face))
          (,x-constants-regexp           . font-lock-constant-face)
          )))

(defvar zc-prisma-mode-syntax-table
  (with-syntax-table (copy-syntax-table)
    ;; comment syntax: begins with "//" or "///", ends with "\n"
    (modify-syntax-entry ?/  ". 12")
    (modify-syntax-entry ?\n ">")

    ;; string syntax: bounded by "
    (modify-syntax-entry ?\" "\"")

    (syntax-table))
  "Syntax table for `zc-prisma-mode'.")

;;;###autoload
(define-derived-mode zc-prisma-mode prog-mode "Prisma"
  "Derived mode for editing Prisma schema files."
  :syntax-table zc-prisma-mode-syntax-table

  (setq-default indent-tabs-mode nil)
  (setq tab-width 2)
  (setq font-lock-defaults '((zc-prisma-font-lock-keywords)))

  (setq-local electric-indent-chars
              (append "{}():;," electric-indent-chars))
  (setq-local electric-layout-rules
              '((?\; . after) (?\{ . after) (?\} . before)))

  (setq-local comment-start "// ")
  (setq-local comment-end ""))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.prisma$" . zc-prisma-mode))

(provide 'zc-prisma)

;;; zc-prisma-mode.el ends here
