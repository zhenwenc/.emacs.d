;;; completion/zc-company/config.el -*- lexical-binding: t; -*-

(after! company
  (setq company-tooltip-limit 12
        company-tooltip-align-annotations t
        company-idle-delay 0.1
        company-require-match nil
        company-minimum-prefix-length 2)

  ;; Prefer clear annotation rather than fancy icons :)
  (setq company-format-margin-function 'company-text-icons-margin))

;; To avoid company popup overlay rendering issue
;; https://github.com/tumashu/company-posframe
(use-package! company-posframe
  :after company
  :config
  (company-posframe-mode 1))
