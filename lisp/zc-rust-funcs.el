(require 'dash)

(autoload 'yas-lookup-snippet "yasnippet")
(autoload 'yas-expand-snippet "yasnippet")
(autoload 'sp--looking-back-p "smartparens")


;; Smartparens

(defun zc-rust/sp-expand-closure (id action &rest _ignored)
  (--when-let (and (eq action 'skip-closing-pair)
                   (or (sp--looking-back-p "= ||") ; declare variable
                       (sp--looking-back-p ", ||") ; as argument
                       (sp--looking-back-p "(||")) ; as first argument
                   (yas-lookup-snippet "closure" 'rustic-mode))
    (backward-delete-char 2)
    (yas-expand-snippet it)))



(provide 'zc-rust-funcs)
