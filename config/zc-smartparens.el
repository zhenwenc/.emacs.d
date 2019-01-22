(eval-when-compile
  (require 'use-package))

(require 'general)

(use-package smartparens
  :straight t

  :hook ((prog-mode . smartparens-mode)
         (text-mode . smartparens-mode)
         (eshell-mode . smartparens-mode))

  :general
  (:keymaps 'smartparens-mode-map
            "M-<right>"   #'sp-forward-slurp-sexp
            "M-<left>"    #'sp-forward-barf-sexp
            "M-S-<right>" #'sp-backward-slurp-sexp
            "M-S-<left>"  #'sp-backward-slurp-sexp
            "M-<up>"      #'sp-raise-sexp)

  :functions (sp-pair
              sp-get-pair
              sp-local-pair)

  :commands (smartparens-mode
             smartparens-global-mode
             show-smartparens-global-mode)

  :config
  (progn
    (setq sp-show-pair-delay 0.2
          sp-show-pair-from-inside t
          sp-cancel-autoskip-on-backward-movement nil
          sp-highlight-pair-overlay nil
          sp-highlight-wrap-overlay nil
          sp-highlight-wrap-tag-overlay nil
          sp-navigate-close-if-unbalanced t)

    (require 'smartparens-config)

    ;; Global pairs

    (sp-pair "{" "}"
             :bind "M-{"
             :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
    (sp-pair "[" "]"
             :bind "M-["
             :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
    (sp-pair "(" ")"
             :bind "M-("
             :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))

    (sp-with-modes '(markdown-mode gfm-mode)
      (sp-local-pair "*" "*"
                     :unless '(sp-in-string-p)
                     :actions '(insert wrap)))

    (sp-with-modes 'org-mode
      (sp-local-pair "[" "]"
                     :post-handlers '(("|" "SPC"))))

    (set-face-attribute 'show-paren-match nil
                        :background "#434956"
                        :foreground nil)

    (smartparens-global-mode +1)
    (show-smartparens-global-mode +1)))

(provide 'zc-smartparens)
