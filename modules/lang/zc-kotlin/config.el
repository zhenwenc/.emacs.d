;;; lang/zc-kotlin/config.el -*- lexical-binding: t; -*-

(use-package! kotlin-mode
  :config
  (setq kotlin-tab-width 2)

  (with-eval-after-load 'smartparens
    ;; Enter > right before the slash in a self-closing tag automatically
    ;; inserts a closing tag and places point inside the element
    (map! :map typescript-mode-map :i ">" #'zc-typescript/sp-jsx-rewrap-tag)

    ;; Expand C-style comment blocks. Doom is missing the `kotlin-mode'.
    (sp-with-modes '(kotlin-mode)
      (sp-local-pair "/*" "*/"
                     :actions '(insert)
                     :post-handlers '(("| " "SPC")
                                      (" | " "*")
                                      ("|[i]\n[i]" "RET")))))
  )
