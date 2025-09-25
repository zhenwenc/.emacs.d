;;; lang/zc-mermaid/config.el -*- lexical-binding: t; -*-

;; https://github.com/abrochard/mermaid-mode
(use-package! mermaid-mode
  :custom
  ;; https://github.com/mermaid-js/mermaid-cli
  (mermaid-mmdc-location "npx -p @mermaid-js/mermaid-cli mmdc"))

;; https://github.com/arnm/ob-mermaid
(use-package! ob-mermaid
  :config
  (setq ob-mermaid-cli-path (concat zc-org/directory "/node_modules/.bin/mmdc"))
  (setq zc-mermaid-preview-buffer "*Mermaid Preview*")

  ;; Default ignore babel execution results to preview
  (setq org-babel-default-header-args:mermaid
        (assoc-delete-all :results org-babel-default-header-args:mermaid))
  (add-to-list 'org-babel-default-header-args:mermaid '(:results . "none"))

  (advice-add #'org-babel-execute:mermaid :around #'zc-mermaid/org-babel-execute))
