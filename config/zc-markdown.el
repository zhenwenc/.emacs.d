(eval-when-compile
  (require 'use-package))

(use-package markdown-mode
  :straight t
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  ;; NOTE: Disabled by default to accommodate coding styles at work
  ;; :hook (markdown-mode . turn-on-auto-fill)
  :init
  (setq markdown-command "marked"))

(provide 'zc-markdown)
