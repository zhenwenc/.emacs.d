(eval-when-compile
  (require 'use-package))

(use-package markdown-mode
  :straight t
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :hook (markdown-mode . turn-on-auto-fill)
  :init
  (setq markdown-command "marked"))

(provide 'zc-markdown)
