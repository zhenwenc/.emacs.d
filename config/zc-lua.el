(eval-when-compile
  (require 'use-package))

(require 'straight)
(require 'zc-hydra-funcs)



(use-package lua-mode
  :straight t
  :defer t
  :mode ("\\.lua\\'" . lua-mode)
  :interpreter ("lua" . lua-mode)
  :defines (lua-indent-level
            lua-indent-string-contents)
  :functions (lua-send-defun
              lua-send-buffer
              lua-send-region
              lua-send-current-line
              lua-search-documentation)
  :hook ((lua-mode . company-mode)
         (lua-mode . flycheck-mode-on-safe))
  :init
  (setq lua-indent-level 2
        lua-indent-string-contents t)
  :config
  (zc-hydra/major-mode-define lua-mode
    ("Send"
     (("sb" lua-send-buffer          "send buffer")
      ("sf" lua-send-defun           "send defun")
      ("sl" lua-send-current-line    "send line")
      ("sr" lua-send-region          "send region"))
     "Help"
     (("hh" lua-search-documentation "doc")))))



(provide 'zc-lua)
