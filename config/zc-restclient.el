(eval-when-compile
  (require 'use-package))

(require 'zc-hydra-funcs)

(use-package restclient
  :straight t
  :defer t
  :commands (restclient-mode)
  :config
  (setq electric-indent-local-mode nil))



(zc-hydra/major-mode-define restclient-mode
  ("Navigation"
   (("n" restclient-jump-next "next" :exit nil)
    ("p" restclient-jump-prev "previous" :exit nil)
    ("N" restclient-narrow-to-current "narrow")
    ("W" widen "widen"))
   "Send"
   (("s" restclient-http-send-current-stay-in-window "send" :exit nil)
    ("S" restclient-http-send-current "send and jump")
    ("r" restclient-http-send-current-raw "send raw"))
   "Misc."
   (("w" restclient-copy-curl-command "copy curl")
    ("m" restclient-mark-current "mark"))))



(provide 'zc-restclient)
