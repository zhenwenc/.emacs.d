(eval-when-compile
  (require 'use-package))

(require 'zc-funcs)
(require 'zc-hydra-funcs)


;; Prodigy provides a UI for managing external processes.

(use-package prodigy
  :straight t
  :defer t
  :commands (prodigy)

  :general
  (:states '(motion normal) :keymaps 'prodigy-mode-map
   "TAB" #'prodigy-display-process)

  :hydra
  ("Basic"
   (("n" prodigy-next "next")
    ("p" prodigy-prev "previous")
    ("<" prodigy-first "first")
    (">" prodigy-last "last"))

   "Marking"
   (("m" prodigy-mark "mark")
    ("t" prodigy-mark-tag "mark tag")
    ("M" prodigy-mark-all "mark all")
    ("u" prodigy-unmark "unmark")
    ("T" prodigy-unmark-tag "unmark tag")
    ("U" prodigy-unmark-all "unmark all"))

   "Filters"
   (("ft" prodigy-add-tag-filter "tag")
    ("fn" prodigy-add-name-filter "name")
    ("fc" prodigy-clear-filters "clear"))

   "Service"
   (("s" prodigy-start "start")
    ("S" prodigy-stop "stop")
    ("r" prodigy-restart "restart")
    ("g" prodigy-refresh "refresh")
    ("o" prodigy-browse "open in browser")
    ("TAB" prodigy-display-process "view output")))

  :config
  ;; Truncate buffers.
  (setq prodigy-view-truncate-by-default t)

  ;; Use standard completing-read.
  (setq prodigy-completion-system 'default)

  ;; Load service configs
  (zc/load-private-package 'private-prodigy "prodigy.el"))



(provide 'zc-prodigy)
