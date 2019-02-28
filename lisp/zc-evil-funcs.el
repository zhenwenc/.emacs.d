(require 'dash)

(autoload 'evil-set-initial-state "evil")



(defun zc-evil/set-initial-state (modes state)
  "Set the initialize STATE of MODES using
`evil-set-initial-state'."
  (with-eval-after-load 'evil
    (dolist (mode (-list modes))
      (evil-set-initial-state mode state))))



(provide 'zc-evil-funcs)
