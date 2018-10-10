(eval-when-compile
  (require 'use-package))

(require 'general)

(use-package company
  :straight t

  :general
  ([remap complete-symbol] #'company-manual-begin
   [remap completion-at-point] #'company-manual-begin
   "S-<return>" #'company-complete)

  (:keymaps 'comint-mode-map
            [remap indent-for-tab-command] #'company-manual-begin)

  :preface
  (defun zc-company/setup ()
    "Correct settings messed up by `evil-collection-company'."
    (general-define-key
     :keymaps 'company-active-map
     [return] #'company-complete-selection
     "RET"    #'company-complete-selection)

    (general-define-key
     :states 'insert :keymaps 'company-active-map
     "C-e"    #'evil-end-of-line
     "C-d"    #'evil-delete-char))

  :hook (after-init . global-company-mode)

  :config
  (progn
    (setq company-idle-delay 0.2
          company-require-match nil
          company-minimum-prefix-length 2

          company-dabbrev-downcase nil
          company-dabbrev-ignore-case nil
          company-dabbrev-code-ignore-case nil
          company-tooltip-limit 10
          company-tooltip-align-annotations t

          company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                              company-preview-frontend
                              company-echo-metadata-frontend)
          company-backends '(company-capf
                             company-dabbrev
                             company-dabbrev-code
                             company-keywords)
          company-transformers '(company-sort-by-occurrence))
    (add-hook 'company-mode-hook #'zc-company/setup)))

(use-package company-dabbrev
  :after company)

(use-package company-statistics
  :straight t
  :after company
  :config
  (setq company-statistics-file (concat paths-cache-directory "/company-statistics"))
  (company-statistics-mode +1))

(use-package company-yasnippet
  :after company
  :general
  ("C-M-y" #'company-yasnippet)
  :config
  (add-to-list company-backends 'company-yasnippet))

(provide 'zc-company)
