(eval-when-compile
  (require 'use-package))

(require 'dash)
(require 'general)

(defvar zc-company/backend-alist
  '((text-mode   :derived (company-capf company-dabbrev company-ispell))
    (prog-mode   :derived (company-capf))
    (conf-mode   :derived (company-dabbrev-code))
    (css-mode    :exact   (company-css))
    (tide-mode   :exact   (company-tide))
    (ensime-mode :exact   (ensime-company)))
  "An alist matching modes to company backends.")



(use-package company
  :straight t

  :general
  ([remap complete-symbol]     #'company-manual-begin
   [remap completion-at-point] #'company-manual-begin
   "S-<return>"                #'company-complete)

  (:keymaps 'comint-mode-map
            [remap indent-for-tab-command] #'company-manual-begin)

  :preface
  (defun zc-company/setup ()
    ;; Set `company-backends' for the current buffer.
    (set (make-local-variable 'company-backends)
         (-mapcat (-lambda ((mode &plist :derived derived :exact exact))
                    (or (and (derived-mode-p mode) derived)
                        (and (or (eq major-mode mode)
                                 (and (boundp mode)
                                      (symbol-value mode)))
                             exact)))
                  zc-company/backend-alist))

    ;; Correct settings messed up by `evil-collection-company'.
    (general-define-key
     :keymaps 'company-active-map
     [return] #'company-complete-selection
     "RET"    #'company-complete-selection)

    (general-define-key
     :states 'insert :keymaps 'company-active-map
     "C-e"    #'evil-end-of-line
     "C-d"    #'evil-delete-char))

  :hook ((after-init . global-company-mode)
         (company-mode . zc-company/setup))

  :init
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
        company-backends '(company-capf)
        company-transformers '(company-sort-by-occurrence)))

(use-package company-dabbrev
  :after company)

(use-package company-statistics
  :straight t
  :after company
  :config
  (setq company-statistics-file (concat paths-cache-directory "/company-statistics"))
  (company-statistics-mode +1))

(provide 'zc-company)
