(eval-when-compile
  (require 'use-package))

(require 'dash)
(require 'general)

(defconst zc-company/backend-alist
  '((text-mode       :derived (company-capf company-dabbrev company-ispell))
    (prog-mode       :derived (company-capf))
    (conf-mode       :derived (company-dabbrev-code))
    (css-mode        :exact   (company-css))
    (tide-mode       :exact   (company-tide))
    (typescript-mode :exact   (company-lsp))
    (python-mode     :exact   (company-lsp))
    (scala-mode      :exact   (company-lsp))
    (ensime-mode     :exact   (ensime-company))
    (terraform-mode  :exact   (company-dabbrev)))
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

        company-tooltip-limit 12
        company-tooltip-align-annotations t

        ;; Only search the current buffer to get suggestions
        ;; for `company-dabbrev'. This prevents company from
        ;; causing lag when there are many buffers open.
        company-dabbrev-other-buffers nil
        company-dabbrev-code-ignore-case nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil

        ;; Always display suggestions in the tooltip, even if
        ;; there is only one. If displaying metadata in the echo
        ;; area, this may conflicts with ElDoc.
        ;; NOTE: disabled `company-echo-metadata-frontend'
        company-frontends '(company-pseudo-tooltip-frontend)
        company-backends '(company-capf)
        company-transformers '(company-sort-by-occurrence)))

(use-package company-dabbrev
  :after company)

;; Provides intelligent sorting and filtering for candidates.
(use-package company-prescient
  :straight t
  :after company
  :hook (company-mode . company-prescient-mode)
  :config
  (setq prescient-save-file (concat paths-cache-dir "prescient-save.el"))
  (prescient-persist-mode +1))

(use-package company-box
  :straight t
  :after company
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-show-single-candidate t
        company-box-backends-colors nil
        company-box-max-candidates 50
        company-box-icons-alist 'company-box-icons-all-the-icons)

  (setq company-box-icons-all-the-icons
        `((Unknown       . ,(all-the-icons-material "find_in_page" :height 0.7  :v-adjust -0.15))
          (Text          . ,(all-the-icons-faicon   "book" :height 0.68 :v-adjust -0.15))
          (Method        . ,(all-the-icons-faicon   "cube" :height 0.7  :v-adjust -0.05 :face 'font-lock-constant-face))
          (Function      . ,(all-the-icons-faicon   "cube" :height 0.7  :v-adjust -0.05 :face 'font-lock-constant-face))
          (Constructor   . ,(all-the-icons-faicon   "cube" :height 0.7  :v-adjust -0.05 :face 'font-lock-constant-face))
          (Field         . ,(all-the-icons-faicon   "tags" :height 0.65 :v-adjust -0.15 :face 'font-lock-warning-face))
          (Variable      . ,(all-the-icons-faicon   "tag" :height 0.7  :v-adjust -0.05 :face 'font-lock-warning-face))
          (Class         . ,(all-the-icons-faicon   "clone" :height 0.65 :v-adjust 0.01 :face 'font-lock-constant-face))
          (Interface     . ,(all-the-icons-faicon   "clone" :height 0.65 :v-adjust 0.01))
          (Module        . ,(all-the-icons-octicon  "package" :height 0.7  :v-adjust -0.15))
          (Property      . ,(all-the-icons-octicon  "package" :height 0.7  :v-adjust -0.05 :face 'font-lock-warning-face))
          (Unit          . ,(all-the-icons-material "settings_system_daydream" :height 0.7 :v-adjust -0.15))
          (Value         . ,(all-the-icons-material "format_align_right" :height 0.7 :v-adjust -0.15 :face 'font-lock-constant-face))
          (Enum          . ,(all-the-icons-material "storage" :height 0.7 :v-adjust -0.15 :face 'all-the-icons-orange))
          (Keyword       . ,(all-the-icons-material "filter_center_focus" :height 0.7 :v-adjust -0.15))
          (Snippet       . ,(all-the-icons-faicon   "code" :height 0.7 :v-adjust 0.02 :face 'font-lock-variable-name-face))
          (Color         . ,(all-the-icons-material "palette" :height 0.7 :v-adjust -0.15))
          (File          . ,(all-the-icons-faicon   "file-o" :height 0.7 :v-adjust -0.05))
          (Reference     . ,(all-the-icons-material "collections_bookmark" :height 0.7 :v-adjust -0.15))
          (Folder        . ,(all-the-icons-octicon  "file-directory" :height 0.7 :v-adjust -0.05))
          (EnumMember    . ,(all-the-icons-material "format_align_right" :height 0.7 :v-adjust -0.15 :face 'all-the-icons-blueb))
          (Constant      . ,(all-the-icons-faicon   "tag" :height 0.7 :v-adjust -0.05))
          (Struct        . ,(all-the-icons-faicon   "clone" :height 0.65 :v-adjust 0.01 :face 'font-lock-constant-face))
          (Event         . ,(all-the-icons-faicon   "bolt" :height 0.7 :v-adjust -0.05 :face 'all-the-icons-orange))
          (Operator      . ,(all-the-icons-fileicon "typedoc" :height 0.65 :v-adjust 0.05))
          (TypeParameter . ,(all-the-icons-faicon   "hashtag" :height 0.65 :v-adjust 0.07 :face 'font-lock-const-face))
          (Template      . ,(all-the-icons-faicon   "code" :height 0.7 :v-adjust 0.02 :face 'font-lock-variable-name-face)))))



(provide 'zc-company)
