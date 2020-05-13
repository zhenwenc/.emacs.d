(eval-when-compile
  (require 'use-package))

(require 'general)



(use-package smartparens
  :straight t

  :hook ((prog-mode    . smartparens-mode)
         (text-mode    . smartparens-mode)
         (eshell-mode  . smartparens-mode)
         (post-command . zc-sp/post-command-hook-handler))

  :general
  (:keymaps 'smartparens-mode-map
   :predicate '(not (derived-mode-p 'org-mode))
   "M-<right>"   #'sp-forward-slurp-sexp
   "M-<left>"    #'sp-forward-barf-sexp
   "M-S-<right>" #'sp-backward-slurp-sexp
   "M-S-<left>"  #'sp-backward-slurp-sexp
   "M-<up>"      #'sp-raise-sexp)

  :functions (sp-pair
              sp-get-pair
              sp-local-pair)

  :commands (smartparens-mode
             smartparens-global-mode
             show-smartparens-global-mode)

  :init

  (defun zc-sp/post-command-hook-handler ()
    "Handler for `post-command-hook'."
    (with-demoted-errors "zc-sp/post-command-hook-handler: %S"
      (when (derived-mode-p 'typescript-mode 'scala-mode)
        (zc-sp/maybe-insert-asterisk))))

  (defun zc-sp/maybe-insert-asterisk ()
    "Insert asterisk when in Javadoc style multiline comment."
    (when (and (member this-command '(newline evil-open-below))
               (save-excursion
                 (forward-line -1)
                 (string-match-p (rx bol (* space) "*" (not (any "/")))
                                 (thing-at-point 'line t))))
      (insert " * ")
      (indent-according-to-mode)))

  :config
  (setq
   sp-show-pair-delay                      0.2
   sp-show-pair-from-inside                t
   sp-cancel-autoskip-on-backward-movement nil
   sp-highlight-pair-overlay               nil
   sp-highlight-wrap-overlay               nil
   sp-highlight-wrap-tag-overlay           nil
   sp-navigate-close-if-unbalanced         t

   ;; Navigation feature doesn't seem to be useful for evil users.
   sp-navigate-skip-match         nil
   sp-navigate-consider-sgml-tags nil

   ;; Improve performance but sacrifice accuracy, because
   ;; smartparen's scans are relatively expensive.
   sp-max-pair-length   4
   sp-max-prefix-length 25)

  ;; Load default rules
  (require 'smartparens-config)

  ;; Global pairs

  (sp-pair "{" "}" :bind "M-{"
           :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
  (sp-pair "[" "]" :bind "M-["
           :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
  (sp-pair "(" ")" :bind "M-("
           :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))

  ;; Language specific pairs

  (sp-with-modes '(markdown-mode gfm-mode)
    (sp-local-pair "*" "*" :unless '(sp-in-string-p) :actions '(insert wrap)))

  (sp-with-modes '(typescript-mode scala-mode)
    (sp-local-pair "/*" "*/" :post-handlers '(("| " "SPC")
                                              (zc-typescript/sp-javadoc-expand "RET"))))

  (sp-with-modes '(typescript-mode)
    ;; Enter < inserts </> to start a new JSX node
    ;; Also see `zc-typescript/sp-jsx-rewrap-tag'
    (sp-local-pair "<" ">" :post-handlers '(zc-typescript/sp-jsx-expand-tag)))

  (sp-with-modes 'org-mode
    (sp-local-pair "[" "]" :post-handlers '(("|" "SPC")))

    ;; Instruct `smartparens' not to impose itself in org-mode
    ;; make delimiter auto-closing a little more conservative
    (sp-local-pair "*" "*" :unless '(:add sp-point-before-word-p zc-org/sp-point-at-bol-p))
    (sp-local-pair "_" "_" :unless '(:add sp-point-before-word-p))
    (sp-local-pair "/" "/" :unless '(:add sp-point-before-word-p zc-org/sp-point-in-checkbox-p))
    (sp-local-pair "~" "~" :unless '(:add sp-point-before-word-p))
    (sp-local-pair "=" "=" :unless '(:add sp-point-before-word-p)))

  ;; Global settings

  ;; Silence some harmless but annoying echo-area spam
  (dolist (key '(:unmatched-expression :no-matching-tag))
    (setf (alist-get key sp-message-alist) nil))

  (set-face-attribute 'show-paren-match nil :background "#434956" :foreground nil)

  (smartparens-global-mode +1)
  (show-smartparens-global-mode +1))



(provide 'zc-smartparens)
