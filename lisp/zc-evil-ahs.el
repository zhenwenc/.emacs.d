(require 'evil)
(require 'iedit)

;; TODO: Only need one direction -> always forward

(autoload 'evil-iedit-state/iedit-mode "evil-iedit-state")

(defvar-local zc-evil-ahs/last-ahs-highlight-p nil
  "Info on the last searched highlighted symbol.")

(defvar ahs-default-range)
(defvar ahs-current-range)



(defun zc-evil-ahs/integrate-evil-search (forward)
  ;; isearch-string is last searched item. Next time
  ;; "n" is hit we will use this.
  (let* ((symbol (evil-find-thing forward 'symbol))
         (regexp (concat "\\<" symbol "\\>")))
    (setq isearch-string regexp
          isearch-regexp regexp
          evil-ex-search-pattern (evil-ex-make-search-pattern regexp)))
  ;; Next time "n" is hit, go the correct direction.
  (setq isearch-forward forward)
  ;; Place the search term into the search rings.
  (isearch-update-ring isearch-string t)
  (evil-push-search-history isearch-string forward)
  ;; Use this search term for empty pattern "%s//replacement/"
  ;; Append case sensitivity
  (setq evil-ex-last-was-search nil
        evil-ex-substitute-pattern `(,(concat isearch-string "\\C")
                                     nil (0 0))))

(defun zc-evil-ahs/goto-last-searched-symbol ()
  "Go to the last known occurrence of the last symbol searched
with `auto-highlight-symbol'."
  (interactive)
  (if zc-evil-ahs/last-ahs-highlight-p
      (progn (goto-char (nth 1 zc-evil-ahs/last-ahs-highlight-p))
             (zc-evil-ahs/ahs-highlight-now-wrapper))
    (message "No symbol has been searched for now.")))

(defun zc-evil-ahs/ensure-ahs-enabled-locally ()
  "Ensures ahs is enabled for the local buffer."
  (unless
      (bound-and-true-p ahs-mode-line)
    (auto-highlight-symbol-mode)))

(defun zc-evil-ahs/ahs-highlight-now-wrapper ()
  "Safe wrapper for ahs-highlight-now"
  (eval '(progn
           (zc-evil-ahs/ensure-ahs-enabled-locally)
           (ahs-highlight-now)) nil))

(defun zc-evil-ahs/goto-next-forward ()
  "Go to the next occurrence of symbol under point with
`auto-highlight-symbol'"
  (interactive)
  (zc-evil-ahs/goto-next t))

(defun zc-evil-ahs/goto-next-backward ()
  "Go to the previous occurrence of symbol under point with
`auto-highlight-symbol'"
  (interactive)
  (zc-evil-ahs/goto-next nil))

(defun zc-evil-ahs/goto-next (forward)
  "Go to the next occurrence of symbol under point with
`auto-highlight-symbol'"
  (zc-evil-ahs/integrate-evil-search forward)
  (zc-evil-ahs/ahs-highlight-now-wrapper)
  (evil-set-jump)
  (if forward (ahs-forward) (ahs-backward))
  ;; recenter window for better visibility
  (recenter))

(defun zc-evil-ahs/highlight-symbol ()
  "Highlight the symbol under point with `auto-highlight-symbol'."
  (interactive)
  (zc-evil-ahs/ahs-highlight-now-wrapper)
  (setq zc-evil-ahs/last-ahs-highlight-p (ahs-highlight-p))
  (zc-evil-ahs/integrate-evil-search t)
  (zc-evil-ahs-hydra/body))

(defun zc-evil-ahs/iedit ()
  "Turn on edit mode for selected symbols."
  (interactive)
  (evil-iedit-state/iedit-mode)
  (iedit-restrict-region (ahs-current-plugin-prop 'start)
                         (ahs-current-plugin-prop 'end))
  (ahs-edit-mode t))

(defun zc-evil-ahs/reset ()
  "Reset the range for `auto-highlight-symbol'."
  (interactive)
  (unless (eq (symbol-value ahs-default-range)
              ahs-current-range)
    (ahs-change-range ahs-default-range)))

(provide 'zc-evil-ahs)
