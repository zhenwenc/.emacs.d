(require 'subr-x)
(require 'f)



;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function.

(add-to-list 'custom-theme-load-path paths-themes-dir)

;; Global settings (defaults)
(setq doom-themes-enable-bold t
      doom-themes-enable-italic t)

;; Load custom theme
(load-theme 'zc-dracula t)



;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

(when (display-graphic-p)
  (setq initial-frame-alist nil)
  (setq frame-resize-pixelwise t))



;; Flash with softer color, my eye hurts!
(after! nav-flash
  (set-face-attribute 'nav-flash-face nil :background (doom-darken 'cyan 0.5)))



;; FIXME: This mode causes moving line up/down extremely slow
;; https://github.com/purcell/page-break-lines/issues/19
(use-package! page-break-lines
  :hook (lisp-mode       . page-break-lines-mode)
  :hook (emacs-lisp-mode . page-break-lines-mode))

(use-package! highlight-thing
  :config
  (setq highlight-thing-delay-seconds 0.5
        highlight-thing-case-sensitive-p t
        highlight-thing-exclude-thing-under-point t
        highlight-thing-excluded-major-modes '(compilation-mode
                                               magit-status-mode
                                               org-mode))
  (global-highlight-thing-mode)

  ;; Disable `highlight-thing' for various cases
  (defun zc-theme/highlight-thing-should-highlight-p (fn &rest args)
    (and
     ;; If symbol is highlighted by `ahs-highlight-now',
     ;; the flicker effect occurs on other candidates.
     (not (bound-and-true-p ahs-highlighted))
     ;; Ensure the original condition satisfies.
     (apply fn args)
     ;; Check custom conditions.
     (let ((thing (highlight-thing-get-thing-at-point)))
       (or (not (stringp thing))
           (and
            ;; Highlight occurrences of a single character is nonsense
            (> (length thing) 1)
            ;; Ignore Org mode specific keywords
            (or (not (eq major-mode 'org-mode))
                (not (or (s-matches? (rx bos (+ "*") eos) thing)
                         (s-matches? (rx bos "+BEGIN_" (+ upper) eos) thing))))
            ;; Ignore repeated characters
            (not (s-matches? (rx bos (+ (or "-" "=")) eos) thing)))))))
  (advice-add 'highlight-thing-should-highlight-p :around
              'zc-theme/highlight-thing-should-highlight-p))
