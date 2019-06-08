(eval-when-compile
  (require 'use-package))

(require 'f)
(require 'color)
(require 'general)

(defvar ahs-highlighted)



(defvar zc-default-font
  "Fira Code 13"
  "The universal default font.")

(defvar zc-variable-pitch-font
  "Fira Code 13"
  "The font to use in the variable-pitch face.")

(defvar zc-fixed-pitch-font
  "Fira Code 13"
  "The font to use in the fixed-pitch face.")


;; Themes

(use-package doom-themes
  :straight t
  :config
  (progn
    ;; Global settings (defaults)
    (setq doom-themes-enable-bold t
          doom-themes-enable-italic t)

    ;; Load the theme
    (load-theme 'zc-dracula t)

    ;; Enable custom treemacs theme
    (doom-themes-treemacs-config)

    ;; Corrects (and improves) org-mode's native fontification.
    (doom-themes-org-config)))



(use-package all-the-icons
  :straight t)

(use-package page-break-lines
  :straight t
  :demand t
  :commands (global-page-break-lines-mode)
  :preface
  (defun zc-theme/disable-all-the-icons-in-tty (orig-fn &rest args)
    (when (display-graphic-p)
      (apply orig-fn args)))
  :init
  (dolist (fn '(all-the-icons-octicon
                all-the-icons-material
                all-the-icons-faicon
                all-the-icons-fileicon
                all-the-icons-wicon
                all-the-icons-alltheicon))
    (advice-add fn :around #'zc-theme/disable-all-the-icons-in-tty))
  :config
  (progn
    (setq page-break-lines-modes
          '(prog-mode
            text-mode
            help-mode
            imenu-list-major-mode
            compilation-mode
            org-agenda-mode))

    ;; Use `all-the-icons-insert-*' to find available icons.
    (dolist (icon '((prodigy-mode   all-the-icons-faicon "bar-chart"
                                    :face all-the-icons-blue)
                    (terraform-mode all-the-icons-faicon "tree"
                                    :face all-the-icons-blue)))
      (add-to-list 'all-the-icons-mode-icon-alist icon))

    (global-page-break-lines-mode)))

(use-package rainbow-delimiters
  :straight t)

(use-package hl-line
  :straight t
  :preface
  (defun zc-theme/maybe-init-hl-line ()
    (unless (or (derived-mode-p 'lisp-mode)
                (derived-mode-p 'emacs-lisp-mode))
      (hl-line-mode)))
  :hook
  (text-mode . zc-theme/maybe-init-hl-line)
  (prog-mode . zc-theme/maybe-init-hl-line))

(use-package hl-todo
  :straight t
  :preface
  (defun zc-theme/maybe-init-hl-todo ()
    (let ((name (buffer-name)))
      (unless (or (s-ends-with? ".org" name)
                  (s-ends-with? "org.el" name))
        (hl-todo-mode))))
  :hook
  (prog-mode . zc-theme/maybe-init-hl-todo))

(use-package highlight-sexp
  :straight t
  :hook ((lisp-mode       . highlight-sexp-mode)
         (emacs-lisp-mode . highlight-sexp-mode))
  :custom
  ;; Lighten background color from doom theme
  (hl-sexp-background-color (doom-color 'bg-alt)))

(use-package highlight-thing
  :straight t
  :commands (global-highlight-thing-mode)
  :init (global-highlight-thing-mode)
  :config
  (setq highlight-thing-delay-seconds 0.5
        highlight-thing-case-sensitive-p t
        highlight-thing-exclude-thing-under-point t
        highlight-thing-excluded-major-modes '(magit-status-mode))

  ;; If the search string happens to be the symbol being
  ;; highlighted by `highlight-thing', the overlays it applies
  ;; should be removed, because `swiper' applies its own
  ;; overlays. Otherwise it can flicker between the two faces
  ;; as you move between candidates.
  (defun zc-theme/clear-highlight-for-swiper (&rest _)
    (when highlight-thing-mode (highlight-thing-remove-last))
    (when highlight-sexp-mode (move-overlay hl-sexp-overlay 0 0)))
  (advice-add 'swiper :before #'zc-theme/clear-highlight-for-swiper)

  ;; Disable `highlight-thing' for various cases
  (defun zc-theme/maybe-disable-highlight-thing (fn &rest args)
    (and
     ;; If symbol is highlighted by `ahs-highlight-now',
     ;; the flicker effect occurs on other candidates.
     (not (bound-and-true-p ahs-highlighted))
     ;; Ensure the original condition satisfies.
     (apply fn args)
     ;; Highlight the occurrences of a single character is
     ;; nonsense.
     (let ((thing (highlight-thing-get-thing-at-point)))
       (or (not (stringp thing)) (> (length thing) 1)))))
  (advice-add 'highlight-thing-should-highlight-p
              :around #'zc-theme/maybe-disable-highlight-thing))



(defun zc-theme/after-init ()
  "Setup frame default fonts."
  (if window-system
      (progn
        (add-to-list 'default-frame-alist `(font . ,zc-default-font))
        (add-to-list 'default-frame-alist '(internal-border-width . 0))

        ;; Workaround for initializing child frame by posframe
        ;; causes weird white square flicker on the screen.
        (add-to-list 'default-frame-alist `(background-color . ,(doom-color 'bg)))

        (set-face-font 'default        zc-default-font)
        (set-face-font 'variable-pitch zc-variable-pitch-font)
        (set-face-font 'fixed-pitch    zc-fixed-pitch-font))
    (when (not (eq system-type 'darwin))
      (menu-bar-mode -1))
    ;; Menu bar always off in text mode
    (menu-bar-mode -1)))
(add-hook 'after-init-hook 'zc-theme/after-init)

(defun zc-theme/no-fringes-in-minibuffer ()
  "Disable fringes in the minibuffer window."
  (set-window-fringes (minibuffer-window) 0 0 nil))
(add-hook 'minibuffer-setup-hook #'zc-theme/no-fringes-in-minibuffer)

(defun zc-theme/set-line-spacing ()
  "Add extra spacing above and below each line, which is
similar to using: `(setq-default line-spacing 1)', but avoid
affecting minibuffers which may cause problem on `ivy'."
  (setq-local default-text-properties
              '(line-spacing 0.25 line-height 1.25)))
(add-hook 'text-mode-hook #'zc-theme/set-line-spacing)
(add-hook 'text-mode-hook #'zc-theme/set-line-spacing)



(setq-default
 ;; Remove continuation arrow on right fringe
 fringe-indicator-alist (delq (assq 'continuation fringe-indicator-alist)
                              fringe-indicator-alist)

 mode-line-default-help-echo nil   ; disable mode-line mouseovers
 show-help-function nil            ; hide :help-echo text
 use-dialog-box nil                ; always avoid GUI
 visible-cursor nil
 visible-bell nil
 ring-bell-function 'ignore        ; no ring bell at all.
 inhibit-compacting-font-caches t  ; don’t compact font caches during GC

 ;; More reliable inter-window border
 ;; The native border "consumes" a pixel of the fringe on righter-most splits,
 ;; `window-divider' does not. Available since Emacs 25.1.
 window-divider-default-places t
 window-divider-default-bottom-width 0
 window-divider-default-right-width 1)

;; Disable ugly build-in modes
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))

(when (display-graphic-p)
  (setq initial-frame-alist '((width . 160) (height . 60))))

;; TODO: Default maximize frame and enter fullscreen mode
;; (setq frame-resize-pixelwise t)
;; (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
;; (run-at-time "2sec" nil (lambda () (toggle-frame-fullscreen)))



(provide 'zc-theme)
