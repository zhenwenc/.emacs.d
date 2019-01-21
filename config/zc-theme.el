(eval-when-compile
  (require 'use-package))

(require 'f)
(require 'color)
(require 'general)



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
    (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
          doom-themes-enable-italic t) ; if nil, italics is universally disabled

    ;; Load the theme
    (setq doom-city-lights-brighter-comments nil)
    (setq doom-city-lights-brighter-modeline nil)
    (load-theme 'doom-city-lights t)

    ;; Slightly darken the default background to have higher contrast
    (set-background-color (doom-darken 'bg 0.05))
    (set-foreground-color (doom-lighten 'fg 0.3))

    ;; Lighten the decorators/annotation in TS/Java
    (set-face-attribute 'font-lock-preprocessor-face nil
                        :foreground (doom-lighten 'violet 0.2)
                        :weight 'light)

    ;; The string face was too dark
    (set-face-attribute 'font-lock-string-face nil
                        :foreground (doom-lighten 'green 0.1))

    ;; The function name face was too dark
    (set-face-attribute 'font-lock-function-name-face nil
                        :foreground (doom-color 'green))

    ;; The ivy match face was too close to the background
    (with-eval-after-load 'ivy
      (set-face-attribute 'ivy-current-match nil
                          :foreground (doom-color 'green))
      (set-face-attribute 'ivy-minibuffer-match-face-1 nil
                          :foreground (doom-color 'fg))
      (set-face-attribute 'ivy-virtual nil
                          :foreground (doom-darken 'fg 0.2)))

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
  :init
  (defun zc-theme/disable-all-the-icons-in-tty (orig-fn &rest args)
    (when (display-graphic-p)
      (apply orig-fn args)))
  ;; all-the-icons doesn't work in the terminal
  (dolist (fn '(all-the-icons-octicon all-the-icons-material
                                      all-the-icons-faicon all-the-icons-fileicon
                                      all-the-icons-wicon all-the-icons-alltheicon))
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
    (add-to-list 'all-the-icons-mode-icon-alist
                 '(terraform-mode all-the-icons-faicon "tree"
                                  :face all-the-icons-blue))

    (global-page-break-lines-mode)))

(use-package rainbow-delimiters
  :straight t)

(use-package hl-todo
  :straight t
  :preface
  (defun zc-theme/maybe-init-hl-todo ()
    (let ((name (buffer-name)))
      (unless (or (s-ends-with? ".org" name)
                  (s-ends-with? "org.el" name))
        (hl-todo-mode))))
  :hook (prog-mode . zc-theme/maybe-init-hl-todo))

(use-package highlight-sexp
  :straight t
  :hook ((lisp-mode . highlight-sexp-mode)
         (emacs-lisp-mode . highlight-sexp-mode))
  :config
  ;; Lighten background color from doom theme
  (setq hl-sexp-background-color (doom-color 'bg-alt)))

(use-package highlight-thing
  :straight t

  :preface
  (progn
    (defun zc-theme/clear-highlight-for-swiper (&rest _)
      (when highlight-thing-mode (highlight-thing-remove-last))
      (when highlight-sexp-mode (move-overlay hl-sexp-overlay 0 0)))

    (defun zc-theme/maybe-disable-highlight-thing (fn &rest args)
      (and
       ;; If symbol is highlighted by `ahs-highlight-now', the
       ;; flicker effect occurs on other candidates.
       (not ahs-highlighted)
       ;; Ensure the original condition satisfies.
       (apply fn args)
       ;; Highlight the occurrences of a single character is
       ;; nonsense.
       (let ((thing (highlight-thing-get-thing-at-point)))
         (or (not (stringp thing)) (> (length thing) 1))))))

  :commands (global-highlight-thing-mode)
  :init (global-highlight-thing-mode)
  :config
  (progn
    (setq highlight-thing-delay-seconds 0.1
          highlight-thing-case-sensitive-p t
          highlight-thing-exclude-thing-under-point t)

    (set-face-attribute 'highlight-thing nil
                        :foreground "#8BD49C"
                        :background "#283637")

    ;; If the search string happens to be the symbol being
    ;; highlighted by `highlight-thing', the overlays it applies
    ;; should be removed, because `swiper' applies its own
    ;; overlays. Otherwise it can flicker between the two faces
    ;; as you move between candidates.
    (advice-add 'swiper :before #'zc-theme/clear-highlight-for-swiper)

    ;; Disable `highlight-thing' for various cases
    (advice-add 'highlight-thing-should-highlight-p
                :around #'zc-theme/maybe-disable-highlight-thing)))



(defun zc-theme/after-init ()
  "Setup frame default fonts."
  (if window-system
      (progn
        (setq-default line-spacing 1)

        (add-to-list 'default-frame-alist `(font . ,zc-default-font))
        (add-to-list 'default-frame-alist '(internal-border-width . 0))

        (set-face-font 'default zc-default-font)
        (set-face-font 'variable-pitch zc-variable-pitch-font)
        (set-face-font 'fixed-pitch zc-fixed-pitch-font))
    (when (not (eq system-type 'darwin))
      (menu-bar-mode -1))
    ;; Menu bar always off in text mode
    (menu-bar-mode -1)))

(defun zc-theme/no-fringes-in-minibuffer ()
  "Disable fringes in the minibuffer window."
  (set-window-fringes (minibuffer-window) 0 0 nil))

(add-hook 'after-init-hook 'zc-theme/after-init)
(add-hook 'minibuffer-setup-hook #'zc-theme/no-fringes-in-minibuffer)



(setq-default
 ;; Remove continuation arrow on right fringe
 fringe-indicator-alist (delq (assq 'continuation fringe-indicator-alist)
                              fringe-indicator-alist)

 mode-line-default-help-echo nil ; disable mode-line mouseovers
 show-help-function nil          ; hide :help-echo text
 use-dialog-box nil              ; always avoid GUI
 visible-cursor nil
 visible-bell nil
 ring-bell-function 'ignore      ; no ring bell at all.

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

(provide 'zc-theme)
