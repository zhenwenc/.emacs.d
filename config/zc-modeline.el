(eval-when-compile
  (require 'use-package))

(require 's)
(require 'f)
(require 'zc-layout)

(autoload 'winum-get-number-string "winum")
(autoload 'all-the-icons-faicon "all-the-icons")
(autoload 'all-the-icons-octicon "all-the-icons")
(autoload 'all-the-icons-icon-for-mode "all-the-icons")



(defvar zc-modeline/active-window nil
  "Current active window.")

(defun zc-modeline/set-active-window (windows)
  (when (not (minibuffer-window-active-p (frame-selected-window)))
    (setq zc-modeline/active-window (selected-window))))
(add-function :before pre-redisplay-function #'zc-modeline/set-active-window)


;; LSP Info

(defvar-local zc-modeline/lsp-info nil
  "Current LSP workspace and status.")

(defun zc-modeline/update-lsp-info (&rest _)
  (let* ((workspace (and (bound-and-true-p lsp-mode)
                         (car (lsp-workspaces))))
         (server-id (when workspace (lsp--workspace-print workspace))))
    (setq zc-modeline/lsp-info server-id)))

(with-eval-after-load 'lsp-mode
  (dolist (hook '(lsp-mode-hook
                  lsp-after-initialize-hook
                  lsp-after-uninitialized-hook))
    (add-hook hook #'zc-modeline/update-lsp-info)))


;; Functions

(defun zc-modeline/get-width (values)
  "Get the length of VALUES."
  (if values
      (let ((val (car values)))
        (+ (cond
            ((stringp val) (string-width (format-mode-line val)))
            ((and (listp val) (eq 'image (car val)))
             (car (image-size val)))
            (t 0))
           (zc-modeline/get-width (cdr values))))
    0))


;; Faces

(defface zc-modeline/inactive '((t (:inherit mode-line)))
  "Primary face for elements in inactive window."
  :group 'mode-line)

(defface zc-modeline/active '((t (:inherit mode-line)))
  "Default face for elements."
  :group 'mode-line)

(defface zc-modeline/primary '((t (:inherit mode-line)))
  "Face for primary elements."
  :group 'mode-line)

(defface zc-modeline/accent '((t (:inherit mode-line)))
  "Face for accented elements"
  :group 'mode-line)

(defface zc-modeline/warning '((t (:inherit mode-line)))
  "Face for elements with warning"
  :group 'mode-line)

(defface zc-modeline/evil-inactive '((t (:inherit mode-line)))
  "Face for indicating evil state in inactive window."
  :group 'mode-line)

(defface zc-modeline/evil-normal-state '((t (:inherit mode-line)))
  "Face for indicating evil normal state."
  :group 'mode-line)

(defface zc-modeline/evil-insert-state '((t (:inherit mode-line)))
  "Face for indicating evil insert state."
  :group 'mode-line)

(defface zc-modeline/evil-visual-state '((t (:inherit mode-line)))
  "Face for indicating evil visual state."
  :group 'mode-line)

(defface zc-modeline/evil-motion-state '((t (:inherit mode-line)))
  "Face for indicating evil readonly state."
  :group 'mode-line)

(let* ((gui           (display-graphic-p))
       (bg            (if gui "#1D2026" nil))
       (text          "#D5D8DC")
       (text-inverse  "#3B4044")
       (text-inactive "#5B6268")
       (primary       "#4296EC")
       (accent        "#FFD203")
       (warning       "#FF0266")
       (normal        "#E6AF3F")
       (insert        "#2ECC71")
       (motion        "#EE42F4")
       (visual        "#ECF0F1")
       (padded         3))

  (set-face-attribute 'mode-line nil
                      :foreground text :background bg
                      :box `(:line-width ,padded :color ,bg :style nil))
  (set-face-attribute 'mode-line-inactive nil
                      :foreground text-inactive :background bg)

  (set-face-attribute 'zc-modeline/active nil)
  (set-face-attribute 'zc-modeline/inactive nil
                      :inherit 'mode-line-inactive)

  (set-face-attribute 'zc-modeline/primary nil
                      :inherit 'zc-modeline/active
                      :foreground primary)

  (set-face-attribute 'zc-modeline/accent nil
                      :inherit 'zc-modeline/active
                      :foreground accent)

  (set-face-attribute 'zc-modeline/warning nil
                      :inherit 'zc-modeline/active
                      :foreground warning :weight 'bold)

  (set-face-attribute 'zc-modeline/evil-inactive nil
                      :inherit 'zc-modeline/active
                      :foreground text-inactive
                      :background bg
                      :weight     'bold
                      :box `(:line-width ,padded :color ,bg :style nil))

  (set-face-attribute 'zc-modeline/evil-visual-state nil
                      :inherit 'zc-modeline/active
                      :foreground text-inverse
                      :background motion
                      :weight     'bold
                      :box `(:line-width ,padded :color ,motion :style nil))

  (set-face-attribute 'zc-modeline/evil-normal-state nil
                      :inherit 'zc-modeline/active
                      :foreground text-inverse
                      :background normal
                      :weight     'bold
                      :box `(:line-width ,padded :color ,normal :style nil))

  (set-face-attribute 'zc-modeline/evil-insert-state nil
                      :inherit 'zc-modeline/active
                      :foreground text-inverse
                      :background insert
                      :weight     'bold
                      :box `(:line-width ,padded :color ,insert :style nil))

  (set-face-attribute 'zc-modeline/evil-visual-state nil
                      :inherit 'zc-modeline/active
                      :foreground text-inverse
                      :background visual
                      :weight     'bold
                      :box `(:line-width ,padded :color ,visual :style nil)))



(defun zc-modeline/separator () " ")

(defun zc-modeline/major-mode-info ()
  (let* ((mode-name (pcase major-mode
                      ('sh-mode         "sh")
                      ('scala-mode      "Scala")
                      ('rustic-mode     "Rust")
                      ('typescript-mode "Typescript")
                      ('emacs-lisp-mode "Elisp")
                      (guard major-mode))))
    (format "%s %s"
            (all-the-icons-icon-for-mode major-mode :height 0.8 :v-adjust 0.05)
            mode-name)))

(defun zc-modeline/status-info ()
  (let* ((layout zc-layout/current-window-config-tag)
         (window-number (winum-get-number-string))
         (window-unicode (pcase window-number
                           ("1"   "➊")
                           ("2"   "➋")
                           ("3"   "➌")
                           ("4"   "➍")
                           ("5"   "➎")
                           ("6"   "➏")
                           ("7"   "➐")
                           ("8"   "➑")
                           ("9"   "➒")
                           ("10"  "➓")
                           (guard window-number))))
    (unless (and layout (> (length layout) 0))
      (setq layout "Default"))
    (format " %s %s " layout window-unicode)))

(defun zc-modeline/vc-info ()
  (if (and vc-mode buffer-file-name)
      (format "%s %s"
              (all-the-icons-octicon "git-branch" :height 0.8 :v-adjust 0.05)
              (replace-regexp-in-string "^ Git[:-]" "" vc-mode))
    ""))

(defun zc-modeline/access-mode-info ()
  (let ((str (concat
              (if (and (buffer-file-name)
                       (file-remote-p (buffer-file-name))) "@" "")
              (if buffer-read-only "%%" "-")
              (if (buffer-modified-p) "*" "-"))))
    (s-pad-right 2 " " str)))

(defun zc-modeline/directory-info ()
  (cond
   ;; Skip for special buffers
   ((or (not buffer-file-name)
        (string-match-p (rx bos "*" (*? anything) "*" eos) (buffer-name)))
    "")
   ((> (f-depth default-directory) 1)
    (format "%s/" (f-base default-directory)))
   (t default-directory)))

(defun zc-modeline/buffer-info ()
  (or (and (buffer-file-name)
           (f-filename (buffer-file-name)))
      (buffer-name)))

(defun zc-modeline/dedicated-icon ()
  (all-the-icons-faicon "lock" :height 0.8 :v-adjust 0.05))

(defun zc-modeline/narrowed-icon ()
  (all-the-icons-faicon "compress" :height 0.8 :v-adjust 0.05))

(defun zc-modeline/auto-recompile-icon ()
  (all-the-icons-faicon "repeat" :height 0.8 :v-adjust 0.05))



(setq-default
 mode-line-format
 '((:eval
    (let* ((active (eq zc-modeline/active-window (get-buffer-window)))
           (face (if active 'zc-modeline/active 'zc-modeline/inactive))
           (primary (if active 'zc-modeline/primary face))
           (accent (if active 'zc-modeline/accent face))
           (warning (if active 'zc-modeline/warning face))
           (evil-state (cond
                        ((not active)          'zc-modeline/evil-inactive)
                        (buffer-read-only      'zc-modeline/evil-motion-state)
                        ((evil-motion-state-p) 'zc-modeline/evil-motion-state)
                        ((evil-insert-state-p) 'zc-modeline/evil-insert-state)
                        ((evil-visual-state-p) 'zc-modeline/evil-visual-state)
                        (t                     'zc-modeline/evil-normal-state)))
           (exceed-80col (>= (current-column) 80))

           (lhs (list
                 ;; Print error on low memory
                 "%e"

                 ;; Emacsclient info
                 mode-line-client

                 ;; Mode line status name
                 (propertize (zc-modeline/status-info) 'face evil-state)

                 (zc-modeline/separator)
                 (propertize (zc-modeline/access-mode-info) 'face face)

                 ;; Shorten directory name
                 (zc-modeline/separator)
                 (propertize (zc-modeline/directory-info) 'face face)

                 ;; Current buffer name
                 "%[" (propertize (zc-modeline/buffer-info) 'face primary) "%]"

                 ;; Window status info
                 (when (window-dedicated-p)
                   (list
                    (zc-modeline/separator)
                    (propertize (zc-modeline/dedicated-icon) 'face warning)))

                 ;; Buffer narrowed status
                 (when (buffer-narrowed-p)
                   (list
                    (zc-modeline/separator)
                    (propertize (zc-modeline/narrowed-icon) 'face warning)))

                 ;; Auto-recompile status
                 (when zc-eval/compile-on-save-mode
                   (list
                    (zc-modeline/separator)
                    (propertize (zc-modeline/auto-recompile-icon) 'face warning)))
                 ))

           (rhs (list
                 (zc-modeline/separator)
                 (or zc-modeline/lsp-info "")

                 ;; Current column and line positions
                 (zc-modeline/separator)
                 (propertize "%4l:" 'face face)
                 (propertize "%2c" 'face (if exceed-80col warning face))

                 ;; Project info
                 (zc-modeline/separator)
                 (propertize (zc-modeline/vc-info) 'face accent)

                 ;; Major mode
                 (zc-modeline/separator)
                 (propertize (zc-modeline/major-mode-info) 'face primary)
                 ))

           (spc (list
                 (propertize
                  " "
                  'display `((space :align-to
                               (- (+ right
                                     right-fringe
                                     right-margin
                                     -2) ;; what is this?
                                  ,(zc-modeline/get-width rhs))))
                  'face face))))

      (-concat lhs spc rhs)))
   ))



;; Testing
;; (progn
;;   (setq mode-line-format (default-value 'mode-line-format))
;;   (zc/measure-time (format-mode-line mode-line-format)))

(provide 'zc-modeline)
