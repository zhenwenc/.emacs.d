(eval-when-compile
  (require 'use-package))

(require 's)
(require 'f)

(autoload 'winum-get-number-string "winum")
(autoload 'all-the-icons-faicon "all-the-icons")
(autoload 'all-the-icons-octicon "all-the-icons")
(autoload 'all-the-icons-icon-for-mode "all-the-icons")

(defvar zc-layout/current-window-config-tag)


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

(defface zc-modeline/inactive nil
  "Primary face for elements in inactive window."
  :group 'mode-line)

(defface zc-modeline/active nil
  "Default face for elements."
  :group 'mode-line)

(defface zc-modeline/primary nil
  "Face for primary elements."
  :group 'mode-line)

(defface zc-modeline/accent nil
  "Face for accented elements"
  :group 'mode-line)

(defface zc-modeline/warning nil
  "Face for elements with warning"
  :group 'mode-line)

(defface zc-modeline/evil-active nil
  "Base face for evil state faces in active window."
  :group 'mode-line)

(defface zc-modeline/evil-inactive nil
  "Face for indicating evil state in inactive window."
  :group 'mode-line)

(defface zc-modeline/evil-normal-state nil
  "Face for indicating evil normal state."
  :group 'mode-line)

(defface zc-modeline/evil-insert-state nil
  "Face for indicating evil insert state."
  :group 'mode-line)

(defface zc-modeline/evil-visual-state nil
  "Face for indicating evil visual state."
  :group 'mode-line)

(defface zc-modeline/evil-motion-state nil
  "Face for indicating evil readonly state."
  :group 'mode-line)



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

(defun zc-modeline/narrowed-lines-icon ()
  (all-the-icons-faicon "scissors" :height 0.8 :v-adjust 0.05))

(defun zc-modeline/auto-recompile-icon ()
  (all-the-icons-faicon "repeat" :height 0.8 :v-adjust 0.05))



(defvar zc-modeline/active-window nil
  "Current active window.")

(defun zc-modeline/set-active-window (windows)
  (when (not (minibuffer-window-active-p (frame-selected-window)))
    (setq zc-modeline/active-window (selected-window))))
(add-function :before pre-redisplay-function #'zc-modeline/set-active-window)



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



;; (setq-default mode-line-format nil)

(setq-default
 mode-line-format
 '((:eval
    (let* ((active     (eq zc-modeline/active-window (get-buffer-window)))
           (face       (if active 'zc-modeline/active 'zc-modeline/inactive))
           (primary    (if active 'zc-modeline/primary face))
           (accent     (if active 'zc-modeline/accent  face))
           (warning    (if active 'zc-modeline/warning face))
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

                 ;; Buffer narrowed status by `consult-focus-lines'
                 (when consult--focus-lines-overlays
                   (list
                    (zc-modeline/separator)
                    (propertize (zc-modeline/narrowed-lines-icon) 'face warning)))

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
                 (propertize "%2c"  'face (if exceed-80col warning face))

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
;;   (setq mode-line-format   (default-value 'mode-line-format))
;;   (setq header-line-format (default-value 'header-line-format))
;;   (zc/measure-time (format-mode-line mode-line-format)))

(provide 'zc-modeline)
