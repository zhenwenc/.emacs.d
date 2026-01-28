(require 'f)


;;; Functions

(cl-defun zc-org/file-with-exts (&key ext dir)
  "Return files in `org-directory' that matches extension in EXTS."
  (unless dir (setq dir zc-org/directory))
  (unless ext (setq ext '("org")))
  (f-files dir (-compose (-partial #'-contains? ext) #'f-ext) nil))



;; Do not use constant to allow local module override for
;; environment specific settings.
(defvar zc-org/directory "~/notes")
(defvar zc-org/main-notes-dir (f-join zc-org/directory "main"))
(defvar zc-org/work-notes-dir (f-join zc-org/directory "work"))

;; File directories must be defined at `:init' block
;; so that they are visible to the navigation functions,
;; such as `zc-org/goto-agenda-file-heading'.
(setq org-directory          (f-expand zc-org/directory)
      org-attach-id-dir      (f-join org-directory "data")
      org-persist-directory  (f-join paths-cache-dir "org-persist")
      org-id-locations-file  (f-join paths-cache-dir "org-id-locations")

      org-default-notes-file (f-join zc-org/main-notes-dir "notes.org")
      org-default-todos-file (f-join zc-org/main-notes-dir "todos.org")
      org-default-babel-file (f-join zc-org/main-notes-dir "babel.org")
      org-agenda-diary-file  (f-join zc-org/main-notes-dir "diary.org")

      org-agenda-files       (append (zc-org/file-with-exts :dir zc-org/directory)
                                     (zc-org/file-with-exts :dir zc-org/main-notes-dir)
                                     (zc-org/file-with-exts :dir zc-org/work-notes-dir)))
