;;; os/zc-macos/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +macos/reveal-in-browser ()
  (interactive)
  (+macos-open-with "Brave Browser" buffer-file-name))
