;;; core/zc-prelude/autoload/window.el -*- lexical-binding: t; -*-

;;;###autoload
(defun zc/toggle-maximize-window ()
  "Maximize window."
  (interactive)
  (let* ((win (window-normalize-window nil))
         (win-height (window-parameter win 'window-height)))
    (cond
     ;; If window is side window, which can not be the only
     ;; window, resize the window
     ((window-parameter win 'window-side)
      (evil-window-set-height nil))
     ;; If window maybe maximized, to restore the previous
     ;; window layout
     ((and (= 1 (length (window-list)))
           (assoc ?_ register-alist))
      (jump-to-register ?_))
     ;; Miximize the selected window
     (t
      (window-configuration-to-register ?_)
      (delete-other-windows win)))))

;;;###autoload
(defun zc/toggle-current-window-dedication ()
  "Toggle whether the current window is dedicated to its current buffer."
  (interactive)
  (let* ((window (selected-window))
         (was-dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not was-dedicated))
    (message "Window %sdedicated to %s"
             (if was-dedicated "no longer " "")
             (buffer-name))))
