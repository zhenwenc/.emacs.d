;;; lang/zc-mermaid/autoload.el -*- lexical-binding: t; -*-

;; HACK: Build diagram on org babel with custom behaviour:
;;
;; - When source block results set to `none', preview the result
;;   instead of output to file.
;;
;;;###autoload
(defun zc-mermaid/org-babel-execute (orig-fn body params)
  (funcall orig-fn body params)
  (when (s-equals? "none" (cdr (assq :results params)))
    (let* ((filename (cdr (assq :file params)))
           (buf (get-buffer-create zc-mermaid-preview-buffer))
           (inhibit-read-only t)
           (coding-system-for-read  (when (f-ext? filename "png") 'binary))
           (coding-system-for-write (when (f-ext? filename "png") 'binary)))
      (with-current-buffer buf
        (fundamental-mode)
        (erase-buffer)
        (insert-file-contents-literally filename)
        (image-mode))
      (save-selected-window
        (view-buffer-other-window buf)))))
