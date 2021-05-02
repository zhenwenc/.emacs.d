(eval-when-compile
  (require 'use-package))



;; https://github.com/arnm/ob-mermaid
(use-package ob-mermaid
  :straight t
  :after org
  :config
  (setq ob-mermaid-cli-path (concat zc-org/directory "/node_modules/.bin/mmdc"))
  (setq zc-mermaid-preview-buffer "*Mermaid Preview*")

  ;; Default ignore babel execution results to preview
  (setq org-babel-default-header-args:mermaid
        (assoc-delete-all :results org-babel-default-header-args:mermaid))
  (add-to-list 'org-babel-default-header-args:mermaid '(:results . "none"))

  ;; HACK: Build diagram on org babel with custom behaviour:
  ;;
  ;; - When source block results set to `none', preview the result
  ;;   instead of output to file.
  ;;
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
  (advice-add #'org-babel-execute:mermaid :around #'zc-mermaid/org-babel-execute))



(provide 'zc-mermaid)
