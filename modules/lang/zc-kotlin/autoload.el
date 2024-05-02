;;; lang/zc-kotlin/autoload.el -*- lexical-binding: t; -*-


;; Org Mode Integration

;;;###autoload
(defun org-babel-execute:kotlin (body params)
  "Execute a block of Kotlin script with org-babel.
This function is called by `org-babel-execute-src-block'."
  (require 'ob-js) ;; HACK temporary
  (let* ((dir (or (cdr (assq :dir params)) zc-org/directory))
         (env (or (cdr (assq :env params)) ""))
         (exe (or (cdr (assq :cmd params)) "kotlinc"))
         (cmd-env "")
         (cmd-arg (s-join " " `("-script"
                                ;; "-classpath /Users/frederick.cai/code/android-mobile-credential-sdks/global/mattr/mobilecredential/mobile-credential-common/0.1.0/mobile-credential-common-0.1.0.aar"
                                "-classpath /Users/frederick.cai/code/android-mobile-credential-sdks/global/mattr/mobilecredential/mobile-credential-holder/0.2.5/mobile-credential-holder-0.2.5.aar"
                                )))
         (script-file (org-babel-temp-file "kotlin-" ".kts"))
         (cmd `(:cmd-compile ,(format "%s %s %s" exe cmd-arg script-file)
                :cmd-eval    ,(format "%s %s %s" exe cmd-arg (org-babel-process-file-name script-file))))
         (full-body (org-babel-expand-body:generic
                     ;; TODO proper Kotolin VAR-LINES support
                     body params (org-babel-variable-assignments:js params))))
    (with-temp-file script-file (insert full-body))
    ;; Execute the code block with `compilation'
    (if (s-equals? "yes" (cdr (assq :compile params)))
        ;; Do not highlight errors for arbitrary outputs
        (let ((compilation-start-hook '(lambda (&rest _ignore)
                                         (make-local-variable 'compilation-error-regexp-alist)
                                         (setq-local compilation-error-regexp-alist nil)))
              (term-name (if (s-equals? "no" (cdr (assq :color params))) "TERM=dumb" "")))
          (compile (format "%s %s %s" term-name cmd-env (plist-get cmd :cmd-compile))))
      ;; Execute the code block with `org-babel-execute'
      (user-error "Not supported yet!")
      ;; (let* ((result (org-babel-eval (format "%s %s" cmd-env (plist-get cmd :cmd-eval)) "")))
      ;;   (org-babel-result-cond (cdr (assq :result-params params))
      ;;     result (org-babel-js-read result)))
      )))
