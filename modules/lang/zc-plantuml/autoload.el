;;; lang/zc-plantuml/autoload.el -*- lexical-binding: nil; -*-

;; HACK: Build diagram on org babel with custom behaviour:
;;
;; - When source block results set to `none', use `plantuml-mode'
;;   preview instead of output to file.
;;
;; - Preview diagram output to `PNG' instead of `SVG', otherwise
;;   displaying the image on dark background looks weird. E.g.:
;;
;;   skinparam backgroundColor #EEEBDC
;;
;;   Use `:output' to specify output type: txt, svg, or png.
;;
;;;###autoload
(defun zc-plantuml/org-babel-execute (orig-fn body params)
  (if (and (not (bound-and-true-p org-export-current-backend)) ;; not in `org-export-as' call
           (not (s-equals? "no" (cdr (assq :preview params)))))
      (let* ((output-type          (cdr (assq :output params)))
             (window-type          (cdr (assq :window params)))
             (height               (cdr (assq :height params)))
             (width                (cdr (assq :width  params)))
             (plantuml-output-type (or output-type "png"))
             (full-body (org-babel-plantuml-make-body body params)))

        ;; TODO Adjust popup window layout
        ;; (setf (cdr (assoc (rx bos "*PLANTUML Preview*" eos) display-buffer-alist))
        ;;       `((display-buffer-reuse-window
        ;;          display-buffer-in-side-window)
        ;;         (reusable-frames . visible)
        ;;         (window-width    . ,(or width  0.5))
        ;;         (window-height   . ,(or height 0.35))
        ;;         (side            . ,(or window-type 'right))
        ;;         (slot            . 1)))

        ;; We want to stay on the current buffer
        (plantuml-preview-string 0 full-body))
    (unless (s-contains? "file" (cdr (assq :results params)))
      (user-error "You must specify \":results file replace\" header argument"))
    (unless (cdr (assq :java params))
      ;; (push (cons :java (s-join " " plantuml-java-args)) params)
      (add-to-list 'params (cons :java (s-join " " plantuml-java-args)))
      )
    ;; Execute script using the original function
    (funcall orig-fn body params)))
