(eval-when-compile
  (require 'use-package))

(require 's)
(require 'zc-paths)

(let ((dir (expand-file-name (concat paths-vendor-dir "plantuml/")))
      (url "https://raw.githubusercontent.com/RicardoNiepel/C4-PlantUML/release/1-0/"))

  (defconst zc-plantuml/component (concat dir "C4_Component.puml"))
  (defconst zc-plantuml/container (concat dir "C4_Container.puml"))

  (defconst zc-plantuml/component-url (concat url "C4_Component.puml"))
  (defconst zc-plantuml/container-url (concat url "C4_Container.puml")))



(use-package plantuml-mode
  :straight t
  :commands zc-plantuml/download

  :preface
  (defun zc-plantuml/download (&optional forced)
    ;; Download PlantUML JAR if not already exists.
    (interactive "P")
    (unless (and (not forced)
                 (or (f-exists? plantuml-jar-path)
                     (f-exists? plantuml-executable-path)))
      (plantuml-download-jar))
    ;; Download extra custom diagram types.
    (mapcar (-lambda ((filename url))
              (unless (and (not forced) (file-exists? filename))
                (with-current-buffer
                    (url-retrieve-synchronously url 'silent 'inhibit-cookies)
                  (delete-region (point-min) url-http-end-of-headers)
                  (write-file filename))))
            `((,zc-plantuml/container ,zc-plantuml/container-url)
              (,zc-plantuml/component ,zc-plantuml/component-url))))

  :config
  (setq plantuml-jar-path        (concat paths-vendor-dir "plantuml/plantuml.jar")
        plantuml-executable-path (concat paths-vendor-dir "plantuml/plantuml")
        plantuml-default-exec-mode 'jar)

  ;; Fix `--illegal-access=deny' doesn't supported by Java 8.
  (setq plantuml-java-args '("-Djava.awt.headless=true" "-jar"))

  ;; Settings for `ob-plantuml'.
  ;;
  ;; - Use "!includeurl componenturl" to import diagram type.
  ;;
  (setq org-plantuml-jar-path plantuml-jar-path)
  (dolist (var `((:cmdline . ,(s-join " " plantuml-jar-args))
                 (:var     . ,(format "componenturl=\"%s\"" zc-plantuml/component-url))
                 (:var     . ,(format "containerurl=\"%s\"" zc-plantuml/container-url))))
    (add-to-list 'org-babel-default-header-args:plantuml var))

  ;; Default ignore babel execution results to preview
  (setq org-babel-default-header-args:plantuml
        (assoc-delete-all :results org-babel-default-header-args:plantuml))
  (add-to-list 'org-babel-default-header-args:plantuml '(:results . "none"))

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
  (defun zc-plantuml/org-babel-execute (orig-fn body params)
    (if (s-equals? "none" (cdr (assq :results params)))
        (let* ((output-type          (cdr (assq :output params)))
               (plantuml-output-type (or output-type "png"))
               (full-body (org-babel-plantuml-make-body body params)))
          ;; We want to stay on the current buffer
          (plantuml-preview-string 0 full-body))
      (unless (s-contains? "file" (cdr (assq :results params)))
        (user-error "You must specify \":results file replace\" header argument"))
      (funcall orig-fn body params)))
  (advice-add #'org-babel-execute:plantuml :around #'zc-plantuml/org-babel-execute)

  ;; Execute command with PlantUML
  ;;
  ;; - Retrieve the language specification:  "-language"
  ;; - Retrieve the list of installed fonts: "-printfonts"
  ;;
  ;; https://plantuml.com/command-line
  (defun zc-plantuml/execute-command (args)
    "Execute PlantUML command with arguments."
    (interactive "MCommand: ")
    (with-temp-buffer
      (let ((cmd-args (append (list plantuml-java-command nil t nil)
                              (plantuml-jar-render-command args))))
        (apply 'call-process cmd-args)
        (message (buffer-string))))))



(provide 'zc-plantuml)
