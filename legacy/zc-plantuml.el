(eval-when-compile
  (require 'use-package))

(require 's)
(require 'zc-paths)

(defconst zc-plantuml/cache-dir (expand-file-name (concat paths-vendor-dir "plantuml/")))
(defconst zc-plantuml/resources
  '(;; https://github.com/plantuml-stdlib/C4-PlantUML
    (C4Puml
     . (:url "https://raw.githubusercontent.com/plantuml-stdlib/C4-PlantUML/master"
        :files ("C4_Component.puml"
                "C4_Container.puml")))
    ;; https://github.com/plantuml-stdlib/Azure-PlantUML
    (AzurePuml
     . (:url "https://raw.githubusercontent.com/plantuml-stdlib/Azure-PlantUML/master/dist"
        :files ("AzureCommon.puml")))
    ;; https://github.com/dcasati/kubernetes-PlantUML
    (KubernetesPuml
     . (:url "https://raw.githubusercontent.com/dcasati/kubernetes-PlantUML/master/dist"
        :files ("OSS/KubernetesSvc.puml"
                "OSS/KubernetesPod.puml")))))



(use-package plantuml-mode
  :straight t
  :defer t

  :mode     ("\\.puml\\'" . plantuml-mode)
  :interpreter ("puml"    . plantuml-mode)

  :config
  (defun zc-plantuml/download (&optional forced)
    ;; Download PlantUML JAR if not already exists.
    (interactive "P")
    (unless (and (not forced)
                 (or (f-exists? plantuml-jar-path)
                     (f-exists? plantuml-executable-path)))
      (plantuml-download-jar))
    ;; Download additional custom diagram components.
    (mapcar (-lambda ((name . (&plist :url url :files files)))
              (--each files
                (let ((uri (format "%s/%s" url it))
                      (filename (concat zc-plantuml/cache-dir it)))
                  (message "Downloading %s" uri)
                  (with-current-buffer
                      (url-retrieve-synchronously uri 'silent 'inhibit-cookies)
                    (delete-region (point-min) url-http-end-of-headers)
                    (write-file filename)))))
            zc-plantuml/resources))

  (setq plantuml-jar-path        (concat paths-vendor-dir "plantuml/plantuml.jar")
        plantuml-executable-path (concat paths-vendor-dir "plantuml/plantuml")
        plantuml-default-exec-mode 'jar)

  ;; PLANTUML_LIMIT_SIZE: Increased diagram size limit, default to 4096.
  (defconst zc-plantuml/java-args '("-DPLANTUML_LIMIT_SIZE=8192"))

  ;; Fix `--illegal-access=deny' doesn't supported by Java 8.
  (setq plantuml-java-args (-concat zc-plantuml/java-args '("-Djava.awt.headless=true" "-jar")))

  ;; Settings for `ob-plantuml'.
  ;;
  ;; - Use "!includeurl componenturl" to import diagram type.
  ;;
  (setq org-plantuml-jar-path plantuml-jar-path)
  (dolist (var (-flatten `(((:cmdline . ,(s-join " " plantuml-jar-args))
                            (:var . ,(format "LocalPuml=\"%s\"" zc-plantuml/cache-dir)))
                           ,(-map (-lambda ((name . (&plist :url url)))
                                    `(:var . ,(format "%s=\"%s\"" name url)))
                                  zc-plantuml/resources))))
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
    (if (and (null org-export-current-backend) ;; not in `org-export-as' call
             (not (s-equals? "no" (cdr (assq :preview params)))))
        (let* ((output-type          (cdr (assq :output params)))
               (window-type          (cdr (assq :window params)))
               (height               (cdr (assq :height params)))
               (width                (cdr (assq :width  params)))
               (plantuml-output-type (or output-type "png"))
               (full-body (org-babel-plantuml-make-body body params)))
          ;; Adjust popup window layout
          (setf (cdr (assoc (rx bos "*PLANTUML Preview*" eos) display-buffer-alist))
                `((display-buffer-reuse-window
                   display-buffer-in-side-window)
                  (reusable-frames . visible)
                  (window-width    . ,(or width  0.5))
                  (window-height   . ,(or height 0.35))
                  (side            . ,(or window-type 'right))
                  (slot            . 1)))
          ;; We want to stay on the current buffer
          (plantuml-preview-string 0 full-body))
      (unless (s-contains? "file" (cdr (assq :results params)))
        (user-error "You must specify \":results file replace\" header argument"))
      (unless (cdr (assq :java params))
        (add-to-list 'params (cons :java (s-join " " plantuml-java-args))))
      ;; Execute script using the original function
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
