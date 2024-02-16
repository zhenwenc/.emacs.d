;;; lang/zc-plantuml/config.el -*- lexical-binding: t; -*-

(defconst zc-plantuml/cache-dir (expand-file-name (concat doom-cache-dir "plantuml/")))
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

(use-package! plantuml-mode
  :commands plantuml-download-jar

  :mode     ("\\.puml\\'" . plantuml-mode)
  :interpreter ("puml"    . plantuml-mode)

  :init
  (setq plantuml-jar-path (concat doom-data-dir "plantuml.jar")
        org-plantuml-jar-path plantuml-jar-path)

  :config
  (set-popup-rule! "^\\*PLANTUML" :size 0.5 :select nil :ttl 0)

  (setq plantuml-default-exec-mode
        (cond ((file-exists-p plantuml-jar-path) 'jar)
              ((executable-find "plantuml") 'executable)
              (plantuml-default-exec-mode)))

  ;; PLANTUML_LIMIT_SIZE: Increased diagram size limit, default to 4096.
  (defconst zc-plantuml/java-args '("-DPLANTUML_LIMIT_SIZE=8192"))

  ;; Fix `--illegal-access=deny' doesn't supported by Java 8.
  (setq plantuml-java-args (-concat zc-plantuml/java-args '("-Djava.awt.headless=true" "-jar"))))

(after! ob-plantuml
  ;; HACK Force ob-plantuml to use `plantuml-mode''s building mechanism, which
  ;;      is more sophisticated.
  (advice-add #'org-babel-execute:plantuml :around #'zc-plantuml/org-babel-execute)
  (add-to-list 'org-babel-default-header-args:plantuml '(:cmdline . "-charset utf-8"))

  ;; (setq org-plantuml-jar-path plantuml-jar-path)

  ;; - Use "!includeurl componenturl" to import diagram type.
  ;;
  (dolist (var (-flatten `(((:cmdline . ,(s-join " " plantuml-jar-args))
                            (:var . ,(format "LocalPuml=\"%s\"" zc-plantuml/cache-dir)))
                           ,(-map (-lambda ((name . (&plist :url url)))
                                    `(:var . ,(format "%s=\"%s\"" name url)))
                                  zc-plantuml/resources))))
    (add-to-list 'org-babel-default-header-args:plantuml var)))
