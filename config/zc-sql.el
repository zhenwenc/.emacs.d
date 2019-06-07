(eval-when-compile
  (require 'use-package))

(require 'zc-sql-funcs)
(require 'zc-hydra-funcs)

(autoload 'sql-set-product-feature "sql")



(use-package sql-mode
  :straight nil
  :defer t

  :hydra
  ("Basic"
   (("f" zc-sql/format-region-or-buffer "format")))

  :config
  (progn
    ;; PostgreSQL databases with underscores in their names
    ;; trip up the prompt specified in `sql.el'.
    (sql-set-product-feature 'postgres :prompt-regexp "^[-[:alnum:]_]*=[#>] ")
    (sql-set-product-feature 'postgres :prompt-cont-regexp
                             "^[-[:alnum:]_]*[-(][#>] ")))



(provide 'zc-sql)
