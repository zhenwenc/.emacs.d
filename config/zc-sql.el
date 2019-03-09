;; We use `pg_format'(https://github.com/darold/pgFormatter)
;; to prettify the SQL queries.
;;
;; Install the package with:
;;   brew install pgFormatter

(eval-when-compile
  (require 'use-package))

(require 'zc-sql-funcs)
(require 'zc-hydra-funcs)

(autoload 'sql-set-product-feature "sql")



(use-package sql-mode
  :defer t
  :config
  (progn
    ;; PostgreSQL databases with underscores in their names
    ;; trip up the prompt specified in `sql.el'.
    (sql-set-product-feature 'postgres :prompt-regexp "^[-[:alnum:]_]*=[#>] ")
    (sql-set-product-feature 'postgres :prompt-cont-regexp
                             "^[-[:alnum:]_]*[-(][#>] ")))



(zc-hydra/major-mode-define sql-mode
  ("Basic"
   (("f" zc-sql/format-region-or-buffer "format"))))



(provide 'zc-sql)
