;;; zc-web-modes.el --- Major modes derived from web-mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Frederick Cai

;; Author: Frederick Cai <frederick.cai@gmail.com>

;;; Code:

(require 'web-mode)

;;;###autoload
(define-derived-mode zc-web-js-mode web-mode "JS"
  "Derived mode for editing JavaScript files.")

;;;###autoload
(define-derived-mode zc-web-json-mode web-mode "JSON"
  "Derived mode for editing JSON files."
  (setq-local web-mode-content-type "json"))

;;;###autoload
(define-derived-mode zc-web-html-mode web-mode "HTML"
  "Derived mode for editing HTML files."
  (setq-local web-mode-content-type "html"))

;;;###autoload
(define-derived-mode zc-web-css-mode web-mode "CSS"
  "Derived mode for editing CSS files."
  (setq-local web-mode-content-type "css"))

(provide 'zc-web-modes)

;;; zc-web-modes.el ends here
