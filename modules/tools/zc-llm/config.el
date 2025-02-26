;;; tools/zc-llm/config.el -*- lexical-binding: t; -*-

;;
;; This package uses ChatGPT API by default, it lookup the API key from authinfo
;;
;;   machine api.openai.com login apikey password TOKEN
;;
;;
;; Basic usage:
;; - Start a dedicated chat buffer: `SPC t C' or `M-x gptel'
;; - Set chat parameters for the session: `C-u C-c RET' or `M-x gptel-menu'
;;
(use-package! gptel
  :commands (gptel)
  :config
  ;; Doom binds `RET' in Org mode to `+org/dwim-at-point', which conflicts with
  ;; gptel’s transient menu bindings. Use `C-m' or `C-c RET' to send query.
  (setq gptel-default-mode 'org-mode)

  ;; Use DeepSeek as default backend, it lookup the API key from authinfo
  ;;
  ;;  machine api.deepseek.com login apikey password TOKEN
  ;;
  (let ((deepseek (gptel-make-openai "DeepSeek"
                    :host "api.deepseek.com"
                    :endpoint "/chat/completions"
                    :stream t
                    :key (zc/secrets-api-key :host "api.deepseek.com")
                    :models '(deepseek-chat deepseek-coder))))
    (setq gptel-model 'deepseek-chat)
    (setq gptel-backend deepseek)))
