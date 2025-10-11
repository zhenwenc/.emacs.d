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
;; https://github.com/karthink/gptel
(use-package! gptel
  :defer t
  :config
  ;; Doom specific, popup manager may bow out
  (setq gptel-display-buffer-action nil)
  (set-popup-rule!
    (lambda (bname _action)
      (and (null gptel-display-buffer-action)
           (buffer-local-value 'gptel-mode (get-buffer bname))))
    :select t :side 'right :size 0.5 :quit nil :ttl nil)

  ;; Doom binds `RET' in Org mode to `+org/dwim-at-point', which conflicts with
  ;; gptel’s transient menu bindings. Use `C-m' or `C-c RET' to send query.
  (setq gptel-default-mode 'org-mode)

  ;; Move cursor to the next prompt after response is inserted
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)

  ;; Set default backend
  ;;
  ;; To use Github Copilot backend, it auto prompt authentication
  ;;
  (setq gptel-backend (gptel-make-gh-copilot "Copilot"))
  (setq gptel-model 'claude-3.5-sonnet)

  ;; To use DeepSeek backend, it lookup the API key from authinfo
  ;;
  ;;  machine api.deepseek.com login apikey password TOKEN
  ;;
  ;; (gptel-make-openai "DeepSeek"
  ;;   :host "api.deepseek.com"
  ;;   :endpoint "/chat/completions"
  ;;   :stream t
  ;;   :key (zc/secrets-api-key :host "api.deepseek.com")
  ;;   :models '(deepseek-chat deepseek-coder))

  (gptel-make-preset 'chatgpt-mini
    :description "Preset for ChatGPT general chat"
    :backend "ChatGPT"
    :model 'gpt-4.1-mini)

  (gptel-make-preset 'copilot-sonnet
    :description "Preset for Copilot sonnet chat"
    :backend "Copilot"
    :model 'claude-3.5-sonnet)

  (gptel-make-preset 'copilot-beast
    :parents 'copilot-sonnet
    :description "Preset for Copilot Beast Mode chat"
    :system (f-read-text (expand-file-name "beastmode.md" paths-prompts-dir)))

  ;; (gptel-make-preset 'deepseek
  ;;   :description "Preset for DeepSeek chat"
  ;;   :backend "DeepSeek"
  ;;   :model 'deepseek-chat)
  )

(use-package! gptel-magit
  :when (modulep! :tools magit)
  :hook (magit-mode . gptel-magit-install))
