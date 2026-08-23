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
  :disabled t
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

  (defun zc/deepseek-api-key  () (zc/secrets-api-key :host "api.deepseek.com"))
  (defun zc/anthropic-api-key () (zc/secrets-api-key :host "api.anthropic.com"))

  ;; Set default backend
  ;;
  ;; For Github Copilot, each chat interactions count as a premium request, while
  ;; a paid plan has unlimited chat interactions using the included models:
  ;;
  ;; - [2026-01] GPT-5 mini, GPT-4.1 and GPT-4o
  ;;
  ;; https://docs.github.com/en/copilot/concepts/billing/copilot-requests#model-multipliers
  (setq gptel-backend (gptel-make-gh-copilot "Copilot" :stream t))
  (setq gptel-model 'gpt-4.1)

  ;; To use Anthropic backend, it lookup the API key from authinfo
  ;;
  ;;  machine api.anthropic.com login apikey password TOKEN
  ;;
  (gptel-make-anthropic "Claude"
    :stream t
    :key 'zc/anthropic-api-key)

  ;; To use DeepSeek backend, it lookup the API key from authinfo
  ;;
  ;;  machine api.deepseek.com login apikey password TOKEN
  ;;
  (gptel-make-openai "DeepSeek"
    :host "api.deepseek.com"
    :endpoint "/chat/completions"
    :stream t
    :key 'zc/deepseek-api-key
    :models '(deepseek-chat deepseek-coder))

  (gptel-make-preset 'chatgpt
    :description "Preset for ChatGPT chat"
    :backend "ChatGPT"
    :model 'gpt-4.1-mini)

  (gptel-make-preset 'copilot
    :description "Preset for Copilot chat"
    :backend "Copilot"
    :model 'claude-sonnet-4.5)

  (gptel-make-preset 'deepseek
    :description "Preset for DeepSeek chat"
    :backend "DeepSeek"
    :model 'deepseek-chat)

  (gptel-make-preset 'claude
    :description "Preset for Anthropic (Claude) chat"
    :backend "Claude"
    :model 'claude-4-5-sonnet-20250929
    :tools '("Glob" "Grep" "Read"))

  (gptel-make-preset 'claude-code
    :description "Preset for Anthropic (Claude) coding agent"
    :backend "Claude"
    :model 'claude-4-5-sonnet-20250929
    :tools '("Glob" "Grep" "Read" "Edit" "Insert"))
  )

(use-package! gptel-agent
  :disabled t
  :after (gptel)
  :config (gptel-agent-update))

(use-package! gptel-magit
  :disabled t
  :after (gptel)
  :when (modulep! :tools magit)
  :hook (magit-mode . gptel-magit-install))

(use-package! gptel-prompts
  :disabled t
  :after (gptel)
  :init (setq gptel-prompts-directory paths-prompts-dir)
  :config (gptel-prompts-update))
