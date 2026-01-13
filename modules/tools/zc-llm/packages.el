;; -*- no-byte-compile: t; -*-
;;; tools/zc-llm/packages.el

;; https://github.com/karthink/gptel
(package! gptel)
(package! gptel-magit)

;; https://github.com/karthink/gptel-agent
(package! gptel-agent)

;; https://github.com/jwiegley/gptel-prompts
(package! gptel-prompts :recipe (:host github :repo "jwiegley/gptel-prompts"))
