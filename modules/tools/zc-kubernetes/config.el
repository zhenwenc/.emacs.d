;;; tools/zc-kubernetes/config.el -*- lexical-binding: t; -*-

(use-package! kubernetes
  :commands (kubernetes-overview))

(use-package! kubernetes-evil
  :ensure t
  :after kubernetes)
