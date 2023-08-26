;;; tools/zc-docker/config.el -*- lexical-binding: t; -*-

(after! docker-process
  (setq docker-run-async-with-buffer-function 'docker-run-async))

(after! docker-compose
  ;; Use compose V2 command
  (setq docker-compose-command "docker compose"))
