;;; tools/zc-docker/config.el -*- lexical-binding: t; -*-

(with-eval-after-load 'docker-process
  (setq docker-run-async-with-buffer-function 'docker-run-async))

(with-eval-after-load 'docker-compose
  ;; Use compose V2 command
  (setq docker-compose-command "docker compose"))
