;;; tools/zc-docker/config.el -*- lexical-binding: t; -*-

(with-eval-after-load 'docker-process
  ;; FIXME This option has been deprecated since 2.5.0
  ;;       How to run docker compose commands asynchronously now?
  ;; (setq docker-run-async-with-buffer-function 'docker-run-async)
  (setq docker-run-async-with-buffer-function nil)

  (setq docker-terminal-backend 'auto))

(with-eval-after-load 'docker-compose
  ;; Use compose V2 command
  (setq docker-compose-command "docker compose"))
