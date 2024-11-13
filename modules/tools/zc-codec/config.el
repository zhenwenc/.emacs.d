;;; tools/zc-codec/config.el -*- lexical-binding: t; -*-

;; Viewing certificates, CRLs, keys, DH-parameters, EC-Parameters and ASN.1 using OpenSSL
;; Open a certificate, either PEM or DER encoded, in a buffer, execute `x509-dwim'.
;;
;; https://github.com/jobbflykt/x509-mode
(use-package! x509-dwim
  :commands (x509-dwim))
