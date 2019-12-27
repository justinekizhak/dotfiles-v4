;;;  -*- lexical-binding: t; -*-
;;;  -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)
;;
(package! ob-typescript)
(package! indium)
(package! prettier-js)
(package! org-fancy-priorities)
(package! move-text)
(package! vue-mode)
(package! origami)
(package! org-super-agenda)
(package! deadgrep)
(package! org-brain)
(package! ascii-art-to-unicode)
(package! browse-kill-ring)
(package! wgrep)  ;; Add support for project wide search and replace
(package! dart-mode)  ;; Support for Dart language
(package! flutter)  ;; Support for Flutter SDK
(package! company-dart :recipe (:host github :repo "sid-kurias/company-dart")) ;; Company completion for Dart
(package! company-lsp)  ;; Company frontend for lsp
(package! lsp-mode)  ;; lsp support
