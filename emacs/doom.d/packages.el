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
;; (package! browse-kill-ring)
(package! wgrep)  ;; Add support for project wide search and replace
(package! dart-mode)  ;; Support for Dart language
(package! flutter)  ;; Support for Flutter SDK
(package! company-dart :recipe (:host github :repo "sid-kurias/company-dart")) ;; Company completion for Dart
(package! company-lsp)  ;; Company frontend for lsp
(package! lsp-mode)  ;; lsp support
(package! goto-line-preview)  ;; Go to line (M-g g) with preview option
(package! tiny)  ;; Number ranges
;; (package! lentic)  ;; Same buffer multiple mode for literate programming
(package! ialign)  ;; Aligning content
(package! clipmon)  ;; Clipboard monitor
(package! zone)  ;; Screensaver emacs
(package! header3 :recipe (:host github :repo "justinekizhak/header3"))
;; (package! zone-end-of-buffer :recipe (:host gitlab :repo "justinekizhak/zone-end-of-buffer"))
(package! apex-legends-quotes :recipe (:host gitlab :repo "justinekizhak/apex-legends-quotes"))
(package! company-tabnine)  ;; Autocomplete using deep learing
(package! esup)
(package! company-box)  ;; Icons for auto complete popup
(package! treemacs-magit)  ;; Magit Treemacs
(package! 2048-game)  ;; TP
(package! yasnippet-snippets)  ;; Snippets for yas
(package! ox-gfm)  ;;Github Flavored Markdown exporter for Org Mode
(package! paren)  ;; Show matching parentheses
(package! toc-org)  ;; TOC for org mode
(package! ox-reveal)  ;; Create beautiful org presentations
(package! htmlize)    ; Syntax highlighting for codeblocks within org reveal presentations
(package! openapi-yaml-mode :recipe (:host github :repo "magoyette/openapi-yaml-mode"))
(package! highlight-indent-guides)  ;; Highlight indentation
(package! restclient)  ;; Rest API client within Emacs
(package! ob-restclient)  ;; Use restclient within Org file
(package! discover-my-major)  ;; Discover key bindings
(package! iedit)  ;; Edit all occurance of symbol simultanously
(package! powerthesaurus)  ;; Thesaurus for Emacs
(package! ace-popup-menu)  ;; Use better popup menu for Emacs
(package! string-inflection)  ;; Cycle through camelCase -> UPCASE -> underscore
(package! pipenv)  ;; Use and manage Python virtual env from Emacs
(package! easy-escape)  ;; Make Elisp regular expressions more readable
(package! cheatsheet)  ;; Make your own Emacs keybinding cheatsheet
;; (package! delsel)  ;; Delete selected text by typing
;; (package! popup-kill-ring)  ;; Browse through your kill ring (clipboard)
