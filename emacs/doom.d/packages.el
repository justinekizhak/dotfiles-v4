;;;  -*- lexical-binding: t; -*-
;;;  -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

(package! ob-typescript)
(package! indium)
(package! prettier-js)
(package! org-fancy-priorities)
(package! move-text)
(package! nvm)                                                                                     ;;; Add Support for Node version manager
(package! vue-mode)                                                                                ;;; Add suppport for Vuejs
(package! origami)
(package! org-super-agenda)
(package! deadgrep)
(package! ascii-art-to-unicode)
(package! wgrep)                                                                                   ;;; Add support for project wide search and replace
(package! goto-line-preview)                                                                       ;;; Go to line (M-g g) with preview option
(package! tiny)                                                                                    ;;; Number ranges
(package! ialign)                                                                                  ;;; Aligning content
(package! clipmon)                                                                                 ;;; Clipboard monitor
(package! header3 :recipe (:host github :repo "justinekizhak/header3"))
(package! esup)
(package! treemacs-magit)                                                                          ;;; Magit Treemacs
(package! 2048-game)                                                                               ;;; TP
(package! yasnippet-snippets)                                                                      ;;; Snippets for yas
(package! ox-gfm)                                                                                  ;;; Github Flavored Markdown exporter for Org Mode
(package! paren)                                                                                   ;;; Show matching parentheses
(package! toc-org)                                                                                 ;;; TOC for org mode
(package! ox-reveal)                                                                               ;;; Create beautiful org presentations
(package! htmlize)                                                                                 ;;; Syntax highlighting for codeblocks within org reveal presentations
(package! openapi-yaml-mode :recipe (:host github :repo "magoyette/openapi-yaml-mode"))
(package! highlight-indent-guides)                                                                 ;;; Highlight indentation
(package! ob-restclient)                                                                           ;;; Use restclient within Org file
(package! iedit)                                                                                   ;;; Edit all occurance of symbol simultanously
(package! powerthesaurus)                                                                          ;;; Thesaurus for Emacs
(package! ace-popup-menu)                                                                          ;;; Use better popup menu for Emacs
(package! string-inflection)                                                                       ;;; Cycle through camelCase -> UPCASE -> underscore
(package! pipenv)                                                                                  ;;; Use and manage Python virtual env from Emacs
(package! easy-escape)                                                                             ;;; Make Elisp regular expressions more readable
(package! web-beautify)                                                                            ;;; Beautify your Web development
;; (package! apex-legends-quotes :recipe (:host gitlab :repo "justinekizhak/apex-legends-quotes")) ;;; Use `apex-legends-voiceline' python package instead
