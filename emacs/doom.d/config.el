;;; config.el --- -*- lexical-binding: t -*-
(setq user-full-name "Justine Kizhakkinedath"
      user-mail-address "justine@kizhak.com")
(defconst *sys/linux*
  (eq system-type 'gnu/linux))
(defconst *sys/gui*
  (display-graphic-p))
(defconst *sys/mac*
  (eq system-type 'darwin))
(defconst *sys/win32*
  (eq system-type 'windows-nt))
(defconst *sys/root*
  (string-equal "root" (getenv "USER")))
(defconst *eaf-env*
  (and *sys/linux* *sys/gui* *python3*
       (executable-find "pip")
       (not (equal (shell-command-to-string "pip freeze | grep '^PyQt\\|PyQtWebEngine'") ""))))
(defconst *clangd*
  (or (executable-find "clangd")  ;; usually
      (executable-find "/usr/local/opt/llvm/bin/clangd")))  ;; macOS
(defconst *gcc*
  (executable-find "gcc"))
(defconst *git*
  (executable-find "git"))
(defconst *mvn*
  (executable-find "mvn"))
(defconst *pdflatex*
  (executable-find "pdflatex"))
(defconst *python3*
  (executable-find "python3"))
(defconst *python*
  (executable-find "python"))
(defconst *rg*
  (executable-find "rg"))
(defconst *tr*
  (executable-find "tr"))
(use-package emacs
  :preface
  (defvar ian/indent-width 2) ; change this value to your preferred width
  :config
  (setq
    ring-bell-function 'ignore       ; minimise distraction
    frame-resize-pixelwise t
    default-directory "~/")

  (tool-bar-mode -1)
  (menu-bar-mode -1)

  ;; increase line space for better readability
  (setq-default line-spacing 3)

  ;; Always use spaces for indentation
  (setq-default indent-tabs-mode nil
                tab-width ian/indent-width))
(use-package autorevert
  :defer t
  :ensure nil
  :config
  (global-auto-revert-mode +1)
  (setq auto-revert-interval 2
        auto-revert-check-vc-info t
        auto-revert-verbose nil))
(use-package files
  :defer t
  :config
  (setq confirm-kill-processes nil))
(use-package scroll-bar
  :defer t
  :ensure nil
  :config (scroll-bar-mode -1))
(use-package recentf
  :defer t
  :ensure nil
  :hook (after-init . recentf-mode)
  :custom
  (recentf-auto-cleanup "05:00am")
  (recentf-max-saved-items 200)
  (recentf-exclude '((expand-file-name package-user-dir)
                     ".cache"
                     ".cask"
                     ".elfeed"
                     "bookmarks"
                     "cache"
                     "ido.*"
                     "persp-confs"
                     "recentf"
                     "undo-tree-hist"
                     "url"
                     "COMMIT_EDITMSG\\'")))

;; When buffer is closed, saves the cursor location
(save-place-mode 1)

;; Set history-length longer
(setq-default history-length 500)
(use-package delsel
  :disabled
  :ensure nil
  :config (delete-selection-mode +1))
(setq delete-selection-mode t)
(use-package paren
  :defer t
  :ensure nil
  :init (setq show-paren-delay 0.5)
  :config (show-paren-mode +1))
(add-hook! '(+doom-dashboard-mode-hook)
           ;; Crypto logo
           (setq fancy-splash-image "~/dotfiles/emacs/doom.d/images/crypto.png"))
(use-package frame
  :ensure t
  :config
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))
(map! :leader
      (:prefix ("w")
        "C-w" nil))
(map! :leader
      (:prefix ("w" . "window")
        :desc "Jump to any window using Ace" "M-w" #'ace-window))
(map! "M-a" #'mark-whole-buffer)
(map! "M-s" #'save-buffer)
(map! "M-v" #'counsel-yank-pop)
(use-package ace-popup-menu
  :defer t)
(use-package annotate)
(setq frame-title-format (shell-command-to-string "apex-voicelines"))

(defun change-emacs-title-apex ()
  "Change your Emacs frame title using the voicelines of `Apex Legends' characters.
This command requires `apex-legends-voicelines' python package."
  (interactive)
  (setq frame-title-format (shell-command-to-string "apex-voicelines")))
(use-package atomic-chrome)
(add-hook 'emacs-startup-hook (lambda ()
                                (if (daemonp)
                                    (atomic-chrome-start-server))))
(add-to-list 'after-init-hook 'clipmon-mode-start)
(use-package dired
  :defer t
  :ensure nil
  :bind
  (("C-x C-j" . dired-jump)
   ("C-x j" . dired-jump-other-window))
  :custom
  ;; Always delete and copy recursively
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  ;; Auto refresh Dired, but be quiet about it
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)
  ;; Quickly copy/move file in Dired
  (dired-dwim-target t)
  ;; Move files to trash when deleting
  (delete-by-moving-to-trash t)
  ;; Load the newest version of a file
  (load-prefer-newer t)
  ;; Detect external file changes and auto refresh file
  (auto-revert-use-notify nil)
  (auto-revert-interval 3) ; Auto revert every 3 sec
  :config
  ;; Enable global auto-revert
  (global-auto-revert-mode t)
  ;; Reuse same dired buffer, to prevent numerous buffers while navigating in dired
  (put 'dired-find-alternate-file 'disabled nil)
  :hook
  (dired-mode . (lambda ()
                  (dired-hide-details-mode)
                  (local-set-key (kbd "<mouse-2>") #'dired-find-alternate-file)
                  (local-set-key (kbd "RET") #'dired-find-alternate-file)
                  (local-set-key (kbd "^")
                                 (lambda () (interactive) (find-alternate-file ".."))))))
(map!
    :n "M-k" #'drag-stuff-up    ; drags line up
    :n "M-j" #'drag-stuff-down)  ; drags line down
(with-eval-after-load 'evil-org
  (map!
    :n "M-l" #'evil-org->       ; indents line to left
    :n "M-h" #'evil-org-<))      ; indents line to right
(use-package easy-escape
  :defer t
  :config
    (set-face-attribute 'easy-escape-face nil :foreground "red"))
(use-package evil-snipe
  :defer t
  :config
  (setq evil-snipe-scope 'visible)
  (setq evil-snipe-repeat-scope 'buffer)
  (setq evil-snipe-spillover-scope 'whole-buffer))
(use-package eww
  :defer t
  :ensure nil
  :commands (eww)
  :hook (eww-mode . (lambda ()
                      "Rename EWW's buffer so sites open in new page."
                      (rename-buffer "eww" t)))
  :config
  ;; I am using EAF-Browser instead of EWW
  (unless *eaf-env*
    (setq browse-url-browser-function 'eww-browse-url))) ; Hit & to browse url with system browser
(use-package flycheck
  :defer t
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-emacs-lisp-load-path 'inherit)
  :config
  (flycheck-add-mode 'javascript-eslint 'js-mode)
  (flycheck-add-mode 'typescript-tslint 'rjsx-mode))
(use-package goto-line-preview
  :defer 3
  :config
    (global-set-key [remap goto-line] 'goto-line-preview))
(use-package htmlize
  :defer t)
(use-package hydra
  :defer t)
(use-package iedit
  :defer t
  :diminish)
(use-package indent-tools
  :defer t
  :after (hydra)
  :bind ("C-c >" . #'indent-tools-hydra/body))
(setq lsp-ui-doc-max-height 30)
(setq lsp-ui-doc-max-width 150)
(use-package org
  :defer t
  :config
  (setq org-startup-with-inline-images nil)
  (setq org-startup-shrink-all-tables t)
  (setq org-use-property-inheritance t)
  (setq org-hide-emphasis-markers t)
  ; Fix `org-cycle' bug
  (map! :map org-mode-map
        :n "<tab>" 'org-cycle)
  ; Add plantUML
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (setq org-plantuml-jar-path "~/plantuml.jar")
  (setq plantuml-default-exec-mode 'jar)
  ; Add graphviz
  (add-to-list 'org-src-lang-modes  '("dot" . graphviz-dot))
  (setq org-ellipsis "⬎"))
   ;; ➡, ⚡, ▼, ↴, , ∞, ⬎, ⤷, ⤵
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(let* ((variable-tuple
        (cond ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
              ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
              ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
              ((x-list-fonts "Verdana")         '(:font "Verdana"))
              ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
              (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (headline           `(:inherit default :weight bold)))

  (custom-theme-set-faces
   'user
   `(org-level-8 ((t (,@headline ,@variable-tuple))))
   `(org-level-7 ((t (,@headline ,@variable-tuple))))
   `(org-level-6 ((t (,@headline ,@variable-tuple))))
   `(org-level-5 ((t (,@headline ,@variable-tuple :height 1.1))))
   `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.2))))
   `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.3))))
   `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.4))))
   `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.5))))
   `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))


(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "ETBembo" :height 160))))
 '(fixed-pitch ((t ( :family "Fira Code" :height 160)))))

(add-hook 'org-mode-hook 'variable-pitch-mode)
(add-hook 'org-mode-hook 'visual-line-mode)

(custom-theme-set-faces
 'user
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-link ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))
(setq org-agenda-files (list "~/org/project/" "~/org/todo.org"))

(setq
  org-deadline-warning-days 7
  org-agenda-breadcrumbs-separator " ❱ "
  org-directory "~/org")
(customize-set-value
    'org-agenda-category-icon-alist
    `(
      ("work" "~/.doom.d/icons/money-bag.svg" nil nil :ascent center)
      ("chore" "~/.doom.d/icons/loop.svg" nil nil :ascent center)
      ("events" "~/.doom.d/icons/calendar.svg" nil nil :ascent center)
      ("todo" "~/.doom.d/icons/checklist.svg" nil nil :ascent center)
      ("walk" "~/.doom.d/icons/walk.svg" nil nil :ascent center)
      ("solution" "~/.doom.d/icons/solution.svg" nil nil :ascent center)))
(setq-hook! org-mode
  org-log-done t
  org-columns-default-format "%60ITEM(Task) %20TODO %10Effort(Effort){:} %10CLOCKSUM"
  org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                ("STYLE_ALL" . "habit")))
  org-archive-location "~/org/archive/todo.org.gpg::")

(setq org-agenda-block-separator (string-to-char " "))

(setq org-agenda-custom-commands
      '(("o" "My Agenda"
         ((todo "TODO" ()
                      (org-agenda-overriding-header "\n⚡ Do Today:\n⎺⎺⎺⎺⎺⎺⎺⎺⎺")
                      (org-agenda-remove-tags t)
                      (org-agenda-prefix-format " %-2i %-15b")
                      (org-agenda-todo-keyword-format ""))

          (agenda "" (
                      (org-agenda-start-day "+0d")
                      (org-agenda-span 5)
                      (org-agenda-overriding-header "⚡ Schedule:\n⎺⎺⎺⎺⎺⎺⎺⎺⎺")
                      (org-agenda-repeating-timestamp-show-all nil)
                      (org-agenda-remove-tags t)
                      (org-agenda-prefix-format   "  %-3i  %-15b %t%s")
                      (org-agenda-todo-keyword-format " ☐ ")
                      (org-agenda-current-time-string "⮜┈┈┈┈┈┈┈ now")
                      (org-agenda-scheduled-leaders '("" ""))
                      (org-agenda-time-grid (quote ((daily today remove-match)
                                                    (0900 1200 1500 1800 2100)
                                                    "      " "┈┈┈┈┈┈┈┈┈┈┈┈┈")))))))))
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
(setq org-latex-hyperref-template "\\hypersetup{\n pdfauthor={%a},\n pdftitle={%t},\n pdfkeywords={%k},\n pdfsubject={%d},\n pdfcreator={%c}, \n pdflang={%L}, \n colorlinks = true}\n")
(use-package ox-reveal
    :defer 3
    :config
    (setq org-reveal-root "/Users/justinkizhakkinedath/revealjs")
    (setq org-reveal-mathjax t))
(use-package ox-gfm
  :defer 3)
(use-package org-ref
  :config
  (setq reftex-default-bibliography '("~/org/references.bib"))

  ;; see org-ref for use of these variables
  (setq org-ref-bibliography-notes "~/org/notes.org"
        org-ref-default-bibliography '("~/org/references.bib")
        org-ref-pdf-directory "~/org/bibtex-pdfs/"))

(setq bibtex-completion-bibliography "~/org/references.bib"
      bibtex-completion-library-path "~/org/bibtex-pdfs"
      bibtex-completion-notes-path "~/org/helm-bibtex-notes")

;; open pdf with system pdf viewer (works on mac)
(setq bibtex-completion-pdf-open-function
  (lambda (fpath)
    (start-process "open" "*open*" "open" fpath)))
(use-package parinfer
  :defer t)
(use-package pipenv
  :defer t)
(use-package plantuml-mode
  :defer t)
(use-package powerthesaurus
  :defer t)
(map! :leader
      (:prefix ("a" . "applications")
        :desc "Use powerthesaurus to fetch better word" "p" #'powerthesaurus-lookup-word-dwim))
(use-package projectile
  :config
    (setq  projectile-project-search-path '("~/projects")))
(use-package deadgrep
  :defer 3
  :config
    (map! :leader
      (:prefix ("a" . "applications")
        :desc "Open Ripgrep interface" "r" #'deadgrep)))
(use-package string-inflection
  :defer t)
(map! :leader
    (:prefix ("a" . "applications")
        :desc "Cycle through string case using String-inflection" "c" #'string-inflection-all-cycle))
(use-package treemacs-magit
  :defer t
  :after (treemacs magit))
(use-package undo-tree
  :defer t
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-timestamps t))
(add-hook 'vterm-mode-hook #'goto-address-mode)
(map! :map vterm-mode-map
      :n "P" #'vterm-yank
      :n "p" #'vterm-yank)
(use-package web-mode
  :defer 3
  :custom-face
  (css-selector ((t (:inherit default :foreground "#66CCFF"))))
  (font-lock-comment-face ((t (:foreground "#828282"))))
  :mode
  ("\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'"
   "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.[t]?html?\\'")
  :config
  (setq
   web-mode-markup-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-css-indent-offset 2))
(use-package js2-mode
  :defer 3
  :mode "\\.js\\'"
  :interpreter "node")
(use-package typescript-mode
  :defer 3
  :mode "\\.ts\\'"
  :commands (typescript-mode))
(use-package prettier-js
  :defer 3
  :hook js2-mode)
(use-package emmet-mode
  :defer 3
  :hook ((web-mode . emmet-mode)
         (css-mode . emmet-mode)))
(use-package instant-rename-tag
  :defer 3
  :load-path (lambda () (expand-file-name "~/dotfiles/emacs/packages/instant-rename-tag"))
  :config
  (map! :leader
        (:prefix ("m" . "local leader")
          :desc "Instantly rename opening/closing HTML tag" "o" #'instant-rename-tag)))
(use-package json-mode
  :defer 3
  :mode "\\.json\\'")
(eval-after-load 'css-mode
  '(add-hook 'css-mode-hook
             (lambda ()
               (add-hook 'before-save-hook 'web-beautify-css-buffer t t))))
(eval-after-load 'prettier-js
  '(add-hook 'web-mode-hook
             (lambda ()
               (add-hook 'before-save-hook 'prettier-js-mode))))
(add-hook 'vue-mode-hook #'lsp-deferred)  ;; Add lsp support to dart
(delete '("\\.vue\\'". web-mode) auto-mode-alist)  ;;; Remove web-mode from vue files and then add vue mode to it

(use-package vue-mode
  :defer 1
  :mode "\\.vue\\'")
(with-eval-after-load 'lsp-mode
  (mapc #'lsp-flycheck-add-mode '(typescript-mode js-mode css-mode vue-html-mode)))
(eval-after-load 'prettier-js
  '(add-hook 'vue-mode-hook
             (lambda ()
               (add-hook 'before-save-hook 'prettier-js-mode))))
(add-hook 'dart-mode-hook #'lsp-deferred)  ;; Add lsp support to dart
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq-local company-backends '((company-capf company-dabbrev-code company-files)))
            (setq tab-width 2)))

(add-hook 'emacs-lisp-mode-hook 'easy-escape-minor-mode)
(add-hook 'gfm-mode-hook
          (lambda () (when buffer-file-name
                       (add-hook 'before-save-hook
                                 'markdown-toc-refresh-toc))))
(use-package python-mode
  :defer t
  :mode "\\.py\\'"
  :custom
  (python-indent-offset 4))
(use-package tex
  :disabled
  :ensure auctex
  :defer t
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-master nil)
  ;; to use pdfview with auctex
  (TeX-view-program-selection '((output-pdf "pdf-tools"))
                              TeX-source-correlate-start-server t)
  (TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))
  (TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  :hook
  (LaTeX-mode . (lambda ()
                  (turn-on-reftex)
                  (setq reftex-plug-into-AUCTeX t)
                  (reftex-isearch-minor-mode)
                  (setq TeX-PDF-mode t)
                  (setq TeX-source-correlate-method 'synctex)
                  (setq TeX-source-correlate-start-server t)))
  :config
  (when (version< emacs-version "26")
    (add-hook LaTeX-mode-hook #'display-line-numbers-mode)))
(add-hook 'yaml-mode-hook 'highlight-indent-guides-mode)

(use-package yaml-mode
  :defer t
  :mode ("\\.yaml\\'" "\\.yml\\'")
  :commands (yaml-mode))
;; Enable backup
(setq make-backup-files t)

;; Backup by copying
(setq backup-by-copying t)
(defun xah-save-all-unsaved ()
  "Save all unsaved files. no ask.
Version 2019-11-05"
  (interactive)
  (save-some-buffers t ))

;; when switching out of emacs, all unsaved files will be saved
(add-hook 'focus-out-hook 'xah-save-all-unsaved)
(defhydra hydra-paste (:color red
                       :hint nil)
  "\n[%s(length kill-ring-yank-pointer)/%s(length kill-ring)] \
 [_C-j_/_C-k_] cycles through yanked text, [_p_/_P_] pastes the same text \
 above or below. Anything else exits."
  ("C-j" evil-paste-pop)
  ("C-k" evil-paste-pop-next)
  ("p" evil-paste-after)
  ("P" evil-paste-before))

(map! :nv "p" #'hydra-paste/evil-paste-after
      :nv "P" #'hydra-paste/evil-paste-before)
(global-auto-revert-mode 1)
(setq mac-command-modifier 'meta)
(defun async-shell-command-no-window (command)
  (interactive)
  (let
      ((display-buffer-alist
        (list
         (cons
          "\\*Async Shell Command\\*.*"
          (cons #'display-buffer-no-window nil)))))
    (async-shell-command
     command)))

(defun run-crypto-music (&optional frame)
  (async-shell-command-no-window "/usr/bin/afplay ~/dotfiles/emacs/doom.d/audio/Crypto.wav"))

(add-hook 'after-make-frame-functions 'run-crypto-music)

(add-hook 'emacs-startup-hook (lambda ()
                                (if (not (daemonp))
                                    (run-crypto-music))))
(defun play-audio-file (file-name)
  "Play a audio file. Input audio file."
  (interactive "faudio-file: ")
  (async-shell-command-no-window (concat "/usr/bin/afplay " file-name)))
