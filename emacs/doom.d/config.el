;;; config.el --- -*- lexical-binding: t -*-
(setq user-full-name "Justine Kizhakkinedath"
      user-mail-address "justine@kizhak.com")
(defconst *sys/gui*
  (display-graphic-p))
(defconst *sys/win32*
  (eq system-type 'windows-nt))
(defconst *sys/linux*
  (eq system-type 'gnu/linux))
(defconst *sys/mac*
  (eq system-type 'darwin))
(defconst *sys/root*
  (string-equal "root" (getenv "USER")))
(defconst *rg*
  (executable-find "rg"))
(defconst *python*
  (executable-find "python"))
(defconst *python3*
  (executable-find "python3"))
(defconst *tr*
  (executable-find "tr"))
(defconst *mvn*
  (executable-find "mvn"))
(defconst *clangd*
  (or (executable-find "clangd")  ;; usually
      (executable-find "/usr/local/opt/llvm/bin/clangd")))  ;; macOS
(defconst *gcc*
  (executable-find "gcc"))
(defconst *git*
  (executable-find "git"))
(defconst *pdflatex*
  (executable-find "pdflatex"))
(defconst *eaf-env*
  (and *sys/linux* *sys/gui* *python3*
       (executable-find "pip")
       (not (equal (shell-command-to-string "pip freeze | grep '^PyQt\\|PyQtWebEngine'") ""))))
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
(use-package delsel
  :disabled
  :ensure nil
  :config (delete-selection-mode +1))
(setq delete-selection-mode t)
(use-package scroll-bar
  :defer t
  :ensure nil
  :config (scroll-bar-mode -1))
(use-package files
  :defer t
  :config
  (setq confirm-kill-processes nil))
(use-package autorevert
  :defer t
  :ensure nil
  :config
  (global-auto-revert-mode +1)
  (setq auto-revert-interval 2
        auto-revert-check-vc-info t
        auto-revert-verbose nil))
(use-package paren
  :defer t
  :ensure nil
  :init (setq show-paren-delay 0.5)
  :config (show-paren-mode +1))
;; (add-hook 'after-change-major-mode-hook
;;           (lambda ()
;;             (modify-syntax-entry ?_ "w")))
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
(use-package frame
  :ensure t
  :config
  (defun my-settings()
    (add-to-list 'default-frame-alist '(fullscreen . maximized))
    (when (member "BlexMono Nerd Font Mono" (font-family-list))
        (set-frame-font "BlexMono Nerd Font Mono" t t)))
  (if (daemonp)
    (add-hook 'after-make-frame-functions
      (lambda (frame)
        (select-frame frame)
        (my-settings))))
  (my-settings))
(add-hook! '(+doom-dashboard-mode-hook)
           ;; Crypto logo
           (setq fancy-splash-image "~/dotfiles/emacs/doom.d/images/crypto.png"))
(map! "M-s" #'save-buffer)
(map! "M-a" #'mark-whole-buffer)
(map! "M-v" #'counsel-yank-pop)
(map! :leader
      (:prefix ("w")
        "C-w" nil))
(map! :leader
      (:prefix ("w" . "window")
        :desc "Jump to any window using Ace" "M-w" #'ace-window))
(use-package org
  :defer t
  :config
  (setq org-startup-with-inline-images nil)
  (setq org-startup-shrink-all-tables t)
  (setq org-use-property-inheritance t)
  ; Fix `org-cycle' bug
  (map! :map org-mode-map
        :n "<tab>" 'org-cycle))
(use-package toc-org
  :defer 3
  :hook (org-mode . toc-org-mode))
(use-package ox-gfm
  :defer 3)
(use-package ox-reveal
    :defer 3
    :config
    (setq org-reveal-root "/Users/justinkizhakkinedath/revealjs")
    (setq org-reveal-mathjax t))
(setq org-latex-hyperref-template "\\hypersetup{\n pdfauthor={%a},\n pdftitle={%t},\n pdfkeywords={%k},\n pdfsubject={%d},\n pdfcreator={%c}, \n pdflang={%L}, \n colorlinks = true}\n")
(setq org-agenda-files (list "~/org/project/" "~/org/todo.org"))
(use-package projectile
  :config
    (setq  projectile-project-search-path '("~/projects")))
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
;;(setq
;; js-indent-level 2
;; json-reformat:indent-width 2
;; typescript-indent-level 2
;; css-indent-offset 2)
;; (eval-after-load 'web-mode
;;   '(add-hook 'web-mode-hook
;;              (lambda ()
;;                (add-hook 'before-save-hook 'web-beautify-html-buffer t t))))

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
(use-package deadgrep
  :defer 3
  :config
    (map! :leader
      (:prefix ("a" . "applications")
        :desc "Open Ripgrep interface" "r" #'deadgrep)))
(use-package goto-line-preview
  :defer 3
  :config
    (global-set-key [remap goto-line] 'goto-line-preview))
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
(use-package yasnippet
  :defer t
  :diminish yas-minor-mode
  :init
  (use-package yasnippet-snippets :after yasnippet)
  :hook ((prog-mode LaTeX-mode org-mode) . yas-minor-mode)
  :bind
  (:map yas-minor-mode-map ("C-c C-n" . yas-expand-from-trigger-key))
  (:map yas-keymap
        (("TAB" . smarter-yas-expand-next-field)
         ([(tab)] . smarter-yas-expand-next-field)))
  :config
  (yas-reload-all)
  (defun smarter-yas-expand-next-field ()
    "Try to `yas-expand' then `yas-next-field' at current cursor position."
    (interactive)
    (let ((old-point (point))
          (old-tick (buffer-chars-modified-tick)))
      (yas-expand)
      (when (and (eq old-point (point))
                 (eq old-tick (buffer-chars-modified-tick)))
        (ignore-errors (yas-next-field))))))
(use-package treemacs-magit
  :defer t
  :after (treemacs magit))
;; (load "~/projects/apex-legends-quotes/apex-legends-quotes.el")
(use-package apex-legends-quotes
  :config
  ; get random quote from Apex Legends character
  (setq frame-title-format (get-random-apex-legends-quote))
  ; interactive function to change title
  (defun change-emacs-title--apex-legends-quote ()
    (interactive)
    (setq frame-title-format (get-random-apex-legends-quote))))
;; (defun zone-pgm-md5 ()
;;     "MD5 the buffer, then recursively checksum each hash."
;;     (let ((prev-md5 (buffer-substring-no-properties ;; Initialize.
;;                      (point-min) (point-max))))
;;       ;; Whitespace-fill the window.
;;       (zone-fill-out-screen (window-width) (window-height))
;;       (random t)
;;       (goto-char (point-min))
;;       (while (not (input-pending-p))
;;         (when (eobp)
;;           (goto-char (point-min)))
;;         (while (not (eobp))
;;           (delete-region (point) (line-end-position))
;;           (let ((next-md5 (md5 prev-md5)))
;;             (insert next-md5)
;;             (setq prev-md5 next-md5))
;;           (forward-line 1)
;;           (zone-park/sit-for (point-min) 0.1)))))
;; (eval-after-load "zone"
;;   '(unless (memq 'zone-pgm-md5 (append zone-programs nil))
;;      (setq zone-programs
;;            (vconcat zone-programs [zone-pgm-md5]))))
;; (with-eval-after-load 'zone
;; (load "~/dotfiles/emacs/packages/zone-end-of-buffer/zone-end-of-buffer.el")
;; (require 'zone-end-of-buffer)
;;     (unless (memq 'zone-pgm-end-of-buffer (append zone-programs nil))
;;         (setq zone-programs
;;             (vconcat zone-programs [zone-pgm-end-of-buffer]))))
(use-package htmlize
  :defer t)
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
(add-hook 'vterm-mode-hook #'goto-address-mode)
(map! :map vterm-mode-map
      :n "P" #'vterm-yank
      :n "p" #'vterm-yank)
(use-package undo-tree
  :defer t
  ;; :diminish undo-tree-mode
  ;; :init (global-undo-tree-mode)
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-timestamps t))
(use-package flycheck
  :defer t
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-emacs-lisp-load-path 'inherit)
  :config
  (flycheck-add-mode 'javascript-eslint 'js-mode)
  (flycheck-add-mode 'typescript-tslint 'rjsx-mode))
(use-package iedit
  :defer t
  :diminish)
(use-package powerthesaurus
  :defer t)
(map! :leader
      (:prefix ("a" . "applications")
        :desc "Use powerthesaurus to fetch better word" "p" #'powerthesaurus-lookup-word-dwim))
(use-package ace-popup-menu
  :defer t)
(use-package string-inflection
  :defer t)
  ;; :config
  ;; (defun my-string-inflection-cycle-auto ()
  ;;   "switching by major-mode"
  ;;   (interactive)
  ;;   (cond
  ;;    ;; for emacs-lisp-mode
  ;;    ((eq major-mode 'emacs-lisp-mode)
  ;;     (string-inflection-all-cycle))
  ;;    ;; for python
  ;;    ((eq major-mode 'python-mode)
  ;;     (string-inflection-python-style-cycle))
  ;;    ;; for java
  ;;    ((eq major-mode 'java-mode)
  ;;     (string-inflection-java-style-cycle))
  ;;    (t
  ;;     ;; default
  ;;     (string-inflection-ruby-style-cycle)))))
(map! :leader
    (:prefix ("a" . "applications")
        :desc "Cycle through string case using String-inflection" "c" #'string-inflection-all-cycle))
(use-package pipenv
  :defer t)
(use-package easy-escape
  :defer t)
(use-package cheatsheet
  :defer t)
(cheatsheet-add-group 'Magit
                      '(:key "<NormalMode> s" :description "Stage hunk")
                      '(:key "<NormalMode> c c" :description "Create commit")
                      '(:key "<NormalMode> p u" :description "Push to upstream")
                      '(:key "<NormalMode> f u" :description "Fetch from upstream")
                      '(:key "<NormalMode> F u" :description "Pull from upstream"))
(cheatsheet-add-group 'Window-management
                      '(:key "<NormalMode> C-j" :description "Next revision")
                      '(:key "<NormalMode> C-k" :description "Previous revision"))
(use-package easy-escape
  :defer t
  :config
    (set-face-attribute 'easy-escape-face nil :foreground "red"))
(use-package parinfer
  :defer t)
(use-package evil-snipe
  :defer t
  :config
  (setq evil-snipe-scope 'visible)
  (setq evil-snipe-repeat-scope 'buffer)
  (setq evil-snipe-spillover-scope 'whole-buffer))
(use-package indent-tools
  :defer t
  :after (hydra)
  :bind ("C-c >" . #'indent-tools-hydra/body))
;; (map! "C-c >" #'indent-tools-hydra/body)
(use-package hydra
  :defer t)
(use-package python-mode
  :defer t
  :mode "\\.py\\'"
  :custom
  (python-indent-offset 4))
;; (add-hook 'python-mode-hook (lambda ()
;;                                 (set (make-local-variable 'company-backends) '(company-tabnine company-capf company-dabbrev-code company-files))))
;; (add-hook 'python-mode-hook
;;  (lambda () (define-key python-mode-map (kbd "C-c >") 'indent-tools-hydra/body)))
(add-hook 'dart-mode-hook #'lsp-deferred)  ;; Add lsp support to dart
(add-hook 'gfm-mode-hook
          (lambda () (when buffer-file-name
                       (add-hook 'before-save-hook
                                 'markdown-toc-refresh-toc))))
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq-local company-backends '((company-capf company-dabbrev-code company-files)))
            (setq tab-width 2)))

(add-hook 'emacs-lisp-mode-hook 'easy-escape-minor-mode)
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
(setq mac-command-modifier 'meta)
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
(setq browse-url-browser-function 'browse-url-firefox)
(global-auto-revert-mode 1)
