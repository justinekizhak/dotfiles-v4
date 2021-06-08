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
(defconst *python3*
  (executable-find "python3"))
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
(map! "M-a" #'mark-whole-buffer)
(map! "M-s" #'save-buffer)
(map! "M-v" #'counsel-yank-pop)

(setq frame-title-format (shell-command-to-string "apex-voicelines"))

(defun change-emacs-title-apex ()
  "Change your Emacs frame title using the voicelines of `Apex Legends' characters.
This command requires `apex-legends-voicelines' python package."
  (interactive)
  (setq frame-title-format (shell-command-to-string "apex-voicelines")))

;; (add-to-list 'after-init-hook 'clipmon-mode-start)
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
;;
(use-package evil-snipe
  :defer t
  :config
  (setq evil-snipe-scope 'visible)
  (setq evil-snipe-repeat-scope 'buffer)
  (setq evil-snipe-spillover-scope 'whole-buffer))
(use-package goto-line-preview
  :defer t
  :config
    (global-set-key [remap goto-line] 'goto-line-preview))

(use-package htmlize
  :defer t)
(use-package hydra
  :defer t)
(use-package iedit
  :defer t)
(use-package indent-tools
  :defer t
  :after (hydra)
  :bind ("C-c >" . #'indent-tools-hydra/body))
(setq lsp-ui-doc-max-height 30)
(setq lsp-ui-doc-max-width 150)

(setq org-agenda-files (list "~/org/project/" "~/org/todo.org"))

(setq
  org-deadline-warning-days 7
  org-agenda-breadcrumbs-separator " ❱ "
  org-directory "~/org")

(use-package powerthesaurus
  :defer t)
(map! :leader
      (:prefix ("a" . "applications")
        :desc "Use powerthesaurus to fetch better word" "p" #'powerthesaurus-lookup-word-dwim))
(use-package projectile
  :defer t
  :config
    (setq  projectile-project-search-path '("~/projects")))
(use-package deadgrep
  :defer t
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
(setq-default magit-process-password-prompt-regexps
  '("^\\(Enter \\)?[Pp]assphrase\\( for \\(RSA \\)?key '.*'\\)?: ?$"
    ;; Match-group 99 is used to identify the "user@host" part.
    "^\\(Enter \\)?[Pp]assword\\( for '\\(https?://\\)?\\(?99:.*\\)'\\)?: ?$"
    ;; Pinentry Curses box in the terminal when used with GnuPG
    "Please enter the passphrase for the ssh key"
    "^.*'s password: ?$"
    "^Yubikey for .*: ?$"
    "^Enter PIN for .*: ?$"))
(use-package undo-tree
  :defer t
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-timestamps t))
(add-hook 'vterm-mode-hook #'goto-address-mode)
(map! :map vterm-mode-map
      :n "P" #'vterm-yank
      :n "p" #'vterm-yank)
;; Enable backup
(setq make-backup-files t)

;; Backup by copying
(setq backup-by-copying t)
(defun xah-save-all-unsaved ()
  "Save all unsaved files. no ask.
Version 2019-11-05"
  (interactive)
  (save-some-buffers t))

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
