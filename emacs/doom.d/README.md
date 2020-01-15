# About Emacs

Emacs changes how you _think_ about programming.

Emacs is **totally introspectable**. You can always find out &rsquo;what code runs when I press this button?&rsquo;.

Emacs is an **incremental programming environment**. There&rsquo;s no edit-compile-run cycle. There isn&rsquo;t even an edit-run cycle. You can execute snippets of code and gradually turn them into a finished project. There&rsquo;s no distinction between your editor and your interpreter.

Emacs is a **mutable environment**. You can set variables, tweak functions with advice, or redefine entire functions. Nothing is off-limits.

Emacs provides **functionality without applications**. Rather than separate applications, functionality is all integrated into your Emacs instance. Amazingly, this works. Ever wanted to use the same snippet tool for writing C++ classes as well as emails?

Emacs is full of **incredible software concepts that haven&rsquo;t hit the mainstream yet**. For example:

- Many platforms have a single item clipboard. Emacs has an **infinite clipboard**.
- If you undo a change, and then continue editing, you can&rsquo;t redo the original change. Emacs allows **undoing to any historical state**, even allowing tree-based exploration of history.
- Emacs supports a **reverse variable search**: you can find variables with a given value.
- You can perform **structural editing** of code, allowing you to make changes without breaking syntax. This works for lisps (paredit) and non-lisps (smartparens).
- Many applications use a modal GUI: for example, you can&rsquo;t do other edits during a find-and-replace operation. Emacs provides **recursive editing** that allow you to suspend what you&rsquo;re currently doing, perform other edits, then continue the original task.

Emacs has a **documentation culture**. Emacs includes a usage manual, a lisp programming manual, pervasive docstrings and even an interactive tutorial.

Emacs has a **broad ecosystem**. If you want to edit code in a niche language, there&rsquo;s probably an Emacs package for it.

Emacs doesn&rsquo;t have a monopoly on good ideas, and there are other great tools out there. Nonetheless, we believe the [Emacs learning curve](https://i.stack.imgur.com/7Cu9Z.jpg) pays off.

_This beautifully written **About EMACS** section credits to [Remacs](https://github.com/remacs/remacs)._

Also if you want to read more about Emacs, checkout [Why You Should Buy Into the Emacs Platform](https://two-wrongs.com/why-you-should-buy-into-the-emacs-platform).

# About my config

This Emacs config is a work of many hours of banging my head on a wall. My Emacs config has gone through many phase. This phase seems to the most pleasent one.

## Screenshot

![img](images/screenshot.png)

## Installation

Use the install directions from [Doom Emacs](https://github.com/hlissner/doom-emacs).

## Modification

You have the permission to use, modify, distribute in any way you want.

However, what is _free_ stays _free_. After all, this is [GPL](LICENSE).

## Contribution

If you spotted a bug or you have any suggestions, please fill in an issue. If you have something to fix, feel free to create a pull request.

## Special Thanks

Everyone starts somewhere, and I started here.

[MatthewZMD](https://github.com/MatthewZMD/.emacs.d)

[Henrik Lissner&rsquo;s Doom Emacs](https://github.com/hlissner/doom-emacs)

# Lexical Binding

Use lexical-binding. [Why?](https://nullprogram.com/blog/2016/12/22/)

> Until Emacs 24.1 (June 2012), Elisp only had dynamically scoped variables, a feature, mostly by accident, common to old lisp dialects. While dynamic scope has some selective uses, it’s widely regarded as a mistake for local variables, and virtually no other languages have adopted it.

```emacs-lisp
;;; config.el --- -*- lexical-binding: t -*-
```

# Personal Information

Let&rsquo;s set some variables with basic user information.

```emacs-lisp
(setq user-full-name "Justine Kizhakkinedath"
      user-mail-address "justine@kizhak.com")
```

# Improve boot up speed

## Omit default startup screen

```emacs-lisp
(use-package "startup"
  :ensure nil
  :config (setq inhibit-startup-screen t))
```

## Disable `package-enable-at-startup`

Package initialize occurs automatically, before `user-init-file` is loaded, but after `early-init-file`. We handle package initialization, so we must prevent Emacs from doing it early!

```emacs-lisp
(setq package-enable-at-startup nil)
```

## Unset `file-name-handler-alist`

Every file opened and loaded by Emacs will run through this list to check for a proper handler for the file, but during startup, it won’t need any of them.

```emacs-lisp
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)
```

## Disable `site-run-file`

```emacs-lisp
(setq site-run-file nil)
```

## Disable Unnecessary Interface

It will be faster to disable them here before they&rsquo;ve been initialized.

```emacs-lisp
(menu-bar-mode -1)
(unless (and (display-graphic-p) (eq system-type 'darwin))
  (push '(menu-bar-lines . 0) default-frame-alist))
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
```

## Setting up garbage collection for speedy startup

We&rsquo;re going to increase the gc-cons-threshold to a very high number to decrease the load and compile time. We&rsquo;ll lower this value significantly after initialization has completed. We don&rsquo;t want to keep this value too high or it will result in long GC pauses during normal usage.

```emacs-lisp
(eval-and-compile
  (setq gc-cons-threshold 402653184
        gc-cons-percentage 0.6))
```

### Better garbage threshold limit

```emacs-lisp
(defvar better-gc-cons-threshold 67108864 ; 64mb
  "The default value to use for `gc-cons-threshold'.

If you experience freezing, decrease this.  If you experience stuttering, increase this.")

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold better-gc-cons-threshold)
            (setq file-name-handler-alist file-name-handler-alist-original)
            (makunbound 'file-name-handler-alist-original)))
```

### Garbage collect when Emacs is out of focus

```emacs-lisp
(add-hook 'emacs-startup-hook
          (lambda ()
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state)
                                  (garbage-collect))))
              (add-hook 'after-focus-change-function 'garbage-collect))
            ;; Avoid garbage collection when using minibuffer
                (defun gc-minibuffer-setup-hook ()
                (setq gc-cons-threshold (* better-gc-cons-threshold 2)))

                (defun gc-minibuffer-exit-hook ()
                (garbage-collect)
                (setq gc-cons-threshold better-gc-cons-threshold))

                (add-hook 'minibuffer-setup-hook #'gc-minibuffer-setup-hook)
                (add-hook 'minibuffer-exit-hook #'gc-minibuffer-exit-hook)))
```

# Use-Package Settings

Tell `use-package` to always defer loading packages unless explicitly told otherwise. This speeds up initialization significantly as many packages are only loaded later when they are explicitly used.

```emacs-lisp
(with-eval-after-load 'use-package
  (setq use-package-always-defer t
        use-package-verbose t
        use-package-expand-minimally t
        use-package-compute-statistics t
        use-package-enable-imenu-support t))
```

# Defining constants

## Are we running on a GUI Emacs?

```emacs-lisp
(defconst *sys/gui*
  (display-graphic-p))
```

## Are we running on a WinTel system?

```emacs-lisp
(defconst *sys/win32*
  (eq system-type 'windows-nt))
```

## Are we running on a GNU/Linux system?

```emacs-lisp
(defconst *sys/linux*
  (eq system-type 'gnu/linux))
```

## Are we running on a Mac system?

```emacs-lisp
(defconst *sys/mac*
  (eq system-type 'darwin))
```

## Are you a ROOT user?

```emacs-lisp
(defconst *sys/root*
  (string-equal "root" (getenv "USER")))
```

## Do we have ripgrep?

```emacs-lisp
(defconst *rg*
  (executable-find "rg"))
```

## Do we have python?

```emacs-lisp
(defconst *python*
  (executable-find "python"))
```

## Do we have python3?

```emacs-lisp
(defconst *python3*
  (executable-find "python3"))
```

## Do we have tr?

```emacs-lisp
(defconst *tr*
  (executable-find "tr"))
```

## Do we have Maven?

```emacs-lisp
(defconst *mvn*
  (executable-find "mvn"))
```

## Do we have clangd?

```emacs-lisp
(defconst *clangd*
  (or (executable-find "clangd")  ;; usually
      (executable-find "/usr/local/opt/llvm/bin/clangd")))  ;; macOS
```

## Do we have gcc?

```emacs-lisp
(defconst *gcc*
  (executable-find "gcc"))
```

## Do we have git?

```emacs-lisp
(defconst *git*
  (executable-find "git"))
```

## Do we have pdflatex?

```emacs-lisp
(defconst *pdflatex*
  (executable-find "pdflatex"))
```

## Check basic requirements for EAF to run.

```emacs-lisp
(defconst *eaf-env*
  (and *sys/linux* *sys/gui* *python3*
       (executable-find "pip")
       (not (equal (shell-command-to-string "pip freeze | grep '^PyQt\\|PyQtWebEngine'") ""))))
```

# Some Emacs defaults

```emacs-lisp
  (use-package emacs
    :preface
    (defvar ian/indent-width 4) ; change this value to your preferred width
    :config
    (setq
     ring-bell-function 'ignore       ; minimise distraction
     frame-resize-pixelwise t
     default-directory "~/")

    (tool-bar-mode -1)
    (menu-bar-mode -1)

    ;; better scrolling experience
    (setq scroll-margin 0
          scroll-conservatively 10000
          scroll-preserve-screen-position t
          auto-window-vscroll nil)

    ;; increase line space for better readability
    (setq-default line-spacing 3)

    ;; Always use spaces for indentation
    (setq-default indent-tabs-mode nil
                  tab-width ian/indent-width))
```

# Better editing experience

## Modernize selection behavior

Replace the active region just by typing text, just like modern editors

```emacs-lisp
(use-package delsel
  :disabled
  :ensure nil
  :config (delete-selection-mode +1))
```

```emacs-lisp
(setq delete-selection-mode t)
```

## Disable scroll bar

```emacs-lisp
(use-package scroll-bar
  :ensure nil
  :config (scroll-bar-mode -1))
```

## Confirm kill process

Don’t bother confirming killing processes

```emacs-lisp
(use-package files
  :defer t
  :config
  (setq confirm-kill-processes nil))
```

## Automatically refreshes the buffer for changes outside of Emacs

Auto refreshes every 2 seconds. Don’t forget to refresh the version control status as well.

```emacs-lisp
(use-package autorevert
  :ensure nil
  :config
  (global-auto-revert-mode +1)
  (setq auto-revert-interval 2
        auto-revert-check-vc-info t
        auto-revert-verbose nil))
```

## Mouse wheel (track-pad) scroll speed

By default, the scrolling is way too fast to be precise and helpful, let’s tune it down a little bit.

```emacs-lisp
(use-package mwheel
  :ensure nil
  :config (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
                mouse-wheel-progressive-speed nil))
```

## Show matching parentheses

Reduce the highlight delay to instantly.

```emacs-lisp
(use-package paren
  :ensure nil
  :init (setq show-paren-delay 0)
  :config (show-paren-mode +1))
```

## Treat underscore as part of the word

```emacs-lisp
;; (add-hook 'after-change-major-mode-hook
;;           (lambda ()
;;             (modify-syntax-entry ?_ "w")))
```

## History

```emacs-lisp
(use-package recentf
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
```

# Appearance

## Setting up some frame defaults

Maximize the frame by default on start-up. Set the font to Fira code, if Fira code is installed.

```emacs-lisp
(use-package frame
  :ensure nil
  :config
  (setq initial-frame-alist (quote ((fullscreen . maximized))))
  ;; (add-to-list 'default-frame-alist
  ;;              '(ns-transparent-titlebar . t))
  ;; (add-to-list 'default-frame-alist
  ;;              '(ns-appearance . dark))
  (when (member "Fira Code" (font-family-list))
    (set-frame-font "Fira Code" t t)))
```

## Dashboard with images

Use the image in the dotfiles folder as the dashboard splash image

```emacs-lisp
(add-hook! '(+doom-dashboard-mode-hook)
           ;; Crypto logo
           (setq fancy-splash-image "~/dotfiles/emacs/doom.d/images/crypto.png"))
```

# Custom Keybinding

## ⌘ + s → Save file

```emacs-lisp
(map! "M-s" #'save-buffer)
```

## ⌘ + a → Select all

```emacs-lisp
(map! "M-a" #'mark-whole-buffer)
```

## ⌘ + v → Paste menu

```emacs-lisp
(map! "M-v" #'counsel-yank-pop)
```

# Packages

## Org mode

Don&rsquo;t display images in a org file which has images. To show image `M-x` `org-toggle-inline-images` OR use keybinding `z i`

```emacs-lisp
(use-package org
  :defer t
  :config
    (setq org-startup-with-inline-images nil))
```

### Org-toc

```emacs-lisp
(use-package toc-org
  :defer 3
  :hook (org-mode . toc-org-mode))
```

### Ox-gfm

Github Flavored Markdown exporter for Org Mode

```emacs-lisp
(use-package ox-gfm
  :defer 3)
```

### Org Reveal

```emacs-lisp
(use-package ox-reveal
    :defer 3
    :config
    (setq org-reveal-root "/Users/justinkizhakkinedath/revealjs")
    (setq org-reveal-mathjax t))
```

## Projectile

```emacs-lisp
(use-package projectile
  :config
    (setq  projectile-project-search-path '("~/projects")))
```

## Web development

### Web mode

Web mode, a major mode for editing web templates.

```emacs-lisp
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
```

### JavaScript/TypeScript

1.  JavaScript2 Mode

    JS2 mode, a feature that offers improved JavsScript editing mode.

    ```emacs-lisp
    (use-package js2-mode
      :defer 3
      :mode "\\.js\\'"
      :interpreter "node")
    ```

2.  TypeScript Mode

    TypeScript mode, a feature that offers TypeScript support for Emacs.

    ```emacs-lisp
    (use-package typescript-mode
      :defer 3
      :mode "\\.ts\\'"
      :commands (typescript-mode))
    ```

### Prettier

```emacs-lisp
(use-package prettier-js
  :defer 3
  :hook js2-mode
  :config
    (setq prettier-js-args '("--single-quote")))
```

### Emmet

Emmet, a feature that allows writing HTML using CSS selectors along with C-j. See usage for more information.

```emacs-lisp
(use-package emmet-mode
  :defer 3
  :hook ((web-mode . emmet-mode)
         (css-mode . emmet-mode)))
```

### Instant Rename Tag

Instant Rename Tag, a plugin that provides ability to rename html tag pairs instantly.

```emacs-lisp
(use-package instant-rename-tag
  :defer 3
  :load-path (lambda () (expand-file-name "~/dotfiles/emacs/packages/instant-rename-tag"))
  :config
  (map! :leader
        (:prefix-map ("m" . "local leader")
          :desc "Instantly rename opening/closing HTML tag" "o" #'instant-rename-tag)))
```

### JSON

JSON Mode, a major mode for editing JSON files.

```emacs-lisp
(use-package json-mode
  :defer 3
  :mode "\\.json\\'")
```

```emacs-lisp
;;(setq
;; js-indent-level 2
;; json-reformat:indent-width 2
;; typescript-indent-level 2
;; css-indent-offset 2)
```

## Ripgrep

```emacs-lisp
(use-package deadgrep
  :defer 3
  :config
    (map! :leader
      (:prefix-map ("a" . "applications")
        :desc "Open Ripgrep interface" "r" #'deadgrep)))
```

## Browse kill ring

```emacs-lisp
(use-package browse-kill-ring
  :disabled
  :defer 3
  :config
    (map! :map browse-kill-ring-mode-map
        "j" #'browse-kill-ring-forward
        "k" #'browse-kill-ring-previous
        "/" #'browse-kill-ring-search-forward
        "?" #'browse-kill-ring-search-backward
        "N" #'(lambda ()
                (interactive)
                (browse-kill-ring-search-backward "")))
    (map! "M-v" #'browse-kill-ring))
```

## Magit

## Go to line preview

```emacs-lisp
(use-package goto-line-preview
  :defer 3
  :config
    (global-set-key [remap goto-line] 'goto-line-preview))
```

## Clipmon

```emacs-lisp
(add-to-list 'after-init-hook 'clipmon-mode-start)
```

## Company

```emacs-lisp
(use-package company
  :defer t
  :diminish company-mode
  :hook ((prog-mode LaTeX-mode latex-mode ess-r-mode) . company-mode)
  :bind
  (:map company-active-map
        ([tab] . smarter-yas-expand-next-field-complete)
        ("TAB" . smarter-yas-expand-next-field-complete))
  :custom
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations t)
  (company-begin-commands '(self-insert-command))
  (company-require-match 'never)
  ;; Don't use company in the following modes
  (company-global-modes '(not shell-mode eaf-mode))
  ;; Trigger completion immediately.
  (company-idle-delay 0.1)
  ;; Number the candidates (use M-1, M-2 etc to select completions).
  (company-show-numbers t)
  :config
  (unless *clangd* (delete 'company-clang company-backends))
  (global-company-mode 1)
  (defun smarter-yas-expand-next-field-complete ()
    "Try to `yas-expand' and `yas-next-field' at current cursor position.

If failed try to complete the common part with `company-complete-common'"
    (interactive)
    (if yas-minor-mode
        (let ((old-point (point))
              (old-tick (buffer-chars-modified-tick)))
          (yas-expand)
          (when (and (eq old-point (point))
                     (eq old-tick (buffer-chars-modified-tick)))
            (ignore-errors (yas-next-field))
            (when (and (eq old-point (point))
                       (eq old-tick (buffer-chars-modified-tick)))
              (company-complete-common))))
      (company-complete-common))))
```

Setting up keybindings for completion selection

```emacs-lisp
;; (with-eval-after-load 'company
;;   (define-key company-active-map (kbd "<return>") nil)
;;   (define-key company-active-map (kbd "RET") nil)
;;   (define-key company-active-map (kbd "C-SPC") #'company-complete-selection))
```

### Company-lsp

```emacs-lisp
(use-package company-lsp
  :defer t
  :custom (company-lsp-cache-candidates 'auto))
```

### Commpany-box

```emacs-lisp
;; (use-package company-box
;;   :defer t
;;   :diminish
;;   :functions (my-company-box--make-line
;;               my-company-box-icons--elisp)
;;   :commands (company-box--get-color
;;              company-box--resolve-colors
;;              company-box--add-icon
;;              company-box--apply-color
;;              company-box--make-line
;;              company-box-icons--elisp)
;;   :hook (company-mode . company-box-mode)
;;   :custom
;;   (company-box-backends-colors nil)
;;   (company-box-show-single-candidate t)
;;   (company-box-max-candidates 50)
;;   (company-box-doc-delay 0.3)
;;   :config
;;   ;; Support `company-common'
;;   (defun my-company-box--make-line (candidate)
;;     (-let* (((candidate annotation len-c len-a backend) candidate)
;;             (color (company-box--get-color backend))
;;             ((c-color a-color i-color s-color) (company-box--resolve-colors color))
;;             (icon-string (and company-box--with-icons-p (company-box--add-icon candidate)))
;;             (candidate-string (concat (propertize (or company-common "") 'face 'company-tooltip-common)
;;                                       (substring (propertize candidate 'face 'company-box-candidate) (length company-common) nil)))
;;             (align-string (when annotation
;;                             (concat " " (and company-tooltip-align-annotations
;;                                              (propertize " " 'display `(space :align-to (- right-fringe ,(or len-a 0) 1)))))))
;;             (space company-box--space)
;;             (icon-p company-box-enable-icon)
;;             (annotation-string (and annotation (propertize annotation 'face 'company-box-annotation)))
;;             (line (concat (unless (or (and (= space 2) icon-p) (= space 0))
;;                             (propertize " " 'display `(space :width ,(if (or (= space 1) (not icon-p)) 1 0.75))))
;;                           (company-box--apply-color icon-string i-color)
;;                           (company-box--apply-color candidate-string c-color)
;;                           align-string
;;                           (company-box--apply-color annotation-string a-color)))
;;             (len (length line)))
;;       (add-text-properties 0 len (list 'company-box--len (+ len-c len-a)
;;                                        'company-box--color s-color)
;;                            line)
;;       line))
;;   (advice-add #'company-box--make-line :override #'my-company-box--make-line)

;;   ;; Prettify icons
;;   (defun my-company-box-icons--elisp (candidate)
;;     (when (derived-mode-p 'emacs-lisp-mode)
;;       (let ((sym (intern candidate)))
;;         (cond ((fboundp sym) 'Function)
;;               ((featurep sym) 'Module)
;;               ((facep sym) 'Color)
;;               ((boundp sym) 'Variable)
;;               ((symbolp sym) 'Text)
;;               (t . nil)))))
;;   (advice-add #'company-box-icons--elisp :override #'my-company-box-icons--elisp)

;;   (when (and *sys/gui*
;;              (require 'all-the-icons nil t))
;;     (declare-function all-the-icons-faicon 'all-the-icons)
;;     (declare-function all-the-icons-material 'all-the-icons)
;;     (declare-function all-the-icons-octicon 'all-the-icons)
;;     (setq company-box-icons-all-the-icons
;;           `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.85 :v-adjust -0.2))
;;             (Text . ,(all-the-icons-faicon "text-width" :height 0.8 :v-adjust -0.05))
;;             (Method . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.05 :face 'all-the-icons-purple))
;;             (Function . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.05 :face 'all-the-icons-purple))
;;             (Constructor . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.05 :face 'all-the-icons-purple))
;;             (Field . ,(all-the-icons-octicon "tag" :height 0.8 :v-adjust 0 :face 'all-the-icons-lblue))
;;             (Variable . ,(all-the-icons-octicon "tag" :height 0.8 :v-adjust 0 :face 'all-the-icons-lblue))
;;             (Class . ,(all-the-icons-material "settings_input_component" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-orange))
;;             (Interface . ,(all-the-icons-material "share" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
;;             (Module . ,(all-the-icons-material "view_module" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
;;             (Property . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.05))
;;             (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.85 :v-adjust -0.2))
;;             (Value . ,(all-the-icons-material "format_align_right" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
;;             (Enum . ,(all-the-icons-material "storage" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-orange))
;;             (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.85 :v-adjust -0.2))
;;             (Snippet . ,(all-the-icons-material "format_align_center" :height 0.85 :v-adjust -0.2))
;;             (Color . ,(all-the-icons-material "palette" :height 0.85 :v-adjust -0.2))
;;             (File . ,(all-the-icons-faicon "file-o" :height 0.85 :v-adjust -0.05))
;;             (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.85 :v-adjust -0.2))
;;             (Folder . ,(all-the-icons-faicon "folder-open" :height 0.85 :v-adjust -0.05))
;;             (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
;;             (Constant . ,(all-the-icons-faicon "square-o" :height 0.85 :v-adjust -0.05))
;;             (Struct . ,(all-the-icons-material "settings_input_component" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-orange))
;;             (Event . ,(all-the-icons-faicon "bolt" :height 0.8 :v-adjust -0.05 :face 'all-the-icons-orange))
;;             (Operator . ,(all-the-icons-material "control_point" :height 0.85 :v-adjust -0.2))
;;             (TypeParameter . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.05))
;;             (Template . ,(all-the-icons-material "format_align_center" :height 0.85 :v-adjust -0.2)))
;;           company-box-icons-alist 'company-box-icons-all-the-icons)))
```

### Company-tabnine

[Company TabNine](https://github.com/TommyX12/company-tabnine), A company-mode backend for [TabNine](https://tabnine.com/), the all-language autocompleter.

This is enabled by default, if ever you find it not good enough for a particular completion, simply use `M-q` to immediately switch to default backends.

**Prerequisite**: Execute `M-x company-tabnine-install-binary` to install the TabNine binary for your system.

```emacs-lisp
(use-package company-tabnine
  :disabled
  :defer 1
  :custom
  (company-tabnine-max-num-results 9)
  :hook
  (lsp-after-open . (lambda ()
                      (setq company-tabnine-max-num-results 3)
                      (add-to-list 'company-transformers 'company//sort-by-tabnine t)
                      (add-to-list 'company-backends '(company-lsp :with company-tabnine :separate))))
  (kill-emacs . company-tabnine-kill-process)
  :config
  ;; Enable TabNine on default
  (add-to-list 'company-backends #'company-tabnine)

  (map! :leader
        (:prefix-map ("a" . "applications")
          :desc "Use company default backend" "o" #'company-other-backend
          :desc "Use company tabnine backend" "t" #'company-tabnine))

  ;; Integrate company-tabnine with lsp-mode
  (defun company//sort-by-tabnine (candidates)
    (if (or (functionp company-backend)
            (not (and (listp company-backend) (memq 'company-tabnine company-backend))))
        candidates
      (let ((candidates-table (make-hash-table :test #'equal))
            candidates-lsp
            candidates-tabnine)
        (dolist (candidate candidates)
          (if (eq (get-text-property 0 'company-backend candidate)
                  'company-tabnine)
              (unless (gethash candidate candidates-table)
                (push candidate candidates-tabnine))
            (push candidate candidates-lsp)
            (puthash candidate t candidates-table)))
        (setq candidates-lsp (nreverse candidates-lsp))
        (setq candidates-tabnine (nreverse candidates-tabnine))
        (nconc (seq-take candidates-tabnine 3)
               (seq-take candidates-lsp 6))))))
```

## Dired

```emacs-lisp
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
                  (local-set-key (kbd "<mouse-2>") #'dired-find-alternate-file)
                  (local-set-key (kbd "RET") #'dired-find-alternate-file)
                  (local-set-key (kbd "^")
                                 (lambda () (interactive) (find-alternate-file ".."))))))
```

## Drag lines

### Vertically

```emacs-lisp
(map!
    :n "M-k" #'drag-stuff-up    ; drags line up
    :n "M-j" #'drag-stuff-down)  ; drags line down
```

### Horizontally

```emacs-lisp
(with-eval-after-load 'evil-org
  (map!
    :n "M-l" #'evil-org->       ; indents line to left
    :n "M-h" #'evil-org-<))      ; indents line to right
```

## Yasnippet

```emacs-lisp
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
```

## Treemacs magit

```emacs-lisp
(use-package treemacs-magit
  :defer t
  :after (treemacs magit))
```

## 2048 game

```emacs-lisp
(use-package 2048-game
  :defer t
  :commands (2048-game))
```

## Lentic mode

## Apex Legends quotes

Use a random quote of a character from [Apex Legends](https://www.ea.com/games/apex-legends/play-now-for-free) as your frame title.

```emacs-lisp
;; (load "~/projects/apex-legends-quotes/apex-legends-quotes.el")
(use-package apex-legends-quotes
  :config
  ; get random quote from Apex Legends character
  (setq frame-title-format (get-random-apex-legends-quote))
  ; interactive function to change title
  (defun change-emacs-title--apex-legends-quote ()
    (interactive)
    (setq frame-title-format (get-random-apex-legends-quote))))
```

## Zone

```emacs-lisp
(use-package zone
  :ensure nil
  :defer 5
  :config
  (zone-when-idle 30) ; in seconds
  (defun zone-choose (pgm)
    "Choose a PGM to run for `zone'."
    (interactive
     (list
      (completing-read
       "Program: "
       (mapcar 'symbol-name zone-programs))))
    (let ((zone-programs (list (intern pgm))))
      (zone))))
```

### Zone md5

```emacs-lisp
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
```

```emacs-lisp
  ;; (eval-after-load "zone"
  ;;   '(unless (memq 'zone-pgm-md5 (append zone-programs nil))
  ;;      (setq zone-programs
  ;;            (vconcat zone-programs [zone-pgm-md5]))))
```

### Zone end of buffer

```emacs-lisp
;; (with-eval-after-load 'zone
;; (load "~/dotfiles/emacs/packages/zone-end-of-buffer/zone-end-of-buffer.el")
;; (require 'zone-end-of-buffer)
;;     (unless (memq 'zone-pgm-end-of-buffer (append zone-programs nil))
;;         (setq zone-programs
;;             (vconcat zone-programs [zone-pgm-end-of-buffer]))))
```

## God mode

```emacs-lisp
;; (global-set-key (kbd "`-<escape>") 'god-local-mode)
;; (global-set-key (kbd "<escape>") 'god-mode-all)

;; (map! "S-<escape>" #'god-mode-all)
```

### Cursor style to indicate mode

You can change the cursor style indicate whether you&rsquo;re in God mode or not.

```emacs-lisp
;; (defun my-update-cursor ()
;;   (setq cursor-type (if (or god-local-mode buffer-read-only)
;;                         'box
;;                       'bar)))

;; (add-hook 'god-mode-enabled-hook 'my-update-cursor)
;; (add-hook 'god-mode-disabled-hook 'my-update-cursor)
```

### Change modeline color

You can use the following function to switch the entire modeline&rsquo;s foreground and background:

```emacs-lisp
;; (defun c/god-mode-update-cursor ()
;;   (let ((limited-colors-p (> 257 (length (defined-colors)))))
;;     (cond (god-local-mode (progn
;;                             (set-face-background 'mode-line (if limited-colors-p "white" "#e9e2cb"))
;;                             (set-face-background 'mode-line-inactive (if limited-colors-p "white" "#e9e2cb"))))
;;           (t (progn
;;                (set-face-background 'mode-line (if limited-colors-p "black" "#0a2832"))
;;                (set-face-background 'mode-line-inactive (if limited-colors-p "black" "#0a2832")))))))
```

## HTMLize

HTMLize, a tool that converts buffer text and decorations to HTML

```emacs-lisp
(use-package htmlize
  :defer t)
```

## EWW

Emacs Web Wowser, the HTML-based Emacs Web Browser.

```emacs-lisp
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
```

## VTerm

Add clickable links inside terminal

```emacs-lisp
(add-hook 'vterm-mode-hook #'goto-address-mode)
```

## Restclient

```emacs-lisp
(use-package restclient
  :defer t
  :config
    (org-babel-do-load-languages
      'org-babel-load-languages
      '((restclient . t))))
```

## Popup kill ring

Popup Kill Ring, a feature that provides the ability to browse Emacs kill ring in autocomplete style popup menu.

```emacs-lisp
(use-package popup-kill-ring
  :disabled
  :defer t
  :bind ("M-y" . popup-kill-ring))
```

## Undo tree

Undo tree, a feature that provides a visualization of the undos in a file.

```emacs-lisp
(use-package undo-tree
  :defer t
  :diminish undo-tree-mode
  :init (global-undo-tree-mode)
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-timestamps t))
```

```emacs-lisp
(map! :leader
    (:prefix-map ("a" . "applications")
        :desc "Open undo tree visualizer" "u" #'undo-tree-visualize))
```

## Discover My Major

Discover my major, a feature that discovers key bindings and their meaning for the current Emacs major mode.

```emacs-lisp
(use-package discover-my-major
  :defer 1
  :config
  (map! :leader (:prefix "h"
                    :desc "Open discover-my-major" "C-m" #'discover-my-major)))
```

## Flycheck

Flycheck, a syntax checking extension.

```emacs-lisp
(use-package flycheck
  :defer t
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-emacs-lisp-load-path 'inherit)
  :config
  (flycheck-add-mode 'javascript-eslint 'js-mode)
  (flycheck-add-mode 'typescript-tslint 'rjsx-mode))
```

## Hightlight indentation guide

```emacs-lisp
(use-package highlight-indent-guides
  :defer t
  :if *sys/gui*
  :diminish
  :hook ((prog-mode web-mode nxml-mode) . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'top)
  (highlight-indent-guides-delay 0)
  (highlight-indent-guides-auto-character-face-perc 7))
```

Indentation config

```emacs-lisp
(setq-default indent-tabs-mode nil)
(setq-default indent-line-function 'insert-tab)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default js-switch-indent-offset 4)
(c-set-offset 'comment-intro 0)
(c-set-offset 'innamespace 0)
(c-set-offset 'case-label '+)
(c-set-offset 'access-label 0)
(c-set-offset (quote cpp-macro) 0 nil)
(add-hook 'after-change-major-mode-hook
          (lambda () (if (equal electric-indent-mode 't)
                         (when (derived-mode-p 'text-mode)
                           (electric-indent-mode -1))
                       (electric-indent-mode 1))))
```

## Iedit

Iedit, a minor mode that allows editing multiple regions simultaneousy in a buffer or a region.

```emacs-lisp
(use-package iedit
  :defer t
  :diminish)
```

## Powerthesaurus

```emacs-lisp
(use-package powerthesaurus
  :defer t)
```

## Ace-popup

```emacs-lisp
(use-package ace-popup-menu
  :defer t)
```

## String-inflection

```emacs-lisp
(use-package string-inflection
  :defer t)
```

## Pipenv

```emacs-lisp
(use-package pipenv
  :defer t)
```

## Easy escape

```emacs-lisp
(use-package easy-escape
  :defer t)
```

## Cheatsheet

```emacs-lisp
(use-package cheatsheet
  :defer t)
```

## Easy escape

Now no more double backslash hell. [Github repo](https://github.com/cpitclaudel/easy-escape)

![img](https://raw.githubusercontent.com/cpitclaudel/easy-escape/master/img/easy-escape.png)

```emacs-lisp
(use-package easy-escape
  :defer t
  :config
    (set-face-attribute 'easy-escape-face nil :foreground "red"))
```

## Parinfer

```emacs-lisp
(use-package parinfer
  :defer t)
```

## Evil snipe

```emacs-lisp
(use-package evil-snipe
  :defer t
  :config
  (setq evil-snipe-scope 'visible)
  (setq evil-snipe-repeat-scope 'buffer)
  (setq evil-snipe-spillover-scope 'whole-buffer))
```

# Languages

## Rust

```emacs-lisp
(add-hook 'rustic-mode-hook (lambda ()
              (set (make-local-variable 'company-backends) '(company-tabnine))))
```

## Python

Adding TabNine completion to buffer

```emacs-lisp
(use-package python-mode
  :ensure nil
  :after flycheck
  :mode "\\.py\\'"
  :custom
  (python-indent-offset 4)
  (flycheck-python-pycompile-executable "python3")
  (python-shell-interpreter "python3")
  :config
  (add-hook 'python-mode-hook (lambda ()
                                (set (make-local-variable 'company-backends) '(company-tabnine)))))
```

## Dart

```emacs-lisp
(add-hook 'dart-mode-hook #'lsp-deferred)  ;; Add lsp support to dart
```

## Markdown

On save refresh markdown table of contents.

```emacs-lisp
(add-hook 'gfm-mode-hook
          (lambda () (when buffer-file-name
                       (add-hook 'before-save-hook
                                 'markdown-toc-refresh-toc))))
```

## Emacs lisp

```emacs-lisp
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq-local company-backends '((company-capf company-dabbrev-code company-files)))
            (setq tab-width 2)))

(add-hook 'emacs-lisp-mode-hook 'easy-escape-minor-mode)
```

## TeX

```emacs-lisp
;; (use-package tex
;;   :ensure auctex
;;   :defer t
;;   :custom
;;   (TeX-auto-save t)
;;   (TeX-parse-self t)
;;   (TeX-master nil)
;;   ;; to use pdfview with auctex
;;   (TeX-view-program-selection '((output-pdf "pdf-tools"))
;;                               TeX-source-correlate-start-server t)
;;   (TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))
;;   (TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
;;   :hook
;;   (LaTeX-mode . (lambda ()
;;                   (turn-on-reftex)
;;                   (setq reftex-plug-into-AUCTeX t)
;;                   (reftex-isearch-minor-mode)
;;                   (setq TeX-PDF-mode t)
;;                   (setq TeX-source-correlate-method 'synctex)
;;                   (setq TeX-source-correlate-start-server t)))
;;   :config
;;   (when (version< emacs-version "26")
;;     (add-hook LaTeX-mode-hook #'display-line-numbers-mode)))
```

## YAML

```emacs-lisp
(add-hook 'yaml-mode-hook 'highlight-indent-guides-mode)
```

# Other config

## Use Command key as meta key (Only on MacOS)

```emacs-lisp
(setq mac-command-modifier 'meta)
```

# Post Initialization

## Play startup music

Play Apex Legends music when booting up Emacs.

```emacs-lisp
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

(run-with-idle-timer 0 nil '(lambda ()
                              (async-shell-command-no-window "/usr/bin/afplay ~/dotfiles/emacs/doom.d/audio/Crypto.wav")))
```
