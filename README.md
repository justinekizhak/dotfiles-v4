[![img](https://img.shields.io/badge/Made_in-Doom_Emacs-blue?style=for-the-badge)](https://github.com/hlissner/doom-emacs)
[![img](https://img.shields.io/badge/follow_me-@alka1e-E4405F?style=for-the-badge&logo=instagram&labelColor=8f3c4c&logoColor=white)](https://www.instagram.com/alka1e)
[![img](https://img.shields.io/badge/follow_me-@alka1e-1DA1F2?style=for-the-badge&logo=twitter&labelColor=27597a&logoColor=white)](https://twitter.com/alka1e)

---

[![img](https://img.shields.io/badge/license-mit-blueviolet?style=for-the-badge)]()

---

# My Dotfiles

[[_TOC_]]

# Introduction

They say your dotfiles will most likely be the longest project you ever work on. So for this reason, your dotfiles must be organized in a disciplined manner for maintainability and extensibility.

My dotfiles are the direct reflection of my software development workflow and practices of my past 3 years.

My dotfiles have undergone many transformation from single file vimrc to forking someone else&rsquo;s to building a simple yet sophisticated structure for housing more dotfiles.

One thing you may notice is that my workflow consists of diverse toolkit, this makes it for easier adoption and exploration.

# Features

Contains settings for

- Emacs
  - Doom emacs
  - Spacemacs
- Vim
- Zsh
  - Zim framework
- Tmux
- Custom bash scripts
- Fonts
- VSCode (only install instructions for dotfiles)
- and some terminal settings

# Screenshots

### Doom Emacs - Dashboard

![img](./emacs/doom.d/images/dashboard.png)

### Doom Emacs - Org mode

![img](./emacs/doom.d/images/org-mode.png)

### Doom Emacs - Python mode

![img](./emacs/doom.d/images/python-mode.png)

### ZSH shell

![img](./zsh/images/zsh.png)

# Getting started

1.  `git clone https://gitlab.com/justinekizhak/dotfiles`
2.  All the instructions are in `devfile.toml`. Use [devinstaller](https://gitlab.com/devinstaller/devinstaller) for installing the packages

# Documentation

Each application has their own documentation in their folders.

| Application | Documentation path                                                     |
| ----------- | ---------------------------------------------------------------------- |
| Doom Emacs  | <https://gitlab.com/justinekizhak/dotfiles/-/tree/master/emacs/doom.d> |
| ZSH         | <https://gitlab.com/justinekizhak/dotfiles/-/tree/master/zsh>          |

_Remaining docs are WIP_

## Repository mirroring

**Gitlab -> Github** repository mirroring.

My main config lives in [Gitlab](https://gitlab.com/justinekizhak/dotfiles), but I do maintain a mirror at [Github](https://github.com/justinekizhak/dotfiles).

The mirroring is done automatically by Gitlab. All I have to do it just keep on pushing commits onto Gitlab.

So here is how to setup the mirroring:

### Step1: Create Github personal token

Instructions <https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token>

### Step2: Mirroring from Gitlab -> Github

Instructions: <http://repositories.compbio.cs.cmu.edu/help/workflow/repository_mirroring.md#setting-up-a-mirror-from-gitlab-to-github>

Make sure to add token in both the URL and in the `password` textfield.

# License

Licensed under the terms of [MIT License](LICENSE.md)

---

[![forthebadge](https://forthebadge.com/images/badges/built-with-love.svg)](https://forthebadge.com)
[![forthebadge](https://forthebadge.com/images/badges/built-with-swag.svg)](https://forthebadge.com)
[![forthebadge](https://forthebadge.com/images/badges/60-percent-of-the-time-works-every-time.svg)](https://forthebadge.com)
