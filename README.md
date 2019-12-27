<a name="top"></a>
[![Built with Spacemacs](https://cdn.rawgit.com/syl20bnr/spacemacs/442d025779da2f62fc86c2082703697714db6514/assets/spacemacs-badge.svg)](http://spacemacs.org)
<a href="https://www.instagram.com/justinekizhak"><img src="https://i.imgur.com/G9YJUZI.png" alt="Instagram" align="right"></a>
<a href="https://twitter.com/justinekizhak"><img src="http://i.imgur.com/tXSoThF.png" alt="Twitter" align="right"></a>
<a href="https://www.facebook.com/justinekizhak"><img src="http://i.imgur.com/P3YfQoD.png" alt="Facebook" align="right"></a>
<br>
- - -
[![MIT license](https://img.shields.io/badge/License-MIT-blue.svg)](https://lbesson.mit-license.org/)
<!-- {Put your badges here} -->

- - -
# Dotfiles
- - -

**Table of Contents**

- [Introduction](#introduction)
- [Features](#features)
- [Get Started](#get-started)
- [Full Documentation](#full-documentation)
    - [Add modules](#add-modules)
        - [Add new prompt themes](#add-new-prompt-themes)
- [License](#license)


## Introduction

They say your dotfiles will most likely be the longest project you ever work on.
So for this reason, your dotfiles must be organized in a disciplined manner for
maintainability and extensibility.

My dotfiles are the direct reflection of my software development workflow and
practices of my past 3 years.

My dotfiles have undergone many transformation from single file vimrc to
forking someone else's to building a simple yet sophisticated structure for
housing more dotfiles.

One thing you may notice is that my workflow consists of diverse toolkit,
this makes it for easier adoption and exploration.

**[Back to top](#table-of-contents)**

## Features

Contains settings for

- Emacs
    - Spacemacs
- Vim
- Zsh
    - Zim framework
- Tmux
- Custom bash scripts
- Fonts
- VSCode (only install instructions for dotfiles)
- and some terminal settings

**[Back to top](#table-of-contents)**

## Get Started


1) `git clone https://gitlab.com/justinekizhak/dotfiles`

2) `cd dotfiles & rm -rf .git`

3) Use instructions in `install.yaml` file.
  Just copy paste lines in the file into terminal.
  All instructions are bash compatible.

**[Back to top](#table-of-contents)**

## Full Documentation

This repo uses [git subrepo] for adding modules.

### Add modules
  `git subrepo clone MODULE_URL MODULE_LOCATION`

#### Add new prompt themes

- Subrepo clone the module
    `git subrepo clone PROMPT_URL PROMPT_LOCATION`

- Symlink zsh file as `prompt_FILENAME_setup`

Example:

```
git subrepo clone https://github.com/denysdovhan/spaceship-prompt \
zsh/zim/modules/prompt/external-themes/spaceship
```

```
ln -s ~/dotfiles/zsh/zim/modules/prompt/external-themes/spaceship/spaceship.zsh \
~/dotfiles/zsh/zim/modules/prompt/functions/prompt_spaceship_setup
```

---

Repo at [GITLAB][website].

Read [CHANGELOG], [CODE OF CONDUCT], [CONTRIBUTING] guide.

[git subrepo]: https://github.com/ingydotnet/git-subrepo
[website]: https://gitlab.com/justinekizhak/dotfiles
[CHANGELOG]: CHANGELOG.md
[CONTRIBUTING]: CONTRIBUTING.md
[CODE OF CONDUCT]: CODE_OF_CONDUCT.md

## License

Licensed under the terms of [MIT License].

[MIT License]: LICENSE.txt

**[Back to top](#table-of-contents)**


- - -
[![forthebadge](https://forthebadge.com/images/badges/compatibility-betamax.svg)](https://forthebadge.com)
[![forthebadge](https://forthebadge.com/images/badges/powered-by-water.svg)](https://forthebadge.com)
[![forthebadge](https://forthebadge.com/images/badges/built-with-love.svg)](https://forthebadge.com)
- - -