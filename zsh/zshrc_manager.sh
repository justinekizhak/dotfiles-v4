# Created: Sun 15 Jul 2018 14:58:36 IST
# Last-Updated: Wed 23 Oct 2019 18:19:52 IST
#
# zshrc_manager.sh is part of dotfiles
# URL: https://gitlab.com/justinethomas/dotfiles
# Description: This is the file that is symlinked to the ~/.zshrc and this
# file sources other files.
#
# Copyright (c) 2018, Justine T Kizhakkinedath
# All rights reserved
#
# Licensed under the terms of MIT License
# See LICENSE file in the project root for full information.
# -----------------------------------------------------------------------------
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
# DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
# OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE
# OR OTHER DEALINGS IN THE SOFTWARE.
#
# -----------------------------------------------------------------------------

# # Profiling
# zmodload zsh/zprof

# Run tmux if exists
if command -v tmux>/dev/null; then
 [ -z $TMUX ] && exec tmux
else
 echo "tmux not installed."
fi

export ZIM_HOME=${ZDOTDIR:-${HOME}}/dotfiles/zsh/zim
[[ -s ${ZIM_HOME}/init.zsh ]] && source ${ZIM_HOME}/init.zsh

source ~/dotfiles/zsh/custom_functions.sh
source ~/dotfiles/zsh/keybindings.sh
source ~/dotfiles/zsh/zshrc.sh
source ~/dotfiles/zsh/env_var.zsh

# [[ -s ${ZDOTDIR:-${HOME}}/dotfile/zsh/zimrc ]] && source ${ZDOTDIR:-${HOME}}/dotfiles/zsh/.zimrc
