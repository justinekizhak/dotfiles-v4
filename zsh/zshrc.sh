# -----------------------------------------------------------------------------
# Copyright (c) 2018, Justine T Kizhakkinedath
# All rights reserved
#
# Licensed under the terms of MIT License
# See LICENSE file in the project root for full information.
# -----------------------------------------------------------------------------

# Alias
alias ec="emacsclient -c -a '' -n"
alias psg="ps ax | rg"
alias gs="git status"
alias kill-last="kill %1"
alias reload="source ~/.zshrc"

# For vim mappings:
  stty -ixon

# Fix for arrow-key searching
# start typing + [Up-Arrow] - fuzzy find history forward
if [ "${terminfo[kcuu1]}" != "" ]; then
  autoload -U up-line-or-beginning-search
  zle -N up-line-or-beginning-search
  bindkey "${terminfo[kcuu1]}" up-line-or-beginning-search
fi
# start typing + [Down-Arrow] - fuzzy find history backward
if [ "${terminfo[kcud1]}" != "" ]; then
  autoload -U down-line-or-beginning-search
  zle -N down-line-or-beginning-search
  bindkey "${terminfo[kcud1]}" down-line-or-beginning-search
fi

eval "$(fasd --init auto)"
eval "$(pipenv --completion)"

export PATH="/home/justine/.pyenv/bin:$PATH"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

alias eme='
setsid emacs
'

# # fix interop
# fix_wsl2_interop() {
# 	for i in $(pstree -np -s $$ | grep -o -E '[0-9]+'); do
# 		if [[ -e "/run/WSL/${i}_interop" ]]; then
# 			export WSL_INTEROP=/run/WSL/${i}_interop
# 		fi
# 	done
# }

# ~/.emacs.d/bin/doom env > /dev/null 2>&1
