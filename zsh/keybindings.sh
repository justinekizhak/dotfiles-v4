# -----------------------------------------------------------------------------
# Copyright (c) 2018, Justine T Kizhakkinedath
# All rights reserved
#
# Licensed under the terms of MIT License
# See LICENSE file in the project root for full information.
# -----------------------------------------------------------------------------

# accept auto-complete
    #Ctrl-space to accept autosuggest
    bindkey '^ ' autosuggest-accept

# git
  function git_prepare() {
    if [ -n "$BUFFER" ];
      then
        BUFFER="git add -A; git commit -m \"$BUFFER\" && git push"
    fi

    if [ -z "$BUFFER" ];
      then
        BUFFER="git add -A; git commit -v && git push"
    fi

    zle accept-line
  }
  zle -N git_prepare
  bindkey "^g" git_prepare

zmodload zsh/complist
bindkey -M menuselect '^[[Z' reverse-menu-complete
