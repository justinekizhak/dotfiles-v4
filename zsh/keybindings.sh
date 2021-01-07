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


zmodload zsh/complist
bindkey -M menuselect '^[[Z' reverse-menu-complete
