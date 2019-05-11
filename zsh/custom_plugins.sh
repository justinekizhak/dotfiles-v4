# -----------------------------------------------------------------------------
# Copyright (c) 2018, Justine T Kizhakkinedath
# All rights reserved
#
# Licensed under the terms of MIT License
# See LICENSE file in the project root for full information.
# -----------------------------------------------------------------------------

# # Fixls
# source ~/dotfiles/zsh/plugins/fixls.zsh

# # Powelevel9k  https://github.com/bhilburn/powerlevel9k     Loading time: 18 ms
# source ~/dotfiles/zsh/plugins/powerlevel9k/powerlevel9k.zsh-theme
# # New line after prompt
# POWERLEVEL9K_PROMPT_ADD_NEWLINE=true
# # for contents on left and right
# POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(dir vcs status root_indicator background_jobs command_execution_time custom_now_playing)
# POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(virtualenv vi_mode)
# # show error code
# POWERLEVEL9K_STATUS_OK=false
# # show raw error values
# POWERLEVEL9K_STATUS_HIDE_SIGNAME=true
# # display execution time
# POWERLEVEL9K_COMMAND_EXECUTION_TIME_THRESHOLD=1
# POWERLEVEL9K_COMMAND_EXECUTION_TIME_PRECISION=2
# # customizing how it shows which vi mode I am in
# POWERLEVEL9K_VI_MODE_INSERT_FOREGROUND='black'
# POWERLEVEL9K_VI_MODE_INSERT_BACKGROUND='cyan'
# POWERLEVEL9K_VI_MODE_NORMAL_FOREGROUND='black'
# POWERLEVEL9K_VI_MODE_NORMAL_BACKGROUND='green'
# # using patched font
# POWERLEVEL9K_MODE="nerdfont-complete"
# POWERLEVEL9K_LEFT_SEGMENT_SEPARATOR=$'\uE0B0'
# POWERLEVEL9K_CUSTOM_NOW_PLAYING='~/dotfiles/scripts/nowplaying'
# POWERLEVEL9K_CUSTOM_NOW_PLAYING_BACKGROUND='blue'
# POWERLEVEL9K_CUSTOM_NOW_PLAYING_FOREGROUND='black'
# # Double-Lined Prompt
# POWERLEVEL9K_PROMPT_ON_NEWLINE=true
# POWERLEVEL9K_RPROMPT_ON_NEWLINE=false
# POWERLEVEL9K_MULTILINE_FIRST_PROMPT_PREFIX="%F{014}╭%F{cyan}"
# POWERLEVEL9K_MULTILINE_LAST_PROMPT_PREFIX="%F{014}\u2570%F{cyan}\uF460%F{073}\uF460%F{109}\uF460%f "


# Zsh autosuggestions https://github.com/zsh-users/zsh-autosuggestions/       Loading time: 2 ms
source ~/dotfiles/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
# refer this image https://misc.flogisoft.com/_media/bash/colors_format/256_colors_bg.png
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=4'

# # Zsh sytax highlighting https://github.com/zsh-users/zsh-syntax-highlighting      Loading time: 12 ms
# source ~/dotfiles/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# Vi mode       Loading time: 4 ms
source ~/dotfiles/zsh/plugins/vi-mode.plugin.zsh

# # Colorls      Loading time: 24 ms
# source $(dirname $(gem which colorls))/tab_complete.sh

# # Fasd         Loading time: 20 ms
# eval "$(fasd --init auto)"

# # This loads nvm
# # export NVM_DIR="$HOME/.nvm"
# # [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
