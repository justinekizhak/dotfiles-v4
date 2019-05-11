#!/usr/bin/env bash
# -----------------------------------------------------------------------------
# Created: Sun 15 Jul 2018 14:54:27 IST
# Last-Updated: Sun 15 Jul 2018 14:55:15 IST
#
# install_cmdT.sh is part of dotfiles
# URL: https://gitlab.com/justinethomas/dotfiles
# Description: Script to install Command-T for Vim
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

has_vim=$(command -v vim >/dev/null)

if ! $has_vim; then
  echo "must have vim installed."
  exit 1
fi

install_cmdT () {
    echo "all good"
    cd ~/.vim/plugged/command-t/ruby/command-t/ext/command-t
    ruby extconf.rb
    make
}

install_system_ruby(){
    if [[ "$OSTYPE" == "linux-gnu" ]]; then
        sudo apt-get install ruby-full
    elif [[ "$OSTYPE" == "darwin"* ]]; then
        brew install ruby
    fi
}

# Checking the ruby support based on the line output received
has_ruby_support=$(vim --version | grep -c ruby)

if [ $has_ruby_support ]; then
    if [ $(which ruby) ]; then
        install_cmdT
    else
        install_system_ruby
    fi
else
    echo "your vim doesn't have support for ruby. \
You will have to install vim from source."
fi
