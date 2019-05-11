#!/usr/bin/env sh
# -----------------------------------------------------------------------------
# Copyright (c) 2018, Justine T Kizhakkinedath
# All rights reserved
#
# Licensed under the terms of MIT License
# See LICENSE file in the project root for full information.
# -----------------------------------------------------------------------------

add-to-path () {
        if ! echo "$PATH" | /usr/bin/grep -Eq "(^|:)$1($|:)" ; then
           if [ "$2" = "after" ] ; then
              PATH="$PATH:$1"
           else
              PATH="$1:$PATH"
           fi
        fi
}

ping-loop () {
    while True; do
        ping $1
    done
}

mkdir-cd () {
    mkdir $1 && cd $1
}
