#!/usr/bin/env sh
# -----------------------------------------------------------------------------
# Copyright (c) 2018, Justine T Kizhakkinedath
# All rights reserved
#
# Licensed under the terms of MIT License
# See LICENSE file in the project root for full information.
# -----------------------------------------------------------------------------


mkdir_cd () {
    mkdir "$1" && cd "$1" || exit
}
