#!/usr/bin/env bash

antibody bundle < ~/.zsh_plugins.txt > ~/.zsh_plugins.sh

fish --command="fisher"
fish --command="set -Ux EDITOR e"
