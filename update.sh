#!/usr/bin/env bash
set -ex
clear

flake=$(hostname)
home_name="sammy@$flake"

function updateNixos() {
  sudo nixos-rebuild switch --flake ".#$flake" "$@"
}

function updateNixosServer() {
  # The `muhbaasu` host is configured for connecting using the SSH config
  nixos-rebuild switch \
    --flake ".#muhbaasu" \
    --build-host "muhbaasu" \
    --target-host "muhbaasu" \
    "$@"
}

function updateHomeManager() {
  home-manager switch --flake ".#$home_name"
}

if [ "$1" == "hm" ]; then
  shift
  updateHomeManager "$@"
  exit 0
elif [ "$1" == "srv" ]; then
  shift
  updateNixosServer "$@"
  exit 0
fi

updateNixos "$@"
updateHomeManager "$@"

