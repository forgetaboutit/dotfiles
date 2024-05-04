#!/usr/bin/env bash
set -ex
clear

flake=$(hostname)
home_name="sammy@$flake"

function updateNixos() {
  sudo nixos-rebuild switch --flake ".#$flake"
}

function updateHomeManager() {
  home-manager switch --flake ".#$home_name"
}

if [ "$1" == "hm" ]; then
  updateHomeManager
  exit 0
fi

updateNixos
updateHomeManager

