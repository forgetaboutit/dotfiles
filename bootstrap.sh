#!/usr/bin/env bash
nix run github:nix-community/nixos-anywhere -- --flake .#muhbaasu-bootstrap root@94.130.212.237

ssh muhbaasu nixos-generate-config

function fetch() {
  local CONFIG_SRC=/etc/nixos/$1
  local CONFIG_DST=./hosts/users/muhbaasu/$1

  scp -4 root@muhbaasu.de:$CONFIG_SRC $CONFIG_DST
}

fetch configuration.nix
fetch hardware-configuration.nix

