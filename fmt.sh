#!/usr/bin/env bash
fd --type f --glob "*.nix" --exec alejandra 2>/dev/null

