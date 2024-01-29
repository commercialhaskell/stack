#!/usr/bin/env bash
# Copyright (c) 2023 b-data GmbH.
# Distributed under the terms of the MIT License.

set -e

if dpkg --compare-versions "${CABAL_VERSION%.*.*}" le-nl "3.8"; then
  mkdir -p "$HOME/.cabal/bin";
fi
mkdir -p "$HOME/.local/bin"

# Copy Zsh-related files and folders from the untouched home directory
if [ "$(id -un)" == "root" ]; then
  if [ ! -d /root/.oh-my-zsh ]; then
    cp -R /home/*/.oh-my-zsh /root;
  fi
  if [ ! -f /root/.zshrc ]; then
    cp /home/*/.zshrc /root;
  fi
else
  if [ ! -d "$HOME/.oh-my-zsh" ]; then
    sudo cp -R /root/.oh-my-zsh "$HOME";
    sudo chown -R "$(id -u)":"$(id -g)" "$HOME/.oh-my-zsh";
  fi
  if [ ! -f "$HOME/.zshrc" ]; then
    sudo cp /root/.zshrc "$HOME";
    sudo chown "$(id -u)":"$(id -g)" "$HOME/.zshrc";
  fi
fi

# If existent, prepend the user's private bin to PATH
if ! grep -q "user's private bin" "$HOME/.bashrc"; then
  cat "/var/tmp/snippets/rc.sh" >> "$HOME/.bashrc"
fi
if ! grep -q "user's private bin" "$HOME/.zshrc"; then
  cat "/var/tmp/snippets/rc.sh" >> "$HOME/.zshrc"
fi

# Set PATH so it includes cabal's bin if it exists
if ! grep -q "cabal's bin" "$HOME/.bashrc"; then
  cat "/var/tmp/snippets/rc2.sh" >> "$HOME/.bashrc"
fi
if ! grep -q "cabal's bin" "$HOME/.zshrc"; then
  cat "/var/tmp/snippets/rc2.sh" >> "$HOME/.zshrc"
fi

# Enable Oh My Zsh plugins
sed -i "s/plugins=(git)/plugins=(cabal git pip screen stack tmux vscode)/g" \
  "$HOME/.zshrc"

# Remove old .zcompdump files
rm -f "$HOME"/.zcompdump*
