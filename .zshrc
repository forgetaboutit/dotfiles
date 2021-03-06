ZSH=$HOME/.oh-my-zsh

ZSH_THEME="muhbaasu"

plugins=(git ssh-agent cabal zsh-syntax-highlighting)

source $ZSH/oh-my-zsh.sh

unsetopt correct
unsetopt correct_all

# Default stuff
export PATH=$PATH:/usr/local/sbin:/usr/local/bin:/usr/bin:/usr/bin/core_perl

# With Cabal binaries
PATH=$HOME/.cabal/bin:$PATH

# With RVM
PATH=$PATH:$HOME/.rvm/bin:$HOME/.gem/ruby/2.1.0/bin

# aliases
alias cabt="cabal test --test-option=--color --show-details=always"
alias sl="ls"

if which ghci-ng >/dev/null; then
    alias cabr="cabal repl --with-ghc=ghci-ng"
else
    alias cabr="cabal repl"
fi

source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

function def-cabal-install() {
    cabal install -j alex happy haskell-src-exts
    cabal install -j haddock hdevtools hlint pointfree stylish-haskell
}

function mkcd() {
    mkdir -p "$1" && cd "$1"
}

# Reattach to tmux, if connected over SSH.
if [[ "$TMUX" == "" ]]; then
  # Attempt to discover a detached session and attach to it;
  # Otherwise, create a new session.
  WHOAMI=$(whoami)

  if tmux has-session -t $WHOAMI 2>/dev/null; then
    tmux -2 attach-session -t $WHOAMI
  else
    tmux -2 new-session -s $WHOAMI
  fi
fi
