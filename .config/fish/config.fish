set -x EDITOR "emacs"
alias editor "$EDITOR"
alias edit "editor"

set -x PROFILE "$HOME/.config/fish/config.fish"
alias profile "editor $PROFILE"

alias reload "source $PROFILE"
alias bat batcat

set -x PATH "$HOME/.cargo/bin:$PATH"

starship init fish | source