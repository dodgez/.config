set -x EDITOR "emacs -nw"
alias editor="$EDITOR"
alias edit="editor"

alias ll="ls -l"
alias lal="ls -al"

alias bat="batcat"
alias untar="tar -xvsf"
alias rimraf="rm -rf"

set -x PROFILE "$HOME/.config/fish/config.fish"
alias profile='editor $PROFILE'
alias reload='source $PROFILE'

fish_add_path "$HOME/.cargo/bin"
fish_add_path "$HOME/.emacs.d/bin"

if test -e "$HOME/.config/fish/work_config.fish"
    source "$HOME/.config/fish/work_config.fish"
end

starship init fish | source
