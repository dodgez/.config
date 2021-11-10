set -x EDITOR "micro"
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
fish_add_path "/usr/local/bin"

if test -e "$HOME/.config/fish/work_config.fish"
    source "$HOME/.config/fish/work_config.fish"
end

switch (uname -s)
    case "*CYGWIN*"
        source "$HOME/.config/fish/starship_cygwin.fish"
    case '*'
        starship init fish | source
end
