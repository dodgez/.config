# Create di = ls ^ cat alias
function di
    set _dir $argv[1]
    if test -d $_dir
        ls -alh $_dir
    else
        cat $_dir
    end
end

# Long and all list directory aliases
alias ll="ls -l"
alias lal="ls -al"

# Alias to untar
alias untar="tar -xvsf"

# Rimraf = rm -rf alias
alias rimraf="rm -rf"

# Pickup a work config if present
if test -e "$HOME/.config/fish/work_config.fish"
    source "$HOME/.config/fish/work_config.fish"
end

starship init fish | source
