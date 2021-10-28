# Taken from "starship init fish --print-full-init" where starship is version 0.58.0

function fish_prompt
    switch "$fish_key_bindings"
        case fish_hybrid_key_bindings fish_vi_key_bindings
            set STARSHIP_KEYMAP "$fish_bind_mode"
        case '*'
            set STARSHIP_KEYMAP insert
    end
    set STARSHIP_CMD_STATUS $status
    # Account for changes in variable name between v2.7 and v3.0
    set STARSHIP_DURATION "$CMD_DURATION$cmd_duration"
    set STARSHIP_JOBS (count (jobs -p))
    # Fix windows-style line endings
    echo (/usr/local/bin/starship.exe prompt --status=$STARSHIP_CMD_STATUS --pipestatus=$pipestatus --keymap=$STARSHIP_KEYMAP --cmd-duration=$STARSHIP_DURATION --jobs=$STARSHIP_JOBS | dos2unix --force)
end

function fish_right_prompt
    # Fix windows-style line endings
    echo (/usr/local/bin/starship.exe prompt --right --status=$STARSHIP_CMD_STATUS --pipestatus=$pipestatus --keymap=$STARSHIP_KEYMAP --cmd-duration=$STARSHIP_DURATION --jobs=$STARSHIP_JOBS | dos2unix --force)
end

# Disable virtualenv prompt, it breaks starship
set -g VIRTUAL_ENV_DISABLE_PROMPT 1

# Remove default mode prompt
builtin functions -e fish_mode_prompt

set -gx STARSHIP_SHELL "fish"

# Set up the session key that will be used to store logs
set -gx STARSHIP_SESSION_KEY (random 10000000000000 9999999999999999)
