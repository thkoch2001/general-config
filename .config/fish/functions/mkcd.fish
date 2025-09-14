# From https://dbohdan.com/mkcd
# License: MIT
# https://dbohdan.com/mit-license/2025

function mkcd --argument name --description "Create a directory and 'cd' to it"
    if test (count $argv) -ne 1
        printf 'usage: mkcd name\n' >&2
        return 2
    end

    mkdir -p $name
    cd $name
end