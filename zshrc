# source /usr/share/zsh/share/antigen.zsh
source ~/.antigen/antigen/antigen.zsh

# Load the oh-my-zsh's library.
antigen use oh-my-zsh

# Bundles from the default repo (robbyrussell's oh-my-zsh).
antigen bundle git
antigen bundle cargo
antigen bundle pip
antigen bundle command-not-found
antigen bundle colored-man-pages
antigen bundle z
antigen bundle extract

# config for z
export _Z_DATA=$HOME/.local/share/z/data

# Syntax highlighting bundle.
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-autosuggestions

# Load the theme.
antigen theme refined

# Tell Antigen that you're done.
antigen apply

if which bat >/dev/null; then
    export BAT_STYLE=plain
    alias cat=bat
fi

