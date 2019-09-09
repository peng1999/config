if [[ "$TERM" == "dumb" ]]; then
    return
fi

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

# config for z
export _Z_DATA=$HOME/.local/share/z/data

antigen bundle  esc/conda-zsh-completion

# Syntax highlighting bundle.
antigen bundle zsh-users/zsh-autosuggestions
antigen bundle zsh-users/zsh-syntax-highlighting

# Load the theme.
antigen theme refined

# Tell Antigen that you're done.
antigen apply

if which bat >/dev/null; then
    export BAT_STYLE=plain
    alias cat=bat
fi

# emacsclient
alias ec='emacsclient -ca ""'

export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"
