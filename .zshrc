fpath+=('/Users/ybbond/.local/share/zsh/site-functions')
autoload -Uz compinit && compinit
# autoload -U compinit && compinit

source ~/.zshrc_git_symbols

# red       009
# yellow    011
# lime      010
# green     002
# blue      012
# aqua      014
# turqoise  045
BACKGROUND_JOBS_PROMPT='%(1j.%F{009}[%j] %f.)'
CLOCK_PROMPT='%F{011}%*%f'
CURRENT_PATH_PROMPT='%F{010}%~%f'
GIT_PROMPT='%F{045}$(_git_info)%f'
# END_PROMPT='%(?.%F{002}.%F{009})▶%f'
# END_PROMPT='%(?.%F{002}.%F{009})λ%f'
END_PROMPT='%(?.%F{002}.%F{009})%(!.#.$)%f'

# PROMPT='%F{011}%*%f %F{047}%~%f%F{039}$(_git_info) %(?.%F{green}.%F{red})$%f '
# palette colors 0-15 only, so the terminal's light/dark theme can remap them
PROMPT='$BACKGROUND_JOBS_PROMPT$CLOCK_PROMPT $CURRENT_PATH_PROMPT${(e)GIT_PROMPT} $END_PROMPT '
# PROMPT='%F{yellow}%* %F{green}%~%F{cyan}$(_git_info) %(?.%F{green}.%F{red})$%f '

export GPG_TTY=$(tty)
export PATH="$HOME/.tool_binaries:$PATH"

alias neorc="cd ~/.config/nvim && nvim . && cd -"

function zvm_config() {
  ZVM_VI_INSERT_ESCAPE_BINDKEY=jj
}

# word = letters, digits, _ ; so option+backspace stops at / . - = etc.
WORDCHARS='_'

function zvm_after_init() {
  bindkey -M viins '^F' autosuggest-accept
  bindkey -M viins '^P' history-substring-search-up
  bindkey -M viins '^N' history-substring-search-down
  # option+backspace (ghostty sends ESC DEL)
  bindkey -M viins '^[^?' backward-kill-word
}

## PLUGINS

# https://github.com/jeffreytse/zsh-vi-mode
source $(brew --prefix)/opt/zsh-vi-mode/share/zsh-vi-mode/zsh-vi-mode.plugin.zsh
# https://github.com/zsh-users/zsh-autosuggestions
source $(brew --prefix)/share/zsh-autosuggestions/zsh-autosuggestions.zsh
# https://github.com/zsh-users/zsh-syntax-highlighting
source $(brew --prefix)/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
# https://github.com/zsh-users/zsh-history-substring-search
source $(brew --prefix)/share/zsh-history-substring-search/zsh-history-substring-search.zsh


# has companion `eval "$(mise activate zsh --shims)"`
# in ~/.zprofile
eval "$(~/.local/bin/mise activate zsh)"
