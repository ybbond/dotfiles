autoload -U compinit && compinit

source ~/.zshrc_git_symbols

PROMPT='%F{011}%*%f %F{047}%~%f%F{039}$(_git_info) %(?.%F{green}.%F{red})$%f '
# PROMPT='%F{yellow}%* %F{green}%~%F{cyan}$(_git_info) %(?.%F{green}.%F{red})$%f '

export GPG_TTY=$(tty)
export GOPATH="$HOME/go"; export GOROOT="$HOME/.go"; export PATH="$GOPATH/bin:$PATH"; # g-install: do NOT edit, see https://github.com/stefanmaric/g
# export JAVA_HOME="/zulu-21.jdk/Contents/Home"
export JAVA_HOME="$HOME/.tool_binaries/zulu25.30.17-ca-jdk25.0.1-macosx_aarch64/"
export PATH=$JAVA_HOME/bin:$PATH
export PATH="$HOME/.tool_binaries:$PATH"
export PATH="$HOME/.tool_binaries/zig:$PATH"
export PATH="$HOME/.tool_binaries/jdtls/bin:$PATH"

alias neorc="cd ~/.config/nvim && nvim . && cd -"
alias roswank="ros run --eval '(ql:quickload :swank)'  --eval '(swank:create-server :dont-close t)'"
alias flutter="fvm flutter"

function zvm_config() {
  ZVM_VI_INSERT_ESCAPE_BINDKEY=jk
}

function zvm_after_init() {
  bindkey -M viins '^F' autosuggest-accept
  bindkey -M viins '^P' history-substring-search-up
  bindkey -M viins '^N' history-substring-search-down
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

## [Completion]
## Completion scripts setup. Remove the following line to uninstall
[[ -f /Users/ybbond/.dart-cli-completion/zsh-config.zsh ]] && . /Users/ybbond/.dart-cli-completion/zsh-config.zsh || true
## [/Completion]


export N_PREFIX="$HOME/n"; [[ :$PATH: == *":$N_PREFIX/bin:"* ]] || PATH+=":$N_PREFIX/bin"  # Added by n-install (see http://git.io/n-install-repo).

[ -f "/Users/ybbond/.ghcup/env" ] && . "/Users/ybbond/.ghcup/env" # ghcup-env

# pnpm
export PNPM_HOME="/Users/ybbond/Library/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac
# pnpm end
