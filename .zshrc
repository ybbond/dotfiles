########################################################################
#                     EXPORTS
########################################################################

export EDITOR=nvim
export SHELL=zsh
export LANG=en_US.UTF-8
export TERM=screen-256color

export PATH=$HOME/bin:/usr/local/bin:$PATH
export PATH="/usr/local/sbin:$PATH"
# export PATH=$HOME/.local/bin:$PATH

export PATH=$PATH:$HOME/go/bin
export PATH=$PATH:$HOME/.cargo/bin
# export PATH=$PATH:/usr/local/bin
export PATH=/.config/yarn/global/node_modules/.bin:$PATH./node_modules/.bin:$HOME/.yarn/bin:$HOME
export PATH=/usr/local/opt/openssl/bin:$PATH
export PATH=$PATH:$KUMPATH/yowez-cli/bin
export GOPATH=$HOME/go
export PATH=$GOPATH/bin:$PATH
export PATH=$HOME/.nimble/bin:$PATH
export PATH="/Applications/Emacs.app/Contents/MacOS:$PATH"
export PATH="$HOME/.emacs.d/bin:$PATH"
# export INITVIM=$HOME/.config/nvim/init.vim
# export PATH=$HOME/bin:$PATH

export BAT_THEME="gruvbox-light"

export ANDROID_HOME=${HOME}/Library/Android/sdk
export JAVA_HOME=/Applications/Android\ Studio.app/Contents/jre/jdk/Contents/Home
export PATH=${PATH}:${ANDROID_HOME}/emulator
export PATH=${PATH}:${ANDROID_HOME}/tool
export PATH="${PATH}:$ANDROID_HOME/tools:$ANDROID_HOME/platform-tools"

export SCRIPT=~/Library/Mobile\ Documents/com\~apple\~ScriptEditor2/Documents

########################################################################
#                     FUNCTIONS
########################################################################

# retry command after n times
function gagal {
  echo $1 >&2
  exit 1
}
function kantal {
  local n=1
  local max="$1"; shift
  local delay=3
  while true; do
    "$@" && break || {
      if [[ $n -lt $max ]]; then
        ((n++))
        echo "Gagal nih. OTW percobaan ke $n/$max:"
        sleep $delay;
      else
        gagal "Gagal muluk abis $n kali coba."
      fi
    }
  done
}

# test
function test_kantal {
  echo "your first argument $1"
  echo "your @ argument $@"
  echo "your 0 argument $0"
  local first="$1"; shift
  echo "now your first argument $1"
  echo "now your @ argument $@"
}

function tempjson {
  nvim ~/tempjson$1.json
}

function commit {
  git add . && git commit -m "$1"
}

n ()
{
    # Block nesting of nnn in subshells
    if [ -n $NNNLVL ] && [ "${NNNLVL:-0}" -ge 1 ]; then
        echo "nnn is already running"
        return
    fi

    # The default behaviour is to cd on quit (nnn checks if NNN_TMPFILE is set)
    # To cd on quit only on ^G, remove the "export" as in:
    #     NNN_TMPFILE="${XDG_CONFIG_HOME:-$HOME/.config}/nnn/.lastd"
    # NOTE: NNN_TMPFILE is fixed, should not be modified
    NNN_TMPFILE="${XDG_CONFIG_HOME:-$HOME/.config}/nnn/.lastd"

    # Unmask ^Q (, ^V etc.) (if required, see `stty -a`) to Quit nnn
    # stty start undef
    # stty stop undef
    # stty lwrap undef
    # stty lnext undef

    nnn -deo "$@"

    if [ -f "$NNN_TMPFILE" ]; then
            . "$NNN_TMPFILE"
            rm -f "$NNN_TMPFILE" > /dev/null
    fi
}

export NNN_USE_EDITOR=1
export NNN_TRASH=1
[ -n "$NNNLVL" ] && PS1="N$NNNLVL $PS1"


########################################################################
#                     ALIASES
########################################################################

eval "$(hub alias -s)"

# projects
alias kapp="cd ${KUMPATH}/app"
alias kweb="cd ${KUMPATH}/web"
alias kmag="cd ${KUMPATH}/dashboard"
alias kwid="cd ${KUMPATH}/widget"
alias kfer="cd ${KUMPATH}/ferdi"
alias uikit="cd ${KUMPATH}/js/packages/kumparan-uikit"
alias slatep="cd ${KUMPATH}/js/packages/slate-plugins"
alias ktrack="cd ${KUMPATH}/js/packages/kumparan-tracker"
alias ybbond="cd ${YBPATH}/ybbond"
alias ybbold="cd ${YBPATH}/old-ybbond"
alias ybreason="cd ${YBPATH}/ybbond-reason"
alias ftex="cd ${YBPATH}/latex"
alias todo="nvim ~/todo.md"
alias tempbin="nvim ~/tempbin.md"
alias personal="nvim ~/personal.md"
alias mine="nvim ~/Library/Mobile\ Documents/27N4MQEA55~pro~writer/Documents"
alias fetchwm="WEBMENTIONS_TOKEN=${WMTOKEN} node ${YBPATH}/ybbond/webmentions.js"

# settings
alias zshrc="nvim ~/.zshrc"
alias vimrc="vim ~/.vimrc"
alias bashpro="nvim ~/.bash_profile"
alias bashrc="nvim ~/.bashrc"
alias neorc="nvim ~/.config/nvim/init.vim"

# git
alias wip="git add . && git commit -m 'wip'"
alias rehead="git reset HEAD~"
alias lg="lazygit"

# misc
alias ld="lazydocker"
alias r="ranger"
alias pixel3a="~/Library/Android/sdk/emulator/emulator -avd Pixel_3a_API_29 -netdelay none -netspeed full"
alias pixelxl="~/Library/Android/sdk/emulator/emulator -avd Pixel_XL_API_30 -netdelay none -netspeed full"

alias cdb="cd .."
alias konak="kantal 10 yarn add"

# better cli
alias ls='exa'
alias top='htop'
alias cat='alias cat="bat --theme=\$(defaults read -globalDomain AppleInterfaceStyle &> /dev/null && echo default || echo GitHub)"'
alias find='fd'
alias du='ncdu --color dark -rr'

alias tarex='tar -xzf'

source ~/Library/Preferences/org.dystroy.broot/launcher/bash/br


########################################################################
#                     SPACESHIP
########################################################################

SPACESHIP_PROMPT_ORDER=(
    battery
    time
    user
    dir
    host
    git
    exec_time
    jobs
    exit_code
    vi_mode
    char
  )
SPACESHIP_PROMPT_FIRST_PREFIX_SHOW=true
SPACESHIP_PROMPT_SEPARATE_LINE=false
SPACESHIP_BATTERY_SHOW=always
SPACESHIP_BATTERY_PREFIX='['
SPACESHIP_BATTERY_SUFFIX=']'
SPACESHIP_BATTERY_THRESHOLD=100
SPACESHIP_GIT_PREFIX='['
SPACESHIP_GIT_SUFFIX=']'
SPACESHIP_GIT_BRANCH_PREFIX='•'
SPACESHIP_GIT_BRANCH_SUFFIX='•'
SPACESHIP_GIT_STATUS_PREFIX='<'
SPACESHIP_GIT_STATUS_SUFFIX='>'
SPACESHIP_DIR_PREFIX='['
SPACESHIP_DIR_SUFFIX=']'
SPACESHIP_CHAR_SYMBOL='λ'
SPACESHIP_CHAR_PREFIX=' '
SPACESHIP_CHAR_SUFFIX=' '
SPACESHIP_TIME_SHOW=true
SPACESHIP_TIME_PREFIX='['
SPACESHIP_TIME_SUFFIX=']'
SPACESHIP_VI_MODE_PREFIX=''
SPACESHIP_VI_MODE_COLOR='gray'

########################################################################
#                     ZINIT
########################################################################

### Added by Zinit's installer
if [[ ! -f $HOME/.zinit/bin/zinit.zsh ]]; then
    print -P "%F{33}▓▒░ %F{220}Installing DHARMA Initiative Plugin Manager (zdharma/zinit)…%f"
    command mkdir -p "$HOME/.zinit" && command chmod g-rwX "$HOME/.zinit"
    command git clone https://github.com/zdharma/zinit "$HOME/.zinit/bin" && \
        print -P "%F{33}▓▒░ %F{34}Installation successful.%f%b" || \
        print -P "%F{160}▓▒░ The clone has failed.%f%b"
fi

source "$HOME/.zinit/bin/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

# Load a few important annexes, without Turbo
# (this is currently required for annexes)
zinit light-mode for \
    zinit-zsh/z-a-patch-dl \
    zinit-zsh/z-a-as-monitor \
    zinit-zsh/z-a-bin-gem-node

### End of Zinit's installer chunk

# Plugins

zplugin light zsh-users/zsh-autosuggestions
  bindkey -M viins '^F' autosuggest-accept
  ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=#d6d6d6"

zplugin light zsh-users/zsh-history-substring-search
  bindkey -M viins '^P' history-substring-search-up
  bindkey -M viins '^N' history-substring-search-down

zplugin light zdharma/fast-syntax-highlighting

zplugin light denysdovhan/spaceship-prompt

# export NVM_DIR="$HOME/.nvm"
# export NVM_COMPLETION=true
# export NVM_LAZY_LOAD=true
# export NVM_LAZY_LOAD_EXTRA_COMMANDS=('nvim')
zplugin light lukechilds/zsh-nvm

zplugin load zdharma/history-search-multi-word

zplugin load zsh-users/zsh-completions
  autoload -Uz compinit
  if [ $(date +'%j') != $(/usr/bin/stat -f '%Sm' -t '%j' ${ZDOTDIR:-$HOME}/.zcompdump) ]; then
    compinit
  else
    compinit -C
  fi

  {
    # Compile zcompdump, if modified, to increase startup speed.
    zcompdump="${ZDOTDIR:-$HOME}/.zcompdump"
    if [[ -s "$zcompdump" && (! -s "${zcompdump}.zwc" || "$zcompdump" -nt "${zcompdump}.zwc") ]]; then
      zcompile "$zcompdump"
    fi
  } &!

zplugin load softmoth/zsh-vim-mode
  bindkey -M viins "jk" vi-cmd-mode
  export KEYTIMEOUT=10

precmd () {
  echo -n -e "\a"
}

# # Change cursor shape for different vi modes.
# function zle-keymap-select {
#   if [[ ${KEYMAP} == vicmd ]] ||
#      [[ $1 = 'block' ]]; then
#     echo -ne '\e[1 q'

#   elif [[ ${KEYMAP} == main ]] ||
#        [[ ${KEYMAP} == viins ]] ||
#        [[ ${KEYMAP} = '' ]] ||
#        [[ $1 = 'beam' ]]; then
#     echo -ne '\e[5 q'
#   fi
# }
# zle -N zle-keymap-select
# _fix_cursor() {
#    echo -ne '\e[5 q'
# }
# precmd_functions+=(_fix_cursor)


#   * To start using RVM you need to run `source /Users/yohanesbandung/.rvm/scripts/rvm`
#     in all your open shell windows, in rare cases you need to reopen all shell windows.
#   * To start using rails you need to run `rails new <project_dir>`.



alias config=$CONFTREE

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"
