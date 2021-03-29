# If you come from bash you might have to change your $PATH.
export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="/Users/yohanesbandung/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="spaceship"
# ZSH_THEME="robbyrussell"

########################################################################
#                     START OF SPACESHIP
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
    # vi_mode
    char
  )
SPACESHIP_PROMPT_FIRST_PREFIX_SHOW=true
SPACESHIP_PROMPT_SEPARATE_LINE=false
SPACESHIP_BATTERY_SHOW=always
SPACESHIP_BATTERY_PREFIX='['
SPACESHIP_BATTERY_SUFFIX=']'
SPACESHIP_BATTERY_THRESHOLD=100
# SPACESHIP_TIME_COLOR='#fcec02'
SPACESHIP_GIT_PREFIX='['
SPACESHIP_GIT_SUFFIX=']'
# SPACESHIP_GIT_BRANCH_COLOR='#00a0e4'
SPACESHIP_GIT_BRANCH_PREFIX='•'
SPACESHIP_GIT_BRANCH_SUFFIX='•'
# SPACESHIP_GIT_STATUS_COLOR='#da2c20'
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

########################################################################
#                     END OF SPACESHIP
########################################################################

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in $ZSH/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to automatically update without prompting.
# DISABLE_UPDATE_PROMPT="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS="true"

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# Caution: this setting can cause issues with multiline prompts (zsh 5.7.1 and newer seem to work)
# See https://github.com/ohmyzsh/ohmyzsh/issues/5765
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git npm yarn zsh-autosuggestions zsh-syntax-highlighting history-substring-search vi-mode)

source $ZSH/oh-my-zsh.sh

# User configuration

########################################################################
#                     START OF BANDUNG's EXPORTS
########################################################################

export EDITOR=nvim
export SHELL=zsh
export LANG=en_US.UTF-8
export TERM=screen-256color

# export PATH=$HOME/bin:/usr/local/bin:$PATH
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

# export BAT_THEME="Monokai Extended Bright"
export BAT_THEME="GitHub"

export ANDROID_HOME=${HOME}/Library/Android/sdk
export JAVA_HOME=/Applications/Android\ Studio.app/Contents/jre/jdk/Contents/Home
export PATH=${PATH}:${ANDROID_HOME}/emulator
export PATH=${PATH}:${ANDROID_HOME}/tool
export PATH="${PATH}:$ANDROID_HOME/tools:$ANDROID_HOME/platform-tools"

export SCRIPT=~/Library/Mobile\ Documents/com\~apple\~ScriptEditor2/Documents

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"

export N_PREFIX="$HOME/n"; [[ :$PATH: == *":$N_PREFIX/bin:"* ]] || PATH+=":$N_PREFIX/bin"  # Added by n-install (see http://git.io/n-install-repo).

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

source /Users/yohanesbandung/Library/Preferences/org.dystroy.broot/launcher/bash/br

# PLUGIN OPTIONS
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=#A4A4A4"
bindkey -M viins '^F' autosuggest-accept
bindkey -M viins '^P' history-substring-search-up
bindkey -M viins '^N' history-substring-search-down


########################################################################
#                     END OF BANDUNG's EXPORTS
########################################################################

########################################################################
#                     START OF BANDUNG's FUNCTIONS
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

nn ()
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
#                     END OF BANDUNG's FUNCTIONS
########################################################################

########################################################################
#                     START OF BANDUNG's ALIASES
########################################################################

eval "$(hub alias -s)"

# projects
alias kapp="cd ${KUMPATH}/app"
alias kweb="cd ${KUMPATH}/web"
alias kmag="cd ${KUMPATH}/dashboard"
alias uikit="cd ${KUMPATH}/js/packages/kumparan-uikit"

alias ybbond="cd ${YBPATH}/ybbond"

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
# alias pixelxl="~/Library/Android/sdk/emulator/emulator -avd Pixel_XL_API_29 -netdelay none -netspeed full"
# alias pixel4="~/Library/Android/sdk/emulator/emulator -avd Pixel_4_API_29 -netdelay none -netspeed full"
# alias pixel3a="~/Library/Android/sdk/emulator/emulator -avd Pixel_3a_API_28 -netdelay none -netspeed full"
alias nexus5x="~/Library/Android/sdk/emulator/emulator -avd Nexus_5X_API_25 -netdelay none -netspeed full"
alias konak="kantal 10 yarn add"

alias cdb="cd .."

# better cli
alias ls='exa'
# alias top='btm --color=default-light'
# alias cat="bat --theme=\$(defaults read -globalDomain AppleInterfaceStyle &> /dev/null && echo default || echo GitHub)"
alias top='btm'
alias cat="bat"
alias find='fd'
alias du='ncdu --color dark -rr'

source <(lab completion zsh)

########################################################################
#                     END OF BANDUNG's ALIASES
########################################################################


########################################################################
#                     START OF BANDUNG's VI MODE CURSOR
########################################################################

# vim mode config
# ---------------

# Activate vim mode.
bindkey -v
bindkey -M viins 'jk' vi-cmd-mode

# Remove mode switching delay.
KEYTIMEOUT=10

# Change cursor shape for different vi modes.
function zle-keymap-select {
  if [[ ${KEYMAP} == vicmd ]] ||
     [[ $1 = 'block' ]]; then
    echo -ne '\e[1 q'

  elif [[ ${KEYMAP} == main ]] ||
       [[ ${KEYMAP} == viins ]] ||
       [[ ${KEYMAP} = '' ]] ||
       [[ $1 = 'beam' ]]; then
    echo -ne '\e[5 q'
  fi
}
zle -N zle-keymap-select

_fix_cursor() {
   echo -ne '\e[5 q'
}

precmd_functions+=(_fix_cursor)

precmd () {
  echo -n -e "\a"
}

########################################################################
#                     END OF BANDUNG's VI MODE CURSOR
########################################################################

alias config=$CONFTREE

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
