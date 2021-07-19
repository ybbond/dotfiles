# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="spaceship"

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
    vi_mode
    char
  )

SPACESHIP_PROMPT_FIRST_PREFIX_SHOW=true
SPACESHIP_PROMPT_SEPARATE_LINE=false
SPACESHIP_BATTERY_SHOW='always'
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
SPACESHIP_VI_MODE_COLOR=grey
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
DISABLE_AUTO_TITLE="true"

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
plugins=(git zsh-autosuggestions zsh-syntax-highlighting history-substring-search vi-mode)

source $ZSH/oh-my-zsh.sh

# User configuration

########################################################################
##                     START OF BANDUNG's VI MODE CURSOR
########################################################################

# PLUGIN OPTIONS
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=#A4A4A4"
bindkey -M viins '^F' autosuggest-accept
bindkey -M viins '^P' history-substring-search-up
bindkey -M viins '^N' history-substring-search-down

bindkey -M viins 'jk' vi-cmd-mode

# vim mode config
# ---------------

# Remove mode switching delay.
KEYTIMEOUT=10

ZVM_VI_INSERT_ESCAPE_BINDKEY=jk

eval spaceship_vi_mode_enable

precmd () {
  echo -n -e "\a"
}

########################################################################
##                     END OF BANDUNG's VI MODE CURSOR
########################################################################


########################################################################
#                     START OF BANDUNG's ALIASES
########################################################################

# # git
# alias wip="git add . && git commit -m 'wip'"
# alias rehead="git reset HEAD~"

# alias cdb="cd .."

# # better cli
# alias lg="lazygit"
# alias ls='exa'
# alias cat="bat"
# alias find='fd'
# alias du='ncdu --color dark -rr'

alias set_dark="cp ~/.tmux-dark.conf ~/.tmux.conf && cp ~/.config/kitty/kitty-dark.conf ~/.config/kitty/kitty.conf"
alias set_light="cp ~/.tmux-light.conf ~/.tmux.conf && cp ~/.config/kitty/kitty-light.conf ~/.config/kitty/kitty.conf"

alias opsimulator="open -a Simulator"
alias opemulator="~/Library/Android/sdk/emulator/emulator -avd Pixel_3a_API_30_arm64-v8a -netdelay none -netspeed full"

alias neorc="cd ~/.config/nvim && nvim . && cd -"

########################################################################
#                     END OF BANDUNG's ALIASES
########################################################################


########################################################################
#                     START OF BANDUNG's eval and sources
########################################################################

eval "$(hub alias -s)"

# see alternative below
# eval "$(pyenv init -)"

# alternative, from old .zshrc
if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init -)"
fi

source <(lab completion zsh)

# pip zsh completion start
function _pip_completion {
  local words cword
  read -Ac words
  read -cn cword
  reply=( $( COMP_WORDS="$words[*]" \
             COMP_CWORD=$(( cword-1 )) \
             PIP_AUTO_COMPLETE=1 $words[1] 2>/dev/null ))
}
compctl -K _pip_completion pip
# pip zsh completion end

# pip3 zsh completion start
function _pip_completion {
  local words cword
  read -Ac words
  read -cn cword
  reply=( $( COMP_WORDS="$words[*]" \
             COMP_CWORD=$(( cword-1 )) \
             PIP_AUTO_COMPLETE=1 $words[1] 2>/dev/null ))
}
compctl -K _pip_completion pip3
# pip3 zsh completion end

###-begin-flutter-completion-###
if type complete &>/dev/null; then
  __flutter_completion() {
    local si="$IFS"
    IFS=$'\n' COMPREPLY=($(COMP_CWORD="$COMP_CWORD" \
                           COMP_LINE="$COMP_LINE" \
                           COMP_POINT="$COMP_POINT" \
                           flutter completion -- "${COMP_WORDS[@]}" \
                           2>/dev/null)) || return $?
    IFS="$si"
  }
  complete -F __flutter_completion flutter
elif type compdef &>/dev/null; then
  __flutter_completion() {
    si=$IFS
    compadd -- $(COMP_CWORD=$((CURRENT-1)) \
                 COMP_LINE=$BUFFER \
                 COMP_POINT=0 \
                 flutter completion -- "${words[@]}" \
                 2>/dev/null)
    IFS=$si
  }
  compdef __flutter_completion flutter
elif type compctl &>/dev/null; then
  __flutter_completion() {
    local cword line point words si
    read -Ac words
    read -cn cword
    let cword-=1
    read -l line
    read -ln point
    si="$IFS"
    IFS=$'\n' reply=($(COMP_CWORD="$cword" \
                       COMP_LINE="$line" \
                       COMP_POINT="$point" \
                       flutter completion -- "${words[@]}" \
                       2>/dev/null)) || return $?
    IFS="$si"
  }
  compctl -K __flutter_completion flutter
fi
###-end-flutter-completion-###

if type brew &>/dev/null; then
  FPATH=$(brew --prefix)/share/zsh/site-functions:$FPATH
fi

fpath=(~/.tool_binaries/completions $fpath)
autoload -U compinit && compinit

########################################################################
#                     START OF BANDUNG's eval and sources
########################################################################


########################################################################
##                     START OF BANDUNG's EXPORTS
#########################################################################

export EDITOR=nvim
export SHELL=zsh
export LANG=en_US.UTF-8
export TERM=screen-256color

# for pyenv to work correctly, I need to use brew version of OpenSSL
export LDFLAGS="-L/opt/homebrew/opt/openssl@1.1/lib"
export CPPFLAGS="-I/opt/homebrew/opt/openssl@1.1/include"

# export BAT_THEME="Monokai Extended Bright"
# export BAT_THEME="GitHub"
export BAT_THEME="Coldark-Dark"

export PATH="$HOME/.tool_binaries/zulu16.30.19-ca-jdk16.0.1-macosx_aarch64/zulu-16.jdk/Contents/Home/bin:$PATH"
export PATH="$PATH:$HOME/.tool_binaries/flutter/bin"

export JAVA_HOME="$HOME/.tool_binaries/zulu16.30.19-ca-jdk16.0.1-macosx_aarch64/zulu-16.jdk/Contents/Home"

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting. Make sure this is the last PATH variable change.

########################################################################
##                     END OF BANDUNG's EXPORTS
#########################################################################


#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"
