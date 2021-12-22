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

# plugins=(git zsh-autosuggestions zsh-syntax-highlighting history-substring-search vi-mode)
plugins=(zsh-autosuggestions zsh-syntax-highlighting history-substring-search vi-mode)

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
alias opemulator="~/Library/Android/sdk/emulator/emulator -avd Pixel_3a_API_31 -netdelay none -netspeed full"

alias neorc="cd ~/.config/nvim && nvim . && cd -"

alias flut="fvm flutter"

########################################################################
#                     END OF BANDUNG's ALIASES
########################################################################


########################################################################
#                     START OF BANDUNG's eval and sources
########################################################################

eval "$(hub alias -s)"

####-begin-flutter-completion-###
#if type complete &>/dev/null; then
#  __flutter_completion() {
#    local si="$IFS"
#    IFS=$'\n' COMPREPLY=($(COMP_CWORD="$COMP_CWORD" \
#                           COMP_LINE="$COMP_LINE" \
#                           COMP_POINT="$COMP_POINT" \
#                           flutter completion -- "${COMP_WORDS[@]}" \
#                           2>/dev/null)) || return $?
#    IFS="$si"
#  }
#  complete -F __flutter_completion flutter
#elif type compdef &>/dev/null; then
#  __flutter_completion() {
#    si=$IFS
#    compadd -- $(COMP_CWORD=$((CURRENT-1)) \
#                 COMP_LINE=$BUFFER \
#                 COMP_POINT=0 \
#                 flutter completion -- "${words[@]}" \
#                 2>/dev/null)
#    IFS=$si
#  }
#  compdef __flutter_completion flutter
#elif type compctl &>/dev/null; then
#  __flutter_completion() {
#    local cword line point words si
#    read -Ac words
#    read -cn cword
#    let cword-=1
#    read -l line
#    read -ln point
#    si="$IFS"
#    IFS=$'\n' reply=($(COMP_CWORD="$cword" \
#                       COMP_LINE="$line" \
#                       COMP_POINT="$point" \
#                       flutter completion -- "${words[@]}" \
#                       2>/dev/null)) || return $?
#    IFS="$si"
#  }
#  compctl -K __flutter_completion flutter
#fi
####-end-flutter-completion-###

if type brew &>/dev/null; then
  FPATH=$(brew --prefix)/share/zsh/site-functions:$FPATH
fi

fpath=(~/.tool_binaries/completions $fpath)
autoload -U compinit && compinit


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
    export NNN_TMPFILE="${XDG_CONFIG_HOME:-$HOME/.config}/nnn/.lastd"

    # Unmask ^Q (, ^V etc.) (if required, see `stty -a`) to Quit nnn
    # stty start undef
    # stty stop undef
    # stty lwrap undef
    # stty lnext undef

    nnn "$@"

    if [ -f "$NNN_TMPFILE" ]; then
            . "$NNN_TMPFILE"
            rm -f "$NNN_TMPFILE" > /dev/null
    fi
}

########################################################################
#                     START OF BANDUNG's eval and sources
########################################################################


########################################################################
##                     START OF BANDUNG's EXPORTS
#########################################################################

export EDITOR=nvim
export SHELL=zsh
export LANG=en_US.UTF-8
export TERM=xterm-256color
#  export TERM=screen-256color

# export BAT_THEME="Monokai Extended Bright"
# export BAT_THEME="GitHub"
export BAT_THEME="Coldark-Dark"

export PATH="$HOME/.tool_binaries/zulu17.30.15-ca-jdk17.0.1-macosx_aarch64/zulu-17.jdk/Contents/Home:$PATH"
export JAVA_HOME="$HOME/.tool_binaries/zulu17.30.15-ca-jdk17.0.1-macosx_aarch64/zulu-17.jdk/Contents/Home"

export PATH="$PATH:$HOME/fvm/default/bin"
export PATH="$PATH:$HOME/.pub-cache/bin"
# export PATH="$PATH:$HOME/flutter/bin"

export PATH="/opt/homebrew/bin:$PATH"

## for llvm homebrew
# export PATH="/opt/homebrew/opt/llvm/bin:$PATH"
# # export LDFLAGS="-L/opt/homebrew/opt/llvm/lib" # for compilers to find llvm
# export LDFLAGS="-L/opt/homebrew/opt/llvm/lib -Wl,-rpath,/opt/homebrew/opt/llvm/lib" # to use built-in libc++
# export CPPFLAGS="-I/opt/homebrew/opt/llvm/include" # for compilers to find llvm

## for llvm homebrew, modified
# export LDFLAGS="-L/opt/homebrew/lib -Wl,-rpath,/opt/homebrew/lib" # to use built-in libc++
# export CPPFLAGS="-I/opt/homebrew/include" # for compilers to find llvm
# export CFLAGS="-I/opt/homebrew/include" # for compilers to find llvm
# export CPLUS_INCLUDE_PATH="$CPLUS_INCLUDE_PATH:/opt/homebrew/include"

# for built-in clangd
# export CLANGD_FLAGS="-I/opt/homebrew/include"

#TEMP
#export CPATH="/opt/homebrew/include"
#export LIBRARY_PATH="/opt/homebrew/lib"

# export C_INCLUDE_DIRS="/opt/homebrew/include"
# export CLANG_LIBS="/opt/homebrew/lib"

export GO111MODULE=on
export GOPRIVATE="gitlab.com/pinvest/*"
export GOPATH="$HOME/go"; export GOROOT="$HOME/.go"; export PATH="$GOPATH/bin:$PATH"; # g-install: do NOT edit, see https://github.com/stefanmaric/g
# export GOPROXY=https://proxy.golang.org

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting. Make sure this is the last PATH variable change.

########################################################################
##                     END OF BANDUNG's EXPORTS
#########################################################################
