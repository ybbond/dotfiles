fpath+=${ZSH_CUSTOM:-${ZSH:-~/.oh-my-zsh}/custom}/plugins/zsh-completions/src
export ZSH="$HOME/.oh-my-zsh"

## https://www.soberkoder.com/better-zsh-history/
# setopt INC_APPEND_HISTORY
# export HISTTIMEFORMAT="[%F %T] "
# setopt EXTENDED_HISTORY
setopt HIST_FIND_NO_DUPS
# export HISTFILESIZE=10000000
export HISTSIZE=10000000

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

# color reference https://upload.wikimedia.org/wikipedia/commons/1/15/Xterm_256color_chart.svg
SPACESHIP_PROMPT_FIRST_PREFIX_SHOW=true
SPACESHIP_PROMPT_SEPARATE_LINE=false
SPACESHIP_BATTERY_SHOW='always'
SPACESHIP_BATTERY_PREFIX='['
SPACESHIP_BATTERY_SUFFIX=']'
SPACESHIP_BATTERY_THRESHOLD=100
SPACESHIP_GIT_PREFIX='['
SPACESHIP_GIT_SUFFIX=']'
SPACESHIP_GIT_BRANCH_PREFIX='•'
SPACESHIP_GIT_BRANCH_SUFFIX='•'
SPACESHIP_GIT_STATUS_PREFIX='<'
SPACESHIP_GIT_STATUS_SUFFIX='>'
[[ $(dark-notify -e) = 'light' ]] && SPACESHIP_DIR_COLOR=blue || SPACESHIP_CHAR_COLOR=cyan
SPACESHIP_DIR_PREFIX='['
SPACESHIP_DIR_SUFFIX=']'
SPACESHIP_CHAR_SYMBOL='λ'
[[ $(dark-notify -e) = 'light' ]] && SPACESHIP_CHAR_COLOR=022 || SPACESHIP_CHAR_COLOR=046 # 028=darkgreen 046=green
SPACESHIP_CHAR_PREFIX=' '
SPACESHIP_CHAR_SUFFIX=' '
SPACESHIP_TIME_SHOW=true
[[ $(dark-notify -e) = 'light' ]] && SPACESHIP_TIME_COLOR=202 || SPACESHIP_TIME_COLOR=226 # 202=orange 226=yellow
SPACESHIP_TIME_PREFIX='['
SPACESHIP_TIME_SUFFIX=']'

function zvm_config() {
  ZVM_VI_INSERT_ESCAPE_BINDKEY=jk
}
function zvm_after_init() {
  bindkey -M viins '^F' autosuggest-accept
  bindkey -M viins '^P' history-substring-search-up
  bindkey -M viins '^N' history-substring-search-down

  # bindkey -M viins 'jk' vi-cmd-mode
}

ZSH_THEME="spaceship"

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

# Uncomment one of the following lines to change the auto-update behavior
# zstyle ':omz:update' mode disabled  # disable automatic updates
# zstyle ':omz:update' mode auto      # update automatically without asking
# zstyle ':omz:update' mode reminder  # just remind me to update when it's time

# Uncomment the following line to change how often to auto-update (in days).
# zstyle ':omz:update' frequency 13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS="true"

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# You can also set it to another string to have that shown instead of the default red dots.
# e.g. COMPLETION_WAITING_DOTS="%F{yellow}waiting...%f"
# Caution: this setting can cause issues with multiline prompts in zsh < 5.7.1 (see #5765)
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
plugins=(zsh-vi-mode zsh-autosuggestions zsh-syntax-highlighting history-substring-search spaceship-vi-mode rustup)

DISABLE_MAGIC_FUNCTIONS=true
DISABLE_AUTO_TITLE=true
RPS1="%{$reset_color%}"
source $ZSH/oh-my-zsh.sh
ZSH_THEME_TERM_TITLE_IDLE='%n@%m:%~'

ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=#A4A4A4"

KEYTIMEOUT=10

eval spaceship_vi_mode_enable

precmd () {
  echo -n -e "\a"
}

if type brew &>/dev/null; then
  FPATH=$(brew --prefix)/share/zsh/site-functions:$FPATH
fi

fpath=(~/.tool_binaries/completions $fpath)
autoload -U compinit && compinit

alias neorc="cd ~/.config/nvim && nvim . && cd -"
alias start_swank="ros run --eval '(ql:quickload :swank)'  --eval '(swank:create-server :dont-close t)'"
alias flutter="fvm flutter"
alias dart="fvm dart"

# User configuration

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

## [Completion]
## Completion scripts setup. Remove the following line to uninstall
[[ -f /Users/yohanesbandung/.dart-cli-completion/zsh-config.zsh ]] && . /Users/yohanesbandung/.dart-cli-completion/zsh-config.zsh || true
## [/Completion]


export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"

export GOPATH="$HOME/go"; export GOROOT="$HOME/.go"; export PATH="$GOPATH/bin:$PATH"; # g-install: do NOT edit, see https://github.com/stefanmaric/g
