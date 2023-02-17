export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="spaceship"

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
SPACESHIP_DIR_PREFIX='['
SPACESHIP_DIR_SUFFIX=']'
SPACESHIP_CHAR_SYMBOL='λ'
SPACESHIP_CHAR_PREFIX=' '
SPACESHIP_CHAR_SUFFIX=' '
SPACESHIP_TIME_SHOW=true
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

plugins=(zsh-vi-mode zsh-autosuggestions zsh-syntax-highlighting history-substring-search lein)

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


alias set_dark="cp ~/.tmux-dark.conf ~/.tmux.conf && cp ~/.config/kitty/kitty-dark.conf ~/.config/kitty/kitty.conf"
alias set_light="cp ~/.tmux-light.conf ~/.tmux.conf && cp ~/.config/kitty/kitty-light.conf ~/.config/kitty/kitty.conf"

alias opsimulator="open -a Simulator"
alias opemulator="~/Library/Android/sdk/emulator/emulator -avd Pixel_3a_API_32 -netdelay none -netspeed full"

alias neorc="cd ~/.config/nvim && nvim . || cd -"

alias flut="fvm flutter"

alias bffrun="source ./local_env.sh && ./tools/run.sh"
alias bfftest="source ./local_env.sh && make test > result.txt"

alias quicklisp="sbcl --eval '(load #P\"~/.quicklisp/setup.lisp\")'"

eval "$(hub alias -s)"


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

    nnn -A "$@"

    if [ -f "$NNN_TMPFILE" ]; then
            . "$NNN_TMPFILE"
            rm -f "$NNN_TMPFILE" > /dev/null
    fi
}


export EDITOR=nvim
export SHELL=zsh
export LANG=en_US.UTF-8
# export TERM=xterm-256color
export TERM=screen-256color

# export BAT_THEME="Monokai Extended Bright"
# export BAT_THEME="GitHub"
export BAT_THEME="Coldark-Dark"

eval "$(frum init)"

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
