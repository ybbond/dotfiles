syntax enable
syntax on
filetype plugin indent on

set background=light
set number
set relativenumber

set tabstop=2
set softtabstop=2
set shiftwidth=2
set expandtab

set laststatus=2

set ruler

set incsearch
set showmatch
set ignorecase
set smartcase
set hlsearch

nnoremap <LEADER><SPACE> :nohlsearch<CR>
nnoremap <LEADER>% :source %<CR>
inoremap jk <ESC>
nnoremap j gj
nnoremap k gk
vnoremap j gj
vnoremap k gk

if !isdirectory($HOME."/.vim")
  call mkdir($HOME."/.vim", "", 0770)
endif
if !isdirectory($HOME."/.vim/undo-dir")
  call mkdir($HOME."/.vim/undo-dir", "", 0770)
endif
set undodir=~/.vim/undo-dir
if has ('persistent_undo')
  set undofile
  set undolevels=250
  set undoreload=500
endif

if exists('$TMUX')
  " tmux will only forward escape sequences to the terminal if surrounded by a DCS sequence
  let &t_SI .= "\<Esc>Ptmux;\<Esc>\<Esc>[5 q\<Esc>\\"
  let &t_SR .= "\<Esc>Ptmux;\<Esc>\<Esc>[4 q\<Esc>\\"
  let &t_EI .= "\<Esc>Ptmux;\<Esc>\<Esc>[1 q\<Esc>\\"
  autocmd VimLeave * silent !echo -ne "\033Ptmux;\033\033[0 q\033\\"
else
  " make the ^[ sign with:
  " (INSERT MODE) <C-v><ESC>
  " make sure it shows different color than the rest
  set t_SI=[5\ q
  set t_SR=[4\ q
  set t_EI=[1\ q
endif

set rtp+=/usr/local/opt/fzf
