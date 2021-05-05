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
nnoremap j gj
nnoremap k gk

inoremap jk <ESC>
inoremap <C-D> <DEL>

" navigating buffers
nnoremap gb :bnext<cr>
nnoremap gB :bprevious<cr>

augroup numbertoggle
  if &number == 1
    autocmd!
    autocmd BufEnter,FocusGained,InsertLeave * set relativenumber
    autocmd BufLeave,FocusLost,InsertEnter * set norelativenumber
  endif
augroup end

function ToggleNumberToggle(numberVar)
  " Reset group
  augroup numbertoggle
    autocmd!
  augroup end

  " Enable if toggled on
  if a:numberVar
    augroup numbertoggle
      if &number == 1
        autocmd!
        autocmd BufEnter,FocusGained,InsertLeave * set relativenumber
        autocmd BufLeave,FocusLost,InsertEnter * set norelativenumber
      endif
    augroup end
  endif
endfunction

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

set rtp+=/usr/local/opt/fzf
