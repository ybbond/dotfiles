syntax enable
syntax on
filetype plugin indent on

set background=dark
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
nnoremap gb :bnext<CR>
nnoremap gB :bprevious<CR>

augroup numbertoggle
  if &number == 1
    autocmd!
    autocmd BufEnter,FocusGained,InsertLeave * set relativenumber
    autocmd BufLeave,FocusLost,InsertEnter * set norelativenumber
  endif
augroup end

function ToggleNumberToggle(numberVar)
  " reset group
  augroup numbertoggle
    autocmd!
  augroup end

  " enable if toggled on
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
