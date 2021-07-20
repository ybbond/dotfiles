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

set hidden

nnoremap <LEADER><SPACE> :nohlsearch<CR>
nnoremap <LEADER>% :source %<CR>

nnoremap j gj
nnoremap k gk

inoremap jk <ESC>
inoremap <C-D> <DEL>

" navigating buffers
nnoremap gb :bnext<CR>
nnoremap gB :bprevious<CR>

au VimEnter * if &diff | execute 'windo set wrap' | execute 'windo set nofoldenable' | endif

if &diff
  nnoremap gr :diffupdate<CR>
  " hi DiffAdd    ctermfg=233 ctermbg=LightGreen guifg=#003300 guibg=#DDFFDD gui=none cterm=none
  " hi DiffChange ctermbg=white  guibg=#ececec gui=none   cterm=none
  " hi DiffText   ctermfg=233  ctermbg=yellow  guifg=#000033 guibg=#DDDDFF gui=none cterm=none
  hi DiffAdd    ctermbg=22
  hi DiffChange ctermbg=94
  hi DiffDelete ctermbg=88
  hi DiffText   ctermfg=233  ctermbg=yellow  guifg=#000033 guibg=#DDDDFF gui=none cterm=none
endif

if &diff
  map gs :call IwhiteToggle()<CR>
  function! IwhiteToggle()
    if &diffopt =~ 'iwhite'
      set diffopt-=iwhite
    else
      set diffopt-=internal
      set diffopt+=iwhite
    endif
  endfunction
endif

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
