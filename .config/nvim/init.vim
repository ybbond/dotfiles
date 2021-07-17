" For Vim
set nocompatible
set encoding=UTF-8

let s:plugin_location = '~/.vim/plugged'
if has('nvim')
  let s:plugin_location = '~/.local/share/nvim/plugged'
endif

" PlugIns
call plug#begin(s:plugin_location)
  Plug 'mhartington/oceanic-next'

  " *nvim-web-devicons*
  Plug 'kyazdani42/nvim-web-devicons'

  " *telescope.nvim*
  Plug 'nvim-lua/popup.nvim'
  Plug 'nvim-lua/plenary.nvim'
  Plug 'nvim-telescope/telescope.nvim'

  " *barbar.nvim*
  Plug 'romgrk/barbar.nvim'
    let bufferline = get(g:, 'bufferline', {})
    " let bufferline.icons = 'numbers'

  " *galaxyline.nvim*
  Plug 'glepnir/galaxyline.nvim' , {'branch': 'main'}

  " *nvim-tree*
  Plug 'kyazdani42/nvim-tree.lua'
    let g:nvim_tree_side = 'right'
    let g:nvim_tree_width = 40

  " *nvim-treesitter*
  " https://github.com/nvim-treesitter/nvim-treesitter#supported-languages
  " :TSInstall {language}
  Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}

  " *vim-fugitive*
  Plug 'tpope/vim-fugitive'
  Plug 'tpope/vim-rhubarb'
  Plug 'shumphrey/fugitive-gitlab.vim'

  " *git-messenger*
  Plug 'rhysd/git-messenger.vim'
    let g:git_messenger_git_command = 'hub'
    let g:git_messenger_no_default_mappings = v:true

  " *vim-sneak*
  Plug 'justinmk/vim-sneak'
    let g:sneak#absolute_dir = 1

  " *tmux*
  Plug 'tmux-plugins/vim-tmux'
  Plug 'tmux-plugins/vim-tmux-focus-events'

  Plug 'junegunn/vim-peekaboo'
  Plug 'machakann/vim-highlightedyank'

  Plug 'tpope/vim-commentary'
  Plug 'tpope/vim-surround'
  Plug 'tpope/vim-repeat'

  " *coc.nvim*
  Plug 'neoclide/coc.nvim', {'branch': 'release'}
    function! s:show_documentation()
      if &filetype == 'vim'
        execute 'h '.expand('<cword>')
      else
        call CocAction('doHover')
      endif
    endfunction
    au BufNewFile,BufRead *.(c|v|vv|py) setlocal tabstop=4
    au BufNewFile,BufRead *.(c|v|vv|py) setlocal shiftwidth=4
    au BufNewFile,BufRead *.(c|v|vv|py) setlocal set noexpandtab

  " Dart
  Plug 'dart-lang/dart-vim-plugin'
    let g:dart_format_on_save = 1

  " JavaScript
  Plug 'pangloss/vim-javascript'
    " let g:javascript_plugin_flow = 1
    let g:javascript_plugin_jsdoc = 1
  Plug 'othree/yajs.vim'
  Plug 'maxmellon/vim-jsx-pretty'
    let g:vim_jsx_pretty_colorful_config = 1
    let g:jsx_ext_required = 0

  " styled-components
  Plug 'styled-components/vim-styled-components', { 'branch': 'main' }

  " TypeScript
  Plug 'leafgarland/typescript-vim'
  Plug 'HerringtonDarkholme/yats.vim'

  Plug 'cespare/vim-toml'
  Plug 'stephpy/vim-yaml'

  " GRAPHQL
  Plug 'jparise/vim-graphql'

call plug#end()

colorscheme OceanicNext
set background=dark

" colorscheme xcodelighthc
" set background=light

set termguicolors

syntax enable
syntax on
filetype plugin on
filetype plugin indent on

set nopaste
set pastetoggle=<F2>

set tabstop=2
set softtabstop=2
set shiftwidth=2
set scrolloff=2
set expandtab
set nowrap
set linebreak
set list
set listchars=tab:▷\ ,trail:◻,nbsp:𐩒

set mouse=n
set noshowmode

set number
set relativenumber

set ruler
set showmatch
set cursorline

set ignorecase
set smartcase
set incsearch
set hlsearch

if has('nvim')
  set inccommand=split
endif

nnoremap <expr> j &wrap == 1 ? 'gj' : 'j'
nnoremap <expr> k &wrap == 1 ? 'gk' : 'k'
nnoremap <expr> $ &wrap == 1 ? 'g$' : '$'
nnoremap <expr> 0 &wrap == 1 ? 'g0' : '0'


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                      NORMAL MODE
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" keep asterisk and pound to be case sensitive
nnoremap <leader>* :let @/='\C\<' . expand('<cword>') . '\>'<CR>:let v:searchforward=1<CR>n
nnoremap <leader># :let @/='\C\<' . expand('<cword>') . '\>'<CR>:let v:searchforward=0<CR>n

" reload all opened buffer
nnoremap <leader>br :bufdo e<CR>

" Copy, Paste and Copy Whole File to clipboard
map <leader>c "+y
map <leader>v :r !pbpaste<CR><CR>
map <leader>x :%w !pbcopy<CR><CR>

" Toggle wrap
map <leader>w :set wrap!<CR>

" Unhighlight search
nnoremap <silent> <LEADER><SPACE> :nohlsearch<CR>

" Toggle check spelling
nnoremap <leader>s :set spell! spelllang=en_us<CR>

map <silent> <A-h> <C-w><
map <silent> <A-k> <C-W>-
map <silent> <A-j> <C-W>+
map <silent> <A-l> <C-w>>

" Remap leader-% to source %
nnoremap <leader>% :source ~/.config/nvim/init.vim<CR>

" |vim-sneak|
  map f <Plug>Sneak_f
  map F <Plug>Sneak_F
  map t <Plug>Sneak_t
  map T <Plug>Sneak_T

" |vim-fugitive|
  map <leader>kp :Gdiff!<CR>
  map <leader>kb :Git blame<CR>

" |coc.nvim|
  nmap gd <Plug>(coc-definition)
  nmap <2-LeftMouse> <Plug>(coc-definition)
  nmap gh :call <SID>show_documentation()<CR>
  nmap <2-LeftMouse> :call <SID>show_documentation()<CR>
  nmap <leader>gd <Plug>(coc-diagnostics-info)
  nmap [c <Plug>(coc-git-prevchunk)
  nmap ]c <Plug>(coc-git-nextchunk)
  nmap <C-w>g <Plug>(coc-git-chunkinfo)
  nmap <C-w><C-g> <Plug>(coc-git-chunkinfo)
  nmap ]C <Plug>(coc-diagnostic-next)
  nmap [C <Plug>(coc-diagnostic-prev)
  nmap gi <Plug>(coc-implementation)
  nmap gr <Plug>(coc-references)
  nmap ga :call CocAction("codeAction")<CR>
  xmap gs  <Plug>(coc-codeaction-selected)
  nmap gs  <Plug>(coc-codeaction-selected)

  command! CocGstatus CocList gstatus
  command! CocBuffers CocList buffers
  command! W noa w

" |git-messenger|
  nmap <C-w>m <Plug>(git-messenger)
  nmap <C-w><C-m> <Plug>(git-messenger)

" |telescope.nvim|
  nnoremap <C-p> <CMD>Telescope find_files<CR>
  nnoremap <C-i> <CMD>Telescope live_grep<CR>
  nnoremap <C-s> <CMD>Telescope grep_string<CR>
  nnoremap <C-g> <CMD>Telescope git_status<CR>
  nnoremap <C-t> <CMD>Telescope treesitter<CR>

" |barbar.nvim|
  " Move to previous/next
  nnoremap    gB    :BufferPrevious<CR>
  nnoremap    gb    :BufferNext<CR>
  " Re-order to previous/next
  nnoremap    <A-<> :BufferMovePrevious<CR>
  nnoremap    <A->> :BufferMoveNext<CR>
  " Goto buffer in position...
  nnoremap    <A-1> :BufferGoto 1<CR>
  nnoremap    <A-2> :BufferGoto 2<CR>
  nnoremap    <A-3> :BufferGoto 3<CR>
  nnoremap    <A-4> :BufferGoto 4<CR>
  nnoremap    <A-5> :BufferGoto 5<CR>
  nnoremap    <A-6> :BufferGoto 6<CR>
  nnoremap    <A-7> :BufferGoto 7<CR>
  nnoremap    <A-8> :BufferGoto 8<CR>
  nnoremap    <A-9> :BufferLast<CR>
  " Close buffer
  nnoremap    gx    :BufferClose<CR>
  " Wipeout buffer
  "                 :BufferWipeout<CR>
  " Close commands
  "                 :BufferCloseAllButCurrent<CR>
  "                 :BufferCloseBuffersLeft<CR>
  "                 :BufferCloseBuffersRight<CR>
  " Magic buffer-picking mode
  nnoremap    <A-s> :BufferPick<CR>

" |nvim-tree|
  nnoremap <LEADER>e :NvimTreeToggle<CR>


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                      INSERT MODE
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Remap <c-d> to delete
inoremap <C-d> <Del>

" Remap escape to j + k
inoremap jk <ESC>
inoremap <C-c> <ESC>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                      APPEARANCE
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

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

" change cursor in different mode
if has('nvim')
  " make the ^[ sign with:
  " (INSERT MODE) <C-v><ESC>
  " make sure it shows different color than the rest
  set t_SI=[5\ q
  set t_SR=[4\ q
  set t_EI=[1\ q
else
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
      autocmd VimLeave * silent !echo -ne "\033[0 q"
  endif
endif

" |vim-fugitive|
set diffopt+=vertical

" |vimdiff|
" au VimEnter * if &diff | execute 'windo set wrap' | execute 'windo set nofoldenable' | endif
if &diff
  set wrap
  set nofoldenable
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


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                      COLOR RELATED
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" TODO highlight
" FIXME highlight
" XXX highlight

" Identify syntax highlighting below cursor
map <LEADER>h :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
\ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
\ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>

noremap <leader>a ga

" Change comment color
" hi Comment guifg=LightBlue
hi Comment gui=italic cterm=italic
" hi htmlStrike gui=strikethrough cterm=strikethrough guibg=Black ctermbg=Black
" hi Todo guibg=Black ctermbg=Black guifg=White ctermfg=White gui=bold,italic cterm=bold,italic
" hi NonText guifg=#4a4a59 ctermfg=Gray
" hi SpecialKey guifg=#4a4a59 ctermfg=Gray
hi SignColumn ctermbg=NONE cterm=NONE guibg=NONE gui=NONE

hi DiffAdded ctermbg=22 guibg=#006c00
hi DiffRemoved ctermbg=94 guibg=#990006
