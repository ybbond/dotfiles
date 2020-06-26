" For Vim
set nocompatible
set encoding=UTF-8

let s:plugin_location = '~/.vim/plugged'
if has('nvim')
  let s:plugin_location = '~/.local/share/nvim/plugged'
endif

" PlugIns
call plug#begin(s:plugin_location)
  Plug 'sainnhe/gruvbox-material'
    let g:gruvbox_material_enable_italic = 1

  " *fzf.vim*
  Plug '/usr/local/opt/fzf'
  Plug 'ybbond/fzf.vim'

  " *bufkill*
  Plug 'qpkorr/vim-bufkill'

  Plug 'junegunn/goyo.vim'
  Plug 'junegunn/limelight.vim'
    let g:limelight_conceal_ctermfg = 'gray'
    let g:limelight_conceal_ctermfg = 240

  " *vim-fugitive*
  Plug 'tpope/vim-fugitive'
  Plug 'tpope/vim-rhubarb'
  Plug 'mhinz/vim-signify'
    let g:signify_realtime = 1
    let g:signify_cursorhold_normal = 0
    let g:signify_cursorhold_insert = 0
  Plug 'shumphrey/fugitive-gitlab.vim'
    let g:fugitive_gitlab_domains = ['https://gitlab.kumparan.com']
  " *git-messenger*
  Plug 'rhysd/git-messenger.vim'
    let g:git_messenger_git_command = 'hub'
    let g:git_messenger_no_default_mappings = v:true

  " *vim-easymotion*
  " Plug 'easymotion/vim-easymotion'
  Plug 'justinmk/vim-sneak'

  " *vim-airline*
  Plug 'vim-airline/vim-airline'
    let g:airline#extensions#tabline#enabled = 1
    let g:airline#extensions#tabline#show_tabs = 0
    let g:airline#extensions#tabline#buffer_nr_show = 1
    let g:airline#extensions#tabline#formatter = 'jsformatter'

    let g:airline_section_x = []
    let g:airline_section_y = []

    let g:airline#extensions#coc#enabled = 1
    " use error & warning count of diagnostics form coc.nvim
    let g:airline_section_error = '%{airline#util#wrap(airline#extensions#coc#get_error(),0)}'
    let g:airline_section_warning = '%{airline#util#wrap(airline#extensions#coc#get_warning(),0)}'
    let g:airline#extensions#vimtex#enabled = 0
    let g:airline_theme = 'gruvbox_material'

  " *tmux*
  Plug 'tmux-plugins/vim-tmux'
  Plug 'tmux-plugins/vim-tmux-focus-events'

  " *nerdtree*
  Plug 'scrooloose/nerdtree'
    Plug 'Xuyuanp/nerdtree-git-plugin'

  Plug 'junegunn/vim-peekaboo'
  Plug 'machakann/vim-highlightedyank'
  Plug 'jiangmiao/auto-pairs'
    let g:AutoPairsMultilineClose = 0

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
    au BufNewFile,BufRead *.c setlocal tabstop=4
    au BufNewFile,BufRead *.c setlocal shiftwidth=4
    au BufNewFile,BufRead *.c setlocal set noexpandtab

  " *vim-clap*
  Plug 'liuchengxu/vim-clap', { 'do': ':Clap install-binary!' }
    let g:clap_provider_grep_delay = 100
    let g:clap_disable_run_rooter = v:true
    let g:clap_layout = { 'relative': 'editor' }

  " JavaScript
  Plug 'pangloss/vim-javascript'
    let g:javascript_plugin_flow = 1
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

  " Markdown
  Plug 'ybbond/vim-markdown'
    let g:vim_markdown_folding_disabled = 1
    let g:vim_markdown_conceal = 0
    let g:vim_markdown_strikethrough = 1
    let g:tex_conceal = ""
    let g:vim_markdown_math = 1
    let g:vim_markdown_frontmatter = 1  " for YAML format
    let g:vim_markdown_toml_frontmatter = 1  " for TOML format
    let g:vim_markdown_json_frontmatter = 1  " for JSON format
  Plug 'itspriddle/vim-marked'
  Plug 'godlygeek/tabular'

  Plug 'cespare/vim-toml'
  Plug 'stephpy/vim-yaml'

  " GRAPHQL
  Plug 'jparise/vim-graphql'

  " Go
  Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }

  " Rust
  Plug 'rust-lang/rust.vim'
    let g:rustfmt_autosave = 1

  " Reason
  Plug 'reasonml-editor/vim-reason-plus'
    autocmd BufNewFile,BufRead *.re nnoremap <leader>w :!refmt --in-place %<cr>

  " vlang
  Plug 'cheap-glitch/vim-v'
    let g:v_warnings=1
    au BufNewFile,BufRead *.v setlocal tabstop=4
    au BufNewFile,BufRead *.v setlocal shiftwidth=4
    au BufNewFile,BufRead *.v setlocal set noexpandtab

  " LaTex
  Plug 'lervag/vimtex'
    let g:vimtex_compiler_progname = 'nvr'
    let g:vimtex_view_method = 'skim'
    let g:vimtex_texcount_custom_arg = 'macro \cmt [state1]'

call plug#end()

colorscheme gruvbox-material
set background=dark
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
set scrolloff=3
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

" Let's save undo info!
if has('nvim')
  if has('persistent_undo')
    set undofile
    set undolevels=250
    set undoreload=500
  endif
else
  if !isdirectory($HOME."/.vim")
    call mkdir($HOME."/.vim", "", 0770)
  endif
  if !isdirectory($HOME."/.vim/undo-dir")
    call mkdir($HOME."/.vim/undo-dir", "", 0700)
  endif
  set undodir=~/.vim/undo-dir
  " Then set persistent undo!
  if has('persistent_undo')
    set undofile
    set undolevels=250
    set undoreload=500
  endif
endif

nnoremap <expr> j &wrap == 1 ? 'gj' : 'j'
nnoremap <expr> k &wrap == 1 ? 'gk' : 'k'
nnoremap <expr> $ &wrap == 1 ? 'g$' : '$'
nnoremap <expr> 0 &wrap == 1 ? 'g0' : '0'


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                      NORMAL MODE
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


" reload all opened buffer
nnoremap <leader>br :bufdo e<cr>

" Copy, Paste and Copy Whole File to clipboard
map <leader>c "+y<cr>
map <leader>v :r !pbpaste<cr><cr>
map <leader>ac :%w !pbcopy<cr><cr>

" Toggle wrap
map <leader>w :set wrap!<cr>

" Unhighlight search
nnoremap <silent> <LEADER><SPACE> :nohlsearch<cr>

" Toggle check spelling
nnoremap <leader>s :set spell! spelllang=en_us<cr>

" Marked toggle
nnoremap <leader>m :MarkedToggle!<cr>

map <silent> <A-h> <C-w><
map <silent> <A-k> <C-W>-
map <silent> <A-j> <C-W>+
map <silent> <A-l> <C-w>>

" Remap leader-% to source %
nnoremap <leader>% :source %<cr>

" navigating buffers
nnoremap gb :bnext<cr>
nnoremap gB :bprevious<cr>

" |vim-sneak|
  map f <Plug>Sneak_f
  map F <Plug>Sneak_F
  map t <Plug>Sneak_t
  map T <Plug>Sneak_T
  " " 2-character Sneak (default)
  " nmap Q <Plug>Sneak_s
  " nmap <leader>Q <Plug>Sneak_S
  " " visual-mode
  " xmap Q <Plug>Sneak_s
  " xmap <leader>Q <Plug>Sneak_S
  " " operator-pending-mode
  " omap Q <Plug>Sneak_s
  " omap <leader>Q <Plug>Sneak_S

" |vim-fugitive|
  map <leader>kp :Gdiff!<cr>
  map <leader>kb :Gblame<cr>

" |coc.nvim|
  nmap gd <Plug>(coc-definition)
  nmap <2-LeftMouse> <Plug>(coc-definition)
  nmap gh :call <SID>show_documentation()<cr>
  nmap <2-LeftMouse> :call <SID>show_documentation()<cr>
  nmap <leader>gd <Plug>(coc-diagnostics-info)
  nmap ]C <Plug>(coc-diagnostic-next)
  nmap [C <Plug>(coc-diagnostic-prev)
  nmap gi <Plug>(coc-implementation)
  nmap gr <Plug>(coc-references)
  nmap <leader>. :call CocAction("codeAction")<cr>

" |git-messenger|
  nmap <C-w>m <Plug>(git-messenger)
  nmap <C-w><C-m> <Plug>(git-messenger)

" |bufkill|
  nnoremap gx :BD<cr>

" |fzf.vim|
  let g:rg_command = '
    \ rg --column --line-number --no-heading --fixed-strings --ignore-case --hidden --follow --color "always"
    \ -g "*.{css,js,jsx,ts,tsx,json,re,php,md,styl,jade,html,config,py,cpp,c,go,hs,rb,conf}"
    \ -g "!{.git,node_modules,coverage,vendor,build}/*" '
  " :F for Ripgrep
  command! -bang -nargs=* Strings call fzf#vim#grep(g:rg_command .shellescape(<q-args>), 1, <bang>0)
  " nnoremap <C-p> <C-o>
  " nnoremap <C-o> :Strings<cr>
  " nnoremap <C-i> :Files<cr>

" |vim-clap|
  nnoremap <C-i> :Clap grep ++opt=--hidden ++opt=-g=!.git<cr>
  nnoremap <C-p> :Clap files --hidden<cr>
  nnoremap <leader>* :Clap grep ++query=<cword> ++opt=--hidden ++opt=-g=!.git<cr>
  nnoremap <C-b> :Clap buffers<cr>

" |nerdtree|
  map <leader>e :NERDTreeToggle<cr>
  map <leader>r :NERDTreeFind<cr>


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                      INSERT MODE
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Remap <c-f> to delete
inoremap <C-f> <Del>

" Remap <c-b> to delete within word
inoremap <C-b> <ESC>lcw

" Remap escape to j + k
inoremap jk <ESC>
inoremap <C-c> <ESC>

" |fzf.vim|
  inoremap <expr> <C-x><C-h> fzf#vim#complete#path_relative('rg --files')


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

function! s:goyo_enter()
  if executable('tmux') && strlen($TMUX)
    silent !tmux set status off
    silent !tmux list-panes -F '\#F' | grep -q Z || tmux resize-pane -Z
  endif
  set noshowcmd
  set wrap
  set nonumber
  set norelativenumber
  set eventignore=FocusGained,BufEnter
  set nocursorline
  Limelight
  :call ToggleNumberToggle(0)
endfunction

function! s:goyo_leave()
  if executable('tmux') && strlen($TMUX)
    silent !tmux set status on
    silent !tmux list-panes -F '\#F' | grep -q Z && tmux resize-pane -Z
  endif
  set showcmd
  set nowrap
  set number
  set relativenumber
  set eventignore=
  set cursorline
  Limelight!
  :call ToggleNumberToggle(1)
endfunction

autocmd! User GoyoEnter nested call <SID>goyo_enter()
autocmd! User GoyoLeave nested call <SID>goyo_leave()

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

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                      COLOR RELATED
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" TODO highlight
" FIXME highlight
" XXX highlight

" Change comment color
" hi Comment guifg=LightBlue
hi Comment gui=bold cterm=bold
hi htmlStrike gui=strikethrough cterm=strikethrough guibg=Black ctermbg=Black
hi Todo guibg=White ctermbg=White guifg=Black ctermfg=Black gui=bold,italic cterm=bold,italic
hi NonText guifg=#4a4a59 ctermfg=Gray
hi SpecialKey guifg=#4a4a59 ctermfg=Gray
hi SignColumn ctermbg=NONE cterm=NONE guibg=NONE gui=NONE
