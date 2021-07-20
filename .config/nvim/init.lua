------------------------------------------------------------
--                      RESOURCES
------------------------------------------------------------

require('plugins')

require('plenary.reload').reload_module('plugins')
require('plenary.reload').reload_module('configs')
require('plenary.reload').reload_module'%'


------------------------------------------------------------
--                    CUSTOMIZATIONS
------------------------------------------------------------

vim.o.compatible = false
vim.o.encoding = 'UTF-8'

vim.o.termguicolors = true
vim.o.background = 'dark'

vim.cmd('colorscheme OceanicNext')
vim.cmd('syntax enable')
vim.cmd('syntax on')
vim.cmd('filetype plugin on')
vim.cmd('filetype plugin indent on')

vim.o.paste = false
vim.o.pastetoggle = '<F2>'

vim.o.hidden = true

-- vim.o.foldmethod = 'expr'
-- vim.o.foldexpr = 'nvim_treesitter#foldexpr()'

vim.o.tabstop = 2
vim.o.softtabstop = 2
vim.o.shiftwidth = 2
vim.o.scrolloff = 2
vim.o.expandtab = true
vim.o.wrap = false
vim.o.linebreak = true
vim.o.list = true
vim.o.listchars = [[tab:▷\ ,trail:◻,nbsp:𐩒]]

vim.o.mouse = 'n'
vim.o.showmode = false

vim.o.number = true
vim.o.relativenumber = true

vim.o.ruler = true
vim.o.showmatch = true
vim.o.cursorline = true

vim.o.ignorecase = true
vim.o.smartcase = true
vim.o.incsearch = true
vim.o.hlsearch = true

vim.o.inccommand = 'split'

vim.o.completeopt = "menuone,noinsert,noselect"


------------------------------------------------------------
--                   DEFAULT KEYBINDINGS
------------------------------------------------------------

-- https://oroques.dev/notes/neovim-init/#mappings
-- local function map(mode, lhs, rhs, opts)
--   local options = {noremap = true}
--   if opts then options = vim.tbl_extend('force', options, opts) end
--   vim.api.nvim_set_keymap(mode, lhs, rhs, options)
-- end
--
-- map('', '<leader>c', '"+y') 

local function t(str) return vim.api.nvim_replace_termcodes(str, true, true, true) end

function _G.smart_wrap_nav_bindings(ifTrue,ifFalse) return vim.o.wrap == true and t(ifTrue) or t(ifFalse) end

vim.api.nvim_set_keymap('n', 'j', [[v:lua.smart_wrap_nav_bindings("gj","j")]], {expr = true, noremap = true})
vim.api.nvim_set_keymap('n', 'k', [[v:lua.smart_wrap_nav_bindings("gk","k")]], {expr = true, noremap = true})
vim.api.nvim_set_keymap('n', '0', [[v:lua.smart_wrap_nav_bindings("g0","0")]], {expr = true, noremap = true})
vim.api.nvim_set_keymap('n', '$', [[v:lua.smart_wrap_nav_bindings("g$","$")]], {expr = true, noremap = true})

-- resource the neovim configurations
vim.api.nvim_set_keymap('', '<LEADER>%', ':luafile ~/.config/nvim/init.lua<CR>', {noremap = true})

-- unhighlight search
vim.api.nvim_set_keymap('n', '<LEADER><SPACE>', ':nohlsearch<CR>', {noremap = true, silent = true})

vim.api.nvim_set_keymap('', '<LEADER>w', ':set wrap!<CR>', {})
vim.api.nvim_set_keymap('', '<LEADER>s', ':set spell! spelllang=en_us<CR>', {noremap = true})

-- resize window
vim.api.nvim_set_keymap('', '<A-h>', '<C-w><', {silent = true})
vim.api.nvim_set_keymap('', '<A-k>', '<C-w>-', {silent = true})
vim.api.nvim_set_keymap('', '<A-j>', '<C-w>+', {silent = true})
vim.api.nvim_set_keymap('', '<A-l>', '<C-w>>', {silent = true})

-- keep asterisk and pound to be case sensitive
vim.api.nvim_set_keymap('n', '<LEADER>*', [[:let @/='\C\<' . expand('<cword>') . '\>'<CR>:let v:searchforward=1<CR>n]], {noremap = true})
vim.api.nvim_set_keymap('n', '<LEADER>#', [[:let @/='\C\<' . expand('<cword>') . '\>'<CR>:let v:searchforward=0<CR>n]], {noremap = true})

-- copy, paste and copy whole file to clipboard
vim.api.nvim_set_keymap('', '<LEADER>c', '"+y', {})
vim.api.nvim_set_keymap('', '<LEADER>v', ':r !pbpaste<CR><CR>', {})
vim.api.nvim_set_keymap('', '<LEADER>x', ':%w !pbcopy<CR><CR>', {})

-- identify syntax below cursor with <LEADER>h
vim.api.nvim_set_keymap('', '<LEADER>h', [[:echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<' . synIDattr(synID(line("."),col("."),0),"name") . "> lo<" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>]], {})

-- rebind vim's ga to be <LEADER>a
vim.api.nvim_set_keymap('', '<LEADER>a', 'ga', {noremap = true})

-- remap <C-d> to Delete
vim.api.nvim_set_keymap('i', '<C-d>', '<DEL>', {noremap = true})

-- remap jk and <C-c> to escape
vim.api.nvim_set_keymap('l', 'jk', '<ESC>', {noremap = true})
vim.api.nvim_set_keymap('i', '<C-c>', '<ESC>', {noremap = true})

-- make emacs navigation available on EX-mode

vim.api.nvim_set_keymap('c', '<C-a>', '<Home>', {noremap = true})
vim.api.nvim_set_keymap('c', '<C-e>', '<End>', {noremap = true})
vim.api.nvim_set_keymap('c', '<C-b>', '<Left>', {noremap = true})
vim.api.nvim_set_keymap('c', '<C-f>', '<Right>', {noremap = true})
vim.api.nvim_set_keymap('c', '<C-M-b>', '<S-Left>', {noremap = true})
vim.api.nvim_set_keymap('c', '<C-M-f>', '<S-Right>', {noremap = true})

------------------------------------------------------------
--                   PLUGINS KEYBINDINGS
------------------------------------------------------------

-- barbar.nvim
vim.api.nvim_set_keymap('n', 'gb',    '<CMD>BufferNext<CR>',         {noremap = true})
vim.api.nvim_set_keymap('n', 'gB',    '<CMD>BufferPrevious<CR>',     {noremap = true})
vim.api.nvim_set_keymap('n', 'g>',    '<CMD>BufferMoveNext<CR>',     {noremap = true})
vim.api.nvim_set_keymap('n', 'g<',    '<CMD>BufferMovePrevious<CR>', {noremap = true})
vim.api.nvim_set_keymap('n', 'gx',    '<CMD>BufferClose<CR>',        {noremap = true})
vim.api.nvim_set_keymap('n', '<A-1>', '<CMD>BufferGoto 1<CR>',       {noremap = true})
vim.api.nvim_set_keymap('n', '<A-2>', '<CMD>BufferGoto 2<CR>',       {noremap = true})
vim.api.nvim_set_keymap('n', '<A-3>', '<CMD>BufferGoto 3<CR>',       {noremap = true})
vim.api.nvim_set_keymap('n', '<A-4>', '<CMD>BufferGoto 4<CR>',       {noremap = true})
vim.api.nvim_set_keymap('n', '<A-5>', '<CMD>BufferGoto 5<CR>',       {noremap = true})
vim.api.nvim_set_keymap('n', '<A-6>', '<CMD>BufferGoto 6<CR>',       {noremap = true})
vim.api.nvim_set_keymap('n', '<A-7>', '<CMD>BufferGoto 7<CR>',       {noremap = true})
vim.api.nvim_set_keymap('n', '<A-8>', '<CMD>BufferGoto 8<CR>',       {noremap = true})
vim.api.nvim_set_keymap('n', '<A-9>', '<CMD>BufferLast<CR>',         {noremap = true})

-- nvim-tree.lua
vim.api.nvim_set_keymap('n', '<LEADER>e', '<CMD>NvimTreeToggle<CR>', {noremap = true})
vim.api.nvim_set_keymap('n', '<LEADER>r', '<CMD>NvimTreeFindFile<CR>', {noremap = true})

-- telescope.nvim
vim.api.nvim_set_keymap('n', '<C-p>',         [[:Telescope find_files hidden=true<CR>]], {noremap = true})
vim.api.nvim_set_keymap('n', '<LEADER><C-p>', [[:Telescope find_files find_command=fd,--no-ignore hidden=true<CR>]], {noremap = true})
vim.api.nvim_set_keymap('n', '<C-n>',         [[:Telescope live_grep find_command=rg,--no-heading,--hidden,-g='!.git/**',--with-filename,--line-number,--column,--smart-case,--ignore<CR>]], {noremap = true})
vim.api.nvim_set_keymap('n', '<LEADER><C-n>', [[:Telescope live_grep find_command=rg,--no-heading,--hidden,-g='!.git/**',--with-filename,--line-number,--column,--smart-case,--no-ignore<CR>]], {noremap = true})
vim.api.nvim_set_keymap('n', '<C-s>',         [[:Telescope grep_string<CR>]], {noremap = true})
vim.api.nvim_set_keymap('n', '<C-g>',         [[:Telescope git_status<CR>]], {noremap = true})
vim.api.nvim_set_keymap('n', '<C-f>',         [[:Telescope registers<CR>]], {noremap = true})
vim.api.nvim_set_keymap('n', '<C-b>',         [[:Telescope buffers<CR>]], {noremap = true})
vim.api.nvim_set_keymap('n', '<C-h>',         [[:Telescope help_tags<CR>]], {noremap = true})
vim.api.nvim_set_keymap('n', '<C-m>',         [[:Telescope marks<CR>]], {noremap = true})
vim.api.nvim_set_keymap('n', '<C-t>',         [[:Telescope treesitter<CR>]], {noremap = true})
vim.api.nvim_set_keymap('n', '<C-a>',         [[:Telescope commands<CR>]], {noremap = true})

-- vim-sneak
vim.api.nvim_set_keymap('', 'f', '<Plug>Sneak_f', {})
vim.api.nvim_set_keymap('', 'F', '<Plug>Sneak_F', {})
vim.api.nvim_set_keymap('', 't', '<Plug>Sneak_t', {})
vim.api.nvim_set_keymap('', 'T', '<Plug>Sneak_T', {})

-- nvim-compe
vim.api.nvim_set_keymap('i', '<C-Space>', [[compe#complete()]], {noremap = true, expr = true, silent = true})
vim.api.nvim_set_keymap('i', '<TAB>',      [[compe#confirm('<TAB>')]], {noremap = true, expr = true, silent = true})
vim.api.nvim_set_keymap('i', '<C-g>',     [[compe#close('<C-g>')]], {noremap = true, expr = true, silent = true})


------------------------------------------------------------
--                    COMPATIBILITIES
------------------------------------------------------------

local Augroup = {}

Augroup.cmds = function (definitions)
  for group_name, definition in pairs(definitions) do
    vim.cmd('augroup '..group_name)
    vim.cmd('autocmd!')
    for _, def in ipairs(definition) do
      local command = table.concat(vim.tbl_flatten{'autocmd', def}, ' ')
      vim.cmd(command)
    end
    vim.cmd('augroup END')
  end
end

Augroup.cmds({
  -- highlight on yank!!!
  highlight_yank = {
    {"TextYankPost", "* silent! lua require'vim.highlight'.on_yank({timeout = 400})"},
  },
  tab_not_spaces = {
    {"BufNewFile,BufRead", "*.(c|v|vv|py) setlocal tabstop=4"},
    {"BufNewFile,BufRead", "*.(c|v|vv|py) setlocal shiftwidth=4"},
    {"BufNewFile,BufRead", "*.(c|v|vv|py) setlocal set noexpandtab"},
  },
})

vim.api.nvim_exec(
[[
fun! YbbondOtherSetups()
  augroup numbertoggle
    if &number == 1
      autocmd!
      autocmd BufEnter,FocusGained,InsertLeave * set relativenumber
      autocmd BufLeave,FocusLost,InsertEnter * set norelativenumber
    endif
  augroup end

  function! ToggleNumberToggle(numberVar)
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
endfun
call YbbondOtherSetups()
]], true)

-- theme configs
vim.api.nvim_exec(
[[
fun! YbbondOtherColors()
  " hi Comment guifg=LightBlue
  hi Comment gui=italic cterm=italic
  " hi htmlStrike gui=strikethrough cterm=strikethrough guibg=Black ctermbg=Black
  " hi Todo guibg=Black ctermbg=Black guifg=White ctermfg=White gui=bold,italic cterm=bold,italic
  " hi NonText guifg=#4a4a59 ctermfg=Gray
  " hi SpecialKey guifg=#4a4a59 ctermfg=Gray
  hi SignColumn ctermbg=NONE cterm=NONE guibg=NONE gui=NONE

  hi DiffAdded ctermbg=22 guibg=#006c00
  hi DiffRemoved ctermbg=94 guibg=#990006

  " |vimdiff|
  " hi DiffAdd    ctermfg=233 ctermbg=LightGreen guifg=#003300 guibg=#DDFFDD gui=none cterm=none
  " hi DiffChange ctermbg=white  guibg=#ececec gui=none   cterm=none
  " hi DiffText   ctermfg=233  ctermbg=yellow  guifg=#000033 guibg=#DDDDFF gui=none cterm=none
  hi DiffAdd    ctermbg=22
  hi DiffChange ctermbg=94
  hi DiffDelete ctermbg=88
  hi DiffText   ctermfg=233  ctermbg=yellow  guifg=#000033 guibg=#DDDDFF gui=none cterm=none

  " |neogit|
  hi def NeogitDiffAddHighlight guibg=#006c00
  hi def NeogitDiffDeleteHighlight guibg=#990006
  hi def NeogitDiffContextHighlight guibg=#3a4650
  hi def NeogitHunkHeader guifg=#cccccc
  hi def NeogitHunkHeaderHighlight guifg=#cccccc

  " |indent-blankline|
  hi IndentBlanklineContextChar guifg=#a7adba
endfun
call YbbondOtherColors()
]], true)

