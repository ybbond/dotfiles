------------------------------------------------------------
--                      RESOURCES
------------------------------------------------------------


require('plenary.reload').reload_module('plugins')
require('plenary.reload').reload_module('configs')
require('plenary.reload').reload_module'%'

require('plugins')
require('configs')
-- require'%'

------------------------------------------------------------
--                    CUSTOMIZATIONS
------------------------------------------------------------

vim.o.compatible = false
vim.o.encoding = 'UTF-8'

vim.o.termguicolors = true
vim.o.background = 'dark'

vim.cmd('colorscheme OceanicNext')
-- vim.cmd('syntax enable')
-- vim.cmd('syntax on')
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

-- originally used by |vim-fugitive|
vim.o.diffopt = vim.o.diffopt .. ',vertical'


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

local function replaceTermcodes(str) return vim.api.nvim_replace_termcodes(str, true, true, true) end

function _G.smart_wrap_nav_bindings(ifTrue,ifFalse) return vim.o.wrap == true and replaceTermcodes(ifTrue) or replaceTermcodes(ifFalse) end

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

-- why does vim's Y behavior different?
vim.api.nvim_set_keymap('n', 'Y', 'y$', {noremap = true})

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

-- nvim-bufferline
vim.api.nvim_set_keymap('n', 'gb', '<CMD>BufferLineCycleNext<CR>', {noremap = true})
vim.api.nvim_set_keymap('n', 'gB', '<CMD>BufferLineCyclePrev<CR>', {noremap = true})
vim.api.nvim_set_keymap('n', 'g>', '<CMD>BufferLineMoveNext<CR>',  {noremap = true})
vim.api.nvim_set_keymap('n', 'g<', '<CMD>BufferLineMovePrev<CR>',  {noremap = true})
-- bufdelete.nvim
vim.api.nvim_set_keymap('n', 'gx', '<CMD>BufDel<CR>',              {noremap = true})

-- nvim-tree.lua
vim.api.nvim_set_keymap('n', '<LEADER>e', '<CMD>NvimTreeToggle<CR>', {noremap = true})
vim.api.nvim_set_keymap('n', '<LEADER>r', '<CMD>NvimTreeFindFile<CR>', {noremap = true})

-- telescope.nvim
vim.api.nvim_set_keymap('n', '<C-p>',         [[<CMD>lua require("telescope.builtin").find_files{ find_command= { "fd", "-E=.git", "--hidden" }} hidden=true<CR>]], {noremap = true})
vim.api.nvim_set_keymap('n', '<LEADER><C-p>', [[<CMD>lua require("telescope.builtin").find_files{ find_command= { "fd", "-E=.git", "--hidden", "--no-ignore" }} hidden=true<CR>]], {noremap = true})
vim.api.nvim_set_keymap('n', '<C-n>',         [[<CMD>Telescope live_grep find_command=rg,--no-heading,--hidden,-g='!.git/**',--with-filename,--line-number,--column,--smart-case,--ignore<CR>]], {noremap = true})
vim.api.nvim_set_keymap('n', '<LEADER><C-n>', [[<CMD>Telescope live_grep find_command=rg,--no-heading,--hidden,-g='!.git/**',--with-filename,--line-number,--column,--smart-case,--no-ignore<CR>]], {noremap = true})
vim.api.nvim_set_keymap('n', '<C-s>',         [[<CMD>Telescope grep_string<CR>]], {noremap = true})
vim.api.nvim_set_keymap('n', '<C-g><C-g>',    [[<CMD>Telescope git_status<CR>]], {noremap = true})
vim.api.nvim_set_keymap('n', '<C-g>g',        [[<CMD>Telescope git_status<CR>]], {noremap = true})
vim.api.nvim_set_keymap('n', '<C-f>',         [[<CMD>Telescope registers<CR>]], {noremap = true})
vim.api.nvim_set_keymap('n', '<C-b>',         [[<CMD>Telescope buffers<CR>]], {noremap = true})
vim.api.nvim_set_keymap('n', '<C-h>',         [[<CMD>Telescope help_tags<CR>]], {noremap = true})
vim.api.nvim_set_keymap('n', '<C-m>',         [[<CMD>Telescope marks<CR>]], {noremap = true})
-- vim.api.nvim_set_keymap('n', '<C-t>',         [[:Telescope treesitter<CR>]], {noremap = true})
-- vim.api.nvim_set_keymap('n', '<C-a>',         [[:Telescope commands<CR>]], {noremap = true})
vim.api.nvim_set_keymap('n', '<C-t>a',    [[<CMD>Telescope commands<CR>]], {noremap = true})
vim.api.nvim_set_keymap('n', '<C-t><C-a>',    [[<CMD>Telescope commands<CR>]], {noremap = true})
vim.api.nvim_set_keymap('n', '<C-t>t',    [[<CMD>Telescope treesitter<CR>]], {noremap = true})
vim.api.nvim_set_keymap('n', '<C-t><C-t>',    [[<CMD>Telescope treesitter<CR>]], {noremap = true})

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
--                   COLOR HIGHLIGHTS
------------------------------------------------------------

vim.cmd('hi Comment gui=italic cterm=italic')
vim.cmd('hi SignColumn ctermbg=NONE cterm=NONE guibg=NONE gui=NONE')

vim.cmd('hi DiffAdded ctermbg=LightGreen guibg=#006c00 ctermfg=Black guifg=#F0F2F5 gui=NONE cterm=NONE')
vim.cmd('hi DiffRemoved ctermbg=LightRed guibg=#990006 ctermfg=Black guifg=#F0F2F5 gui=NONE cterm=NONE')
vim.cmd('hi diffAdded ctermbg=LightGreen guibg=#006c00 ctermfg=Black guifg=#F0F2F5 gui=NONE cterm=NONE')
vim.cmd('hi diffRemoved ctermbg=LightRed guibg=#990006 ctermfg=Black guifg=#F0F2F5 gui=NONE cterm=NONE')

-- |vimdiff|
vim.cmd('hi DiffAdd    ctermbg=22')
vim.cmd('hi DiffChange ctermbg=94')
vim.cmd('hi DiffDelete ctermbg=88')
vim.cmd('hi DiffText   ctermfg=233  ctermbg=yellow  guifg=#000033 guibg=#DDDDFF gui=none cterm=none')

-- |neogit|
vim.cmd('hi def NeogitDiffAddHighlight guibg=#006c00')
vim.cmd('hi def NeogitDiffDeleteHighlight guibg=#990006')
vim.cmd('hi def NeogitDiffContextHighlight guibg=#3a4650')
vim.cmd('hi def NeogitHunkHeader guifg=#cccccc')
vim.cmd('hi def NeogitHunkHeaderHighlight guifg=#cccccc')


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
