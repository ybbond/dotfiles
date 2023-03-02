------------------------------------------------------------
--                    CUSTOMIZATIONS
------------------------------------------------------------

vim.o.compatible = false
vim.o.encoding = 'UTF-8'

vim.g.mapleader = [[\]]

vim.opt.termguicolors = true
vim.o.background = 'dark'

vim.cmd('filetype plugin on')
vim.cmd('filetype plugin indent on')

vim.o.paste = false
vim.o.pastetoggle = '<F2>'

vim.o.tabstop = 2
vim.o.softtabstop = 2
vim.o.shiftwidth = 2
vim.o.expandtab = true
vim.o.wrap = false
vim.o.linebreak = true
vim.o.list = true
vim.o.listchars = [[tab:> ,trail:-,nbsp:+]]

vim.o.laststatus = 3

vim.o.mouse = 'n'
vim.o.showmode = false

vim.o.number = true
vim.g.use_relative_number = true

vim.o.ruler = true
vim.o.showmatch = true
vim.o.cursorline = true

vim.o.ignorecase = true
vim.o.smartcase = true
vim.o.incsearch = true
vim.o.hlsearch = true

vim.o.inccommand = 'split'

-- originally used by |vim-fugitive|
vim.o.diffopt = vim.o.diffopt .. ',vertical'

-- fold settings
-- https://www.reddit.com/r/neovim/comments/psl8rq/sexy_folds/
-- vim.wo.foldmethod = "expr"
-- vim.wo.foldexpr = "nvim_treesitter#foldexpr()"
-- vim.wo.foldtext =
    -- [[substitute(getline(v:foldstart),'\\t',repeat('\ ',&tabstop),'g').'...'.trim(getline(v:foldend)) ]]
-- vim.wo.fillchars = "fold:\\"
-- vim.wo.foldnestmax = 3
-- vim.wo.foldminlines = 1

------------------------------------------------------------
--                   KEYBINDINGS
------------------------------------------------------------

local function base_map(lhs)
  return function(mode)
    return function(rhs)
      vim.api.nvim_set_keymap(mode, lhs, rhs, {noremap = true, silent = true})
    end
  end
end

local function base_map_opt(lhs)
  return function(mode)
    return function(rhs)
      return function(opts)
        vim.api.nvim_set_keymap(mode, lhs, rhs, opts or {noremap = true, silent = true})
      end
    end
  end
end

function _G.noremap(lhs) return base_map(lhs)('') end
function _G.nnoremap(lhs) return base_map(lhs)('n') end
function _G.inoremap(lhs) return base_map(lhs)('i') end
function _G.vnoremap(lhs) return base_map(lhs)('v') end
function _G.xnoremap(lhs) return base_map(lhs)('x') end

function _G.map(lhs) return base_map_opt(lhs)('') end
function _G.nmap(lhs) return base_map_opt(lhs)('n') end

function _G.replaceTermcodes(str) return vim.api.nvim_replace_termcodes(str, true, true, true) end

function _G.smart_wrap_nav_bindings(ifTrue,ifFalse)
  return vim.o.wrap == true and replaceTermcodes(ifTrue) or replaceTermcodes(ifFalse)
end

nmap 'j' [[v:lua.smart_wrap_nav_bindings("gj","j")]] ({expr = true, noremap = true})
nmap 'k' [[v:lua.smart_wrap_nav_bindings("gk","k")]] ({expr = true, noremap = true})
nmap '0' [[v:lua.smart_wrap_nav_bindings("g0","0")]] ({expr = true, noremap = true})
nmap '$' [[v:lua.smart_wrap_nav_bindings("g$","$")]] ({expr = true, noremap = true})

-- resource the neovim configurations
noremap '<LEADER>%' ':luafile ~/.config/nvim/init.lua<CR>'

-- unhighlight search
nnoremap '<LEADER><SPACE>' ':nohlsearch<CR>'

noremap '<LEADER>w' ':set wrap!<CR>'
noremap '<LEADER>s' ':set spell! spelllang=en_us<CR>'

-- resize window
noremap 'Ó' '<C-w><' -- Alt Shift h
noremap '' '<C-w>-' -- Alt Shift k
noremap 'Ô' '<C-w>+' -- Alt Shift j
noremap 'Ò' '<C-w>>' -- Alt Shift l

-- temporarily handled in nvim-hlslens
-- keep asterisk and pound to be case sensitive
-- nnoremap '<LEADER>*' [[:let @/='\C\<' . expand('<cword>') . '\>'<CR>:let v:searchforward=1<CR>n]]
-- nnoremap '<LEADER>#' [[:let @/='\C\<' . expand('<cword>') . '\>'<CR>:let v:searchforward=0<CR>n]]

-- copy, paste and copy whole file to clipboard
map '<LEADER>cs'   '"+y' ({})
map '<LEADER>v'    ':r !pbpaste<CR><CR>' ({})
map '<LEADER>ca'   ':%w !pbcopy<CR><CR>' ({})

-- temporarily handled in nvim-treesitter
-- identify syntax below cursor with <LEADER>h
-- noremap '<LEADER>h' [[:echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<' . synIDattr(synID(line("."),col("."),0),"name") . "> lo<" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>]]

nnoremap '<LEADER>a' 'ga'
-- nnoremap '<LEADER>x' 'gx'

inoremap '<C-d>' '<DEL>'

inoremap 'jk'    '<ESC>'
inoremap '<C-c>' '<ESC>'

-- https://www.reddit.com/r/neovim/comments/tsol2n/why_macros_are_so_slow_compared_to_emacs/i2ugipm/
nnoremap '@' [[<cmd>execute "noautocmd norm! " . v:count1 . "@" . getcharstr()<cr>]]
xnoremap '@' [[:<C-U>execute "noautocmd '<,'>norm! " . v:count1 . "@" . getcharstr()<cr>]]

vim.g.markdown_fenced_languages = {
  "ts=typescript"
}

-- -- make emacs navigation available on EX-mode
-- -- done in compat
-- cnoremap '<C-a>'   '<Home>'
-- cnoremap '<C-e>'   '<End>'
-- cnoremap '<C-b>'   '<Left>'
-- cnoremap '<C-f>'   '<Right>'
-- cnoremap '<C-A-b>' '<S-Left>'
-- cnoremap '<C-A-f>' '<S-Right>'

-- <C-R><C-W> command line mode, insert word under cursor


------------------------------------------------------------
--                       RESOURCES
------------------------------------------------------------

require'compat'
require'autos'

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable",
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

require('lazy').setup('plugins')
