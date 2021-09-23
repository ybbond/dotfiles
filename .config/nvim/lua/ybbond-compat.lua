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
  HighlightYank = {
    {"TextYankPost", "* silent! lua require'vim.highlight'.on_yank({timeout = 400})"},
  },
  TabNotSpaces = {
    {"BufNewFile,BufRead", "*.(c|v|vv|py) setlocal tabstop=4"},
    {"BufNewFile,BufRead", "*.(c|v|vv|py) setlocal shiftwidth=4"},
    {"BufNewFile,BufRead", "*.(c|v|vv|py) setlocal set noexpandtab"},
  },
  DartDollar = {
    {"BufNewFile,BufRead", "*.dart setlocal iskeyword+=$"},
  },
  WrapForWritingMode = {
    {"BufNewFile,BufRead", "*.(md|mmd|txt|markdown) setlocal set wrap!"},
  }
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
  " if has('nvim')
  "   " make the ^[ sign with:
  "   " (INSERT MODE) <C-v><ESC>
  "   " make sure it shows different color than the rest
  "   set t_SI=[5\ q
  "   set t_SR=[4\ q
  "   set t_EI=[1\ q
  " else
  if exists('$TMUX')
      " tmux will only forward escape sequences to the terminal if surrounded by a DCS sequence
      let &t_SI .= "\<Esc>Ptmux;\<Esc>\<Esc>[5 q\<Esc>\\"
      let &t_SR .= "\<Esc>Ptmux;\<Esc>\<Esc>[4 q\<Esc>\\"
      let &t_EI .= "\<Esc>Ptmux;\<Esc>\<Esc>[1 q\<Esc>\\"

      " italic
      let &t_8f="\<Esc>[38;2;%lu;%lu;%lum"
      let &t_8b="\<Esc>[48;2;%lu;%lu;%lum"

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
  " endif

  " vimdiff
  " au VimEnter * if &diff | execute 'windo set wrap' | execute 'windo set nofoldenable' | endif
  if &diff
    set wrap
    set nofoldenable
    nnoremap gr :diffupdate<CR>
  endif

  if &diff
    map <LEADER>s :call IwhiteToggle()<CR>
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
