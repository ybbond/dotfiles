------------------------------------------------------------
--                    COMPATIBILITIES
------------------------------------------------------------

local au = require'au'

au({'TextYankPost'}, {
  '*',
  function()
    vim.highlight.on_yank({ timeout = 400 })
  end
})

au({'BufNewFile', 'BufRead'}, {
  '*.c,*.cpp,*.cc,*.h,*.py',
  function()
    vim.bo.tabstop = 4
    vim.bo.shiftwidth = 4
    vim.bo.expandtab = false
  end,
})

au({'BufNewFile', 'BufRead'}, {
  '*.md,*.mmd,*.txt,*.markdown,*.multimarkdown',
  function()
    vim.wo.wrap = true
  end
})

au({'FileType'}, {
  'qf',
  function()
    vim.api.nvim_buf_set_keymap(0, 'n', 'q', '<CMD>cclose<CR>', {noremap = true})
  end,
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

  cnoremap <C-a>   <Home>
  cnoremap <C-e>   <End>
  cnoremap <C-b>   <Left>
  cnoremap <C-f>   <Right>
  cnoremap <C-A-b> <S-Left>
  cnoremap <C-A-f> <S-Right>

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
