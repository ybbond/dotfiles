vim.api.nvim_exec(
[[
fun! DetectWrap()
  nnoremap <expr> j &wrap == 1 ? 'gj' : 'j'
  nnoremap <expr> k &wrap == 1 ? 'gk' : 'k'
  nnoremap <expr> $ &wrap == 1 ? 'g$' : '$'
  nnoremap <expr> 0 &wrap == 1 ? 'g0' : '0'
endfun
]], true)

-- |coc.nvim|
vim.api.nvim_exec(
[[
fun! ShowDocumentation()
  if &filetype == 'vim'
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfun
]], true)

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
endfun
call YbbondOtherSetups()
]], true)


-- theme configs
vim.api.nvim_exec(
[[
fun! ColorMyPencils()
  " hi Comment guifg=LightBlue
  hi Comment gui=italic cterm=italic
  " hi htmlStrike gui=strikethrough cterm=strikethrough guibg=Black ctermbg=Black
  " hi Todo guibg=Black ctermbg=Black guifg=White ctermfg=White gui=bold,italic cterm=bold,italic
  " hi NonText guifg=#4a4a59 ctermfg=Gray
  " hi SpecialKey guifg=#4a4a59 ctermfg=Gray
  hi SignColumn ctermbg=NONE cterm=NONE guibg=NONE gui=NONE

  hi DiffAdded ctermbg=22 guibg=#006c00
  hi DiffRemoved ctermbg=94 guibg=#990006
endfun
call ColorMyPencils()
]], true)


