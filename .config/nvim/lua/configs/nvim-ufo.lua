vim.o.foldcolumn = '1'
vim.o.foldlevel = 99
vim.o.foldlevelstart = 99
vim.o.foldenable = true
-- vim.o.fillchars = [[eob: ,fold: ,foldopen:,foldsep: ,foldclose:]]

-- nnoremap 'zR' [[<Cmd>lua require('ufo').openAllFolds()<CR>]]
-- nnoremap 'zM' [[<Cmd>lua require('ufo').closeAllFolds()<CR>]]

nnoremap ']f' [[<Cmd>lua require('ufo').goNextClosedFold()<CR>]]
nnoremap '[f' [[<Cmd>lua require('ufo').goPreviousClosedFold()<CR>]]
nnoremap ']F' [[<Cmd>lua require('ufo').goNextClosedFold()<CR><Cmd>lua require('ufo').peekFoldedLinesUnderCursor()<CR>]]
nnoremap '[F' [[<Cmd>lua require('ufo').goPreviousClosedFold()<CR><Cmd>lua require('ufo').peekFoldedLinesUnderCursor()<CR>]]

local folded_lines_handler = function(virtText, lnum, endLnum, width, truncate)
  local newVirtText = {}
  local suffix = ('  %d '):format(endLnum - lnum)
  local sufWidth = vim.fn.strdisplaywidth(suffix)
  local targetWidth = width - sufWidth
  local curWidth = 0
  for _, chunk in ipairs(virtText) do
    local chunkText = chunk[1]
    local chunkWidth = vim.fn.strdisplaywidth(chunkText)
    if targetWidth > curWidth + chunkWidth then
      table.insert(newVirtText, chunk)
    else
      chunkText = truncate(chunkText, targetWidth - curWidth)
      local hlGroup = chunk[2]
      table.insert(newVirtText, {chunkText, hlGroup})
      chunkWidth = vim.fn.strdisplaywidth(chunkText)
      -- str width returned from truncate() may less than 2nd argument, need padding
      if curWidth + chunkWidth < targetWidth then
        suffix = suffix .. (' '):rep(targetWidth - curWidth - chunkWidth)
      end
      break
    end
    curWidth = curWidth + chunkWidth
  end
  table.insert(newVirtText, {suffix, 'MoreMsg'})
  return newVirtText
end

require('ufo').setup({
  fold_virt_text_handler = folded_lines_handler,
  provider_selector = function(bufnr, filetype, buftype)
      return {'treesitter', 'indent'}
  end,
})
