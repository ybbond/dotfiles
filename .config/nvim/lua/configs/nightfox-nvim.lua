require('nightfox').setup({
  fox = "nordfox",
  styles = {
    comments = 'italic',
    strings = 'italic',
    variables = 'bold',
    functions = 'bold',
    keywords = 'bold',
  },
  hlgroups = {
    LspDiagnosticsUnderlineError = { bg = '#850007', style = 'bold' },
    LspDiagnosticsUnderlineWarning = { bg = '#6b4700', style = 'bold' },
    LspDiagnosticsUnderlineInformation = { bg = '#072e55', style = 'bold' },
    LspDiagnosticsUnderlineHint = { bg = '#0f3938', style = 'bold' }
  },
})

require('nightfox').load()
