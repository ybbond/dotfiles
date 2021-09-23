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
    LspDiagnosticsUnderlineHint = { bg = '#0f3938', style = 'bold' },
    GitSignsAddNr = { bg = '#266d6a', fg = '#2e3440', style = 'bold' },
    GitSignsChangeNr = { bg= '#536c9e', fg = '#2e3440', style = 'bold'  },
    GitSignsDeleteNr = { bg='#b2555b', fg = '#2e3440', style = 'bold' },
    -- LspDiagnosticsSignError = { bg = '#ec5f67', fg = '#2e3440', style = 'bold'},
    -- LspDiagnosticsSignWarning = { bg = '#f99157', fg = '#2e3440', style = 'bold'},
    -- LspDiagnosticsSignInformation = { bg = '#6699cc', fg = '#2e3440', style = 'bold'},
    -- LspDiagnosticsSignHint = { bg = '#ffffff', fg = '#2e3440', style = 'bold'},
},
})

require('nightfox').load()
