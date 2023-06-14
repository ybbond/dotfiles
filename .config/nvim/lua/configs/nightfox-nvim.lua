local colors = {
  white = '#ffffff',
  black = '#000000',

  fg1 = '#e0e7f0',
  fg2 = '#b9bfca',
  fg3 = '#969cab',

  bg0 = '#1E1F25',
  bg1 = '#232831',
  bg2 = '#2e3440',
  bg3 = '#3b4252',

  bright = {
    error = '#bf616a',
    hint = '#a3be8c',
    info = '#8cafd2',
    warn = '#ebcb8b',
  },
  dark = {
    error = '#a54e56',
    hint = '#8aa872',
    info = '#668aab',
    warn = '#d9b263',
  },
  darker = {
    warn = '#754e01',
  },
}

local groups = {
  all = {
    LspDiagnosticsUnderlineError = { bg = '#850007', style = 'bold' },
    LspDiagnosticsUnderlineWarning = { bg = '#6b4700', style = 'bold' },
    LspDiagnosticsUnderlineInformation = { bg = '#072e55', style = 'bold' },
    LspDiagnosticsUnderlineHint = { bg = '#0f3938', style = 'bold' },

    -- since when??
    DiagnosticUnderlineError = { bg = '#850007', style = 'bold' },
    DiagnosticUnderlineWarn = { bg = '#6b4700', style = 'bold' },
    DiagnosticUnderlineInfo = { bg = '#072e55', style = 'bold' },
    DiagnosticUnderlineHint = { bg = '#0f3938', style = 'bold' },

    GitSignsAddNr = { bg = '#266d6a', fg = '#ffffff' },
    GitSignsChangeNr = { bg= '#536c9e', fg = '#ffffff'  },
    GitSignsDeleteNr = { bg='#b2555b', fg = '#ffffff' },
    GitSignsAddLn = { bg = '#40494B' },
    GitSignsChangeLn = { bg = '#3A4453' },
    GitSignsDeleteLn = { bg = '#443B46' },
    GitSignsAddWord = { bg = '#4d585b' },
    GitSignsChangeWord = { bg = '#475466' },
    GitSignsDeleteWord = { bg = '#544856' },

    -- LspDiagnosticsSignError = { bg = '#ec5f67', fg = '#2e3440', style = 'bold'},
    -- LspDiagnosticsSignWarning = { bg = '#f99157', fg = '#2e3440', style = 'bold'},
    -- LspDiagnosticsSignInformation = { bg = '#6699cc', fg = '#2e3440', style = 'bold'},
    -- LspDiagnosticsSignHint = { bg = '#ffffff', fg = '#2e3440', style = 'bold'},

    BufferCurrent        = { bg = colors.bg3, fg = colors.fg1 },
    BufferCurrentERROR = { bg = colors.bg3, fg = colors.bright.error },
    BufferCurrentHINT = { bg = colors.bg3, fg = colors.bright.hint },
    BufferCurrentINFO = { bg = colors.bg3, fg = colors.bright.info },
    BufferCurrentWARN = { bg = colors.bg3, fg = colors.bright.warn },
    BufferCurrentIndex   = { bg = colors.bg3, fg = colors.bright.info },
    BufferCurrentMod     = { bg = colors.bg3, fg = colors.bright.warn },
    BufferCurrentSign    = { bg = colors.bg3, fg = colors.bright.info },
    BufferCurrentTarget  = { bg = colors.bg3, fg = colors.bright.error },
    BufferAlternate = { bg = colors.bg2, fg = colors.fg1 },
    BufferAlternateERROR = { bg = colors.bg2, fg = colors.bright.error },
    BufferAlternateHINT = { bg = colors.bg2, fg = colors.bright.hint },
    BufferAlternateIndex = { bg = colors.bg2, fg = colors.bright.info },
    BufferAlternateINFO = { bg = colors.bg2, fg = colors.bright.info },
    BufferAlternateMod = { bg = colors.bg2, fg = colors.bright.warn },
    BufferAlternateSign = { bg = colors.bg2, fg = colors.bright.info },
    BufferAlternateTarget = { bg = colors.bg2, fg = colors.bright.red },
    BufferAlternateWARN = { bg = colors.bg2, fg = colors.bright.warn },
    BufferVisible        = { bg = colors.bg0, fg = colors.fg2 },
    BufferVisibleERROR = { bg = colors.bg0, fg = colors.bright.error },
    BufferVisibleHINT = { bg = colors.bg0, fg = colors.bright.hint },
    BufferVisibleINFO = { bg = colors.bg0, fg = colors.bright.info },
    BufferVisibleWARN = { bg = colors.bg0, fg = colors.bright.warn },
    BufferVisibleIndex   = { bg = colors.bg0, fg = colors.bright.info },
    BufferVisibleMod     = { bg = colors.bg0, fg = colors.bright.warn },
    BufferVisibleSign    = { bg = colors.bg0, fg = colors.bright.info },
    BufferVisibleTarget  = { bg = colors.bg0, fg = colors.bright.error },
    BufferInactive       = { bg = colors.bg0, fg = colors.fg3 },
    BufferInactiveERROR = { bg = colors.bg0, fg = colors.dark.error },
    BufferInactiveHINT = { bg = colors.bg0, fg = colors.dark.hint },
    BufferInactiveINFO = { bg = colors.bg0, fg = colors.dark.info },
    BufferInactiveWARN = { bg = colors.bg0, fg = colors.dark.warn },
    BufferInactiveIndex  = { bg = colors.bg0, fg = colors.fg3 },
    BufferInactiveMod    = { bg = colors.bg0, fg = colors.darker.warn },
    BufferInactiveSign   = { bg = colors.bg0, fg = colors.bg2 },
    BufferInactiveTarget = { bg = colors.bg0, fg = colors.bright.error },
    BufferOffset = { bg = colors.bg0, fg = colors.fg2 },
    BufferTabpageFill = { bg = colors.bg0, fg = colors.fg2 },
    BufferTabpages       = { bg = colors.bg0 },
    BufferTabpage        = { bg = colors.bg0, fg = colors.bg2 },

  }
}

require('nightfox').setup({
  options = {
    transparent = false,
    styles = {
      comments = 'italic',
      strings = 'italic',
      variables = 'bold',
      functions = 'bold',
      keywords = 'bold',
    },
  },
  groups = groups,
})

-- require('nightfox').load()
-- require('nightfox').load('nordfox')
