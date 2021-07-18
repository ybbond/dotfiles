local vim = vim
local function plug(path, config)
  vim.validate {
    path = {path, 's'},
    config = {config, vim.tbl_islist, 'an array of packages'},
  }
  vim.fn["plug#begin"](path)
  for _, v in ipairs(config) do
    if type(v) == 'string' then
      vim.fn["plug#"](v)
    elseif type(v) == 'table' then
      local p = v[1]
      assert(p, 'Must specify package as first index.')
      v[1] = nil
      vim.fn["plug#"](p, v)
      v[1] = p
    end
  end
  vim.fn["plug#end"]()
end

plug(tostring(os.getenv("HOME")) .. '/.local/share/nvim/plugged', {
  -- sublime like dark mode
  'mhartington/oceanic-next',
  -- icon for all nvim related packages
  'kyazdani42/nvim-web-devicons',

  'nvim-lua/popup.nvim',
  'nvim-lua/plenary.nvim',
  'nvim-telescope/telescope.nvim',
  'fannheyward/telescope-coc.nvim',

  'romgrk/barbar.nvim',

  {'glepnir/galaxyline.nvim', ['branch'] = 'main'},

  'kyazdani42/nvim-tree.lua',

  -- https://github.com/nvim-treesitter/nvim-treesitter#supported-languages
  -- :TSInstall {language}
  {'nvim-treesitter/nvim-treesitter', ['do'] = ':TSUpdate'},

  'tpope/vim-fugitive',
  'tpope/vim-rhubarb',
  'shumphrey/fugitive-gitlab.vim',

  'rhysd/git-messenger.vim',

  'justinmk/vim-sneak',

  'tmux-plugins/vim-tmux',
  'tmux-plugins/vim-tmux-focus-events',

  'junegunn/vim-peekaboo',

  'tpope/vim-commentary',
  'tpope/vim-surround',
  'tpope/vim-repeat',

  {'neoclide/coc.nvim', ['branch'] = 'release'},

  'dart-lang/dart-vim-plugin',

  'pangloss/vim-javascript',
  'othree/yajs.vim',
  'maxmellon/vim-jsx-pretty',

  {'styled-components/vim-styled-components', ['branch'] = 'main' },

  'leafgarland/typescript-vim',
  'HerringtonDarkholme/yats.vim',

  'cespare/vim-toml',
  'stephpy/vim-yaml',

  'jparise/vim-graphql',
})
