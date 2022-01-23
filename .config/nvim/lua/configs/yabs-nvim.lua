local yabs = require("yabs")

-- languages = {
--   x = {
--     default_task = "<name_of_task>",
--     tasks = {
--       <name_of_task> = {
--         command = "<command>",
--         type = "`vim` | `lua` | default: `shell`",
--         output = "`buffer` | `consolation` | `echo` | `quickfix` | `terminal` | `none`",
--         opts = {
--           open_on_run = "`never` | `always` | default: `auto`",
--         },
--       },
--     },
--   },
-- },

yabs:setup {
  languages = {
    lua = {
      tasks = {
        run = {
          command = "luafile %",
          type = "vim",
        },
      },
    },
    c = {
      default_task = "build_and_run",
      tasks = {
        build = {
          command = "gcc % -o %:r",
          output = "quickfix",
          -- opts = {
          --   open_on_run = "always",
          -- },
        },
        make_this_file = {
          command = "make %:r",
          output = "terminal",
          opts = {
            open_on_run = "always",
          },
        },
        run = {
          command = function()
            return "./"..vim.fn.expand("%:r")
          end,
          output = "terminal",
          opts = {
            open_on_run = "always",
          },
        },
        build_and_run = {
          -- command = function()
          --   yabs:run_task("build", {on_exit = function()
          --     yabs.languages.c:run_task("run")
          --   end})
          -- end,
          -- type = "lua",
          command = function()
            return "gcc % -o %:r && ./"..vim.fn.expand("%:r")
          end,
          output = "terminal",
          opts = {
            open_on_run = "always",
          },
        },
      },
    },
    typescript = {
      tasks = {
        run = {
          command = "yarn dev",
          output = "terminal",
        },
      },
    },
    typescriptreact = {
      tasks = {
        run = {
          command = "yarn dev",
          output = "terminal",
        },
      },
    },
  },
}
