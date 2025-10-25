return {
  -- Disable some expensive LazyVim features for large projects
  {
    "folke/flash.nvim",
    enabled = true,
    opts = {
      -- Reduce search overhead
      modes = {
        search = {
          enabled = false, -- Don't integrate with search
        },
      },
    },
  },

  -- Optimize which-key (can be slow with many mappings)
  {
    "folke/which-key.nvim",
    opts = {
      preset = "modern",
      delay = 400, -- Faster popup
    },
  },

  -- Optimize mini.indentscope (can be expensive on large files)
  {
    "nvim-mini/mini.indentscope",
    event = "LazyFile",
    opts = {
      draw = {
        delay = 100,
        animation = function()
          return 0 -- Disable animation
        end,
      },
    },
  },

  -- Optimize nvim-notify
  {
    "rcarriga/nvim-notify",
    opts = {
      timeout = 1000,
      max_height = function()
        return math.floor(vim.o.lines * 0.5)
      end,
      max_width = function()
        return math.floor(vim.o.columns * 0.5)
      end,
      -- Reduce animation stages for performance
      stages = "static",
    },
  },
}
