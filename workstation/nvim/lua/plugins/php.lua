return {
  -- Disable PHPCS linter that comes with PHP extras
  {
    "mfussenegger/nvim-lint",
    optional = true,
    opts = {
      linters_by_ft = {
        php = {}, -- Empty table = no linters for PHP
      },
    },
  },

  -- Disable PHPCS in none-ls (formerly null-ls)
  {
    "nvimtools/none-ls.nvim",
    optional = true,
    opts = function(_, opts)
      -- Remove PHPCS from the sources if it exists
      if opts.sources then
        opts.sources = vim.tbl_filter(function(source)
          return source.name ~= "phpcs"
        end, opts.sources)
      end
      return opts
    end,
  },

  -- Configure LSP preferences
  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        phpactor = {
          mason = false,
          enabled = false,
        },
        intelephense = {
          enabled = true,
        },
      },
    },
  },
}
