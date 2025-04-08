return {
  -- Add vim-coffee-script plugin
  {
    "kchmck/vim-coffee-script",
    ft = "coffee", -- Load the plugin only for CoffeeScript files
  },

  -- Optional: Enhance with Treesitter
  {
    "nvim-treesitter/nvim-treesitter",
    opts = function(_, opts)
      -- Extend the ensure_installed table to include CoffeeScript
      if type(opts.ensure_installed) == "table" then
        vim.list_extend(opts.ensure_installed, { "coffee" })
      end
    end,
  },
}
