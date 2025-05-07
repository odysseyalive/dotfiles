return {
  -- mason work-around
  { "mason-org/mason.nvim", version = "1.11.0" },
  { "mason-org/mason-lspconfig.nvim", version = "1.32.0" },
  --
  "WhoIsSethDaniel/mason-tool-installer.nvim",
  config = function()
    require("mason-tool-installer").setup({
      ensure_installed = {
        "php-cs-fixer", -- PHP formatter
        "intelephense",
        "php-debug-adapter",
        -- Add other tools as needed
      },
      auto_update = true, -- Optional: automatically update tools
      run_on_start = true, -- Install/update on startup
    })
  end,
}
