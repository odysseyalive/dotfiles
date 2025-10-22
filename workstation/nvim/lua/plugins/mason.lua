return {
  -- mason work-around
  { "mason-org/mason-lspconfig.nvim", dependencies = { "mason-org/mason.nvim" } },
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
