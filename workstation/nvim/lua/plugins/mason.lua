return {
  -- mason work-around
  { "mason-org/mason-lspconfig.nvim", dependencies = { "mason-org/mason.nvim" } },
  -- Append Mason's bin dir instead of prepending it, so a working
  -- ~/.local/bin/tree-sitter (built for this glibc by `setup.sh tools`) wins
  -- over Mason's upstream copy, which needs glibc 2.39 on older servers.
  { "mason-org/mason.nvim", opts = { PATH = "append" } },
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
