return {
  "nvim-treesitter/nvim-treesitter",
  dependencies = {
    {
      "nvim-treesitter/nvim-treesitter-context",
      opts = {
        enable = true, -- Enable the sticky function/class header
        max_lines = 3, -- Number of lines to display at the top (adjust as needed)
        trim_scope = "outer", -- Keeps the outermost context
        mode = "cursor", -- Update context when the cursor moves
      },
    },
  },
  opts = {
    ensure_installed = {},
    auto_install = true,
    highlight = { enable = true },
    indent = { enable = true },
  },
}
