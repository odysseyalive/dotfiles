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
    ignore_install = { "coffee" },
    highlight = {
      enable = true,
      -- Disable for large files
      disable = function(lang, buf)
        local max_filesize = 100 * 1024 -- 100 KB
        local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(buf))
        if ok and stats and stats.size > max_filesize then
          return true
        end
      end,
      -- Improve performance
      additional_vim_regex_highlighting = false,
    },
    indent = { enable = true },
    -- Incremental selection is expensive
    incremental_selection = {
      enable = false,
    },
  },
}
