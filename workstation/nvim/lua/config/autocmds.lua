-- Autocmds are automatically loaded on the VeryLazy event
-- Default autocmds that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/autocmds.lua
--
-- Add any additional autocmds here
-- with `vim.api.nvim_create_autocmd`
--
-- Or remove existing autocmds by their group name (which is prefixed with `lazyvim_` for the defaults)
-- e.g. vim.api.nvim_del_augroup_by_name("lazyvim_wrap_spell")

-- Reload colorscheme when terminal theme changes (via omarchy-theme-sync)
local theme_trigger_file = vim.fn.expand("~/.cache/nvim-theme-trigger")
local last_theme_mtime = 0

vim.api.nvim_create_autocmd("FocusGained", {
  pattern = "*",
  callback = function()
    local ok, stats = pcall(vim.loop.fs_stat, theme_trigger_file)
    if ok and stats and stats.mtime.sec > last_theme_mtime then
      last_theme_mtime = stats.mtime.sec
      -- Re-apply the current colorscheme to pick up terminal color changes
      local current = vim.g.colors_name
      if current then
        vim.schedule(function()
          vim.cmd.colorscheme(current)
        end)
      end
    end
  end,
})

-- Disable expensive features for large files
vim.api.nvim_create_autocmd("BufReadPre", {
  pattern = "*",
  callback = function()
    local ok, stats = pcall(vim.loop.fs_stat, vim.fn.expand("%:p"))
    if ok and stats then
      local size_mb = stats.size / (1024 * 1024)
      if size_mb > 1 then
        -- Disable expensive features for files > 1MB
        vim.opt_local.foldmethod = "manual"
        vim.opt_local.spell = false
        vim.opt_local.swapfile = false
        vim.opt_local.undofile = false
        vim.opt_local.loadplugins = false
        -- Notify user
        vim.notify("Large file detected, some features disabled", vim.log.levels.WARN)
      end
    end
  end,
})
