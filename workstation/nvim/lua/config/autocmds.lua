-- Autocmds are automatically loaded on the VeryLazy event
-- Default autocmds that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/autocmds.lua
--
-- Add any additional autocmds here
-- with `vim.api.nvim_create_autocmd`
--
-- Or remove existing autocmds by their group name (which is prefixed with `lazyvim_` for the defaults)
-- e.g. vim.api.nvim_del_augroup_by_name("lazyvim_wrap_spell")

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
