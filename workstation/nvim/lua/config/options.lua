-- Options are automatically loaded before lazy.nvim startup
-- Default options that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/options.lua
-- Add any additional options here
vim.g.copilot_no_tab_map = true
vim.g.mapleader = ","
vim.g.maplocalleader = ","
vim.g.lazyvim_php_lsp = "intelephense"
vim.api.nvim_create_autocmd("FileType", {
  pattern = "php",
  callback = function()
    vim.bo.shiftwidth = 4
    vim.bo.tabstop = 4
    vim.bo.softtabstop = 4
    vim.bo.expandtab = true -- Use spaces instead of tabs
  end,
})
