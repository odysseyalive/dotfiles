-- Options are automatically loaded before lazy.nvim startup
-- Default options that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/options.lua
-- Add any additional options here
vim.g.copilot_no_tab_map = true
vim.g.mapleader = ","
vim.g.maplocalleader = ","
vim.g.lazyvim_php_lsp = "intelephense"
vim.g.intelephense_license_key = "YOUR-INTELEPHENSE-KEY"
vim.api.nvim_create_autocmd("FileType", {
  pattern = "php",
  callback = function()
    vim.bo.shiftwidth = 4
    vim.bo.tabstop = 4
    vim.bo.softtabstop = 4
    vim.bo.expandtab = true -- Use spaces instead of tabs
  end,
})

-- Performance optimizations for large projects
vim.opt.updatetime = 300 -- Faster completion (default 4000ms)
vim.opt.timeoutlen = 400 -- Faster key sequence completion

-- Limit syntax highlighting for large files
vim.opt.synmaxcol = 500 -- Only highlight first 500 columns

-- Improve matchparen performance
vim.g.matchparen_timeout = 20
vim.g.matchparen_insert_timeout = 20

-- Better wildignore (respects .gitignore but adds common patterns)
vim.opt.wildignore:append({
  "*/node_modules/*",
  "*/.git/*",
  "*/vendor/*",
  "*/sessions/*",
  "*/tmp/*",
  "*/cache/*",
  "*.so",
  "*.swp",
  "*.zip",
  "*/dist/*",
  "*/build/*",
})
