-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here
vim.keymap.set("n", "<leader>r", ":FzfLua resume<CR>", { desc = "Resume" })

-- orgmode
vim.keymap.set("n", "<leader>o", "<Nop>", { desc = "OrgMode" })

-- tables mode
vim.keymap.set("n", "<leader>t", "<Nop>", { desc = "TableMode" })

-- Performance profiling
vim.keymap.set("n", "<leader>tp", function()
  vim.cmd("profile start /tmp/nvim-profile.log")
  vim.cmd("profile func *")
  vim.cmd("profile file *")
  vim.notify("Profiling started. Use <leader>tP to stop.", vim.log.levels.INFO)
end, { desc = "Start profiling" })

vim.keymap.set("n", "<leader>tP", function()
  vim.cmd("profile stop")
  vim.cmd("e /tmp/nvim-profile.log")
  vim.notify("Profiling stopped. Check buffer for results.", vim.log.levels.INFO)
end, { desc = "Stop profiling" })
