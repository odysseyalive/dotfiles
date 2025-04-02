-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here
vim.keymap.set("n", "<leader>r", ":FzfLua resume<CR>", { desc = "Resume" })

-- orgmode
vim.keymap.set("n", "<leader>o", "<Nop>", { desc = "OrgMode" })

-- tables mode
vim.keymap.set("n", "<leader>t", "<Nop>", { desc = "TableMode" })
