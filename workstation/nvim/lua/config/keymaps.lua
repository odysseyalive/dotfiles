-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here
vim.keymap.set("n", "<leader>r", ":FzfLua resume<CR>", { desc = "Resume" })

-- orgmode
vim.keymap.set("n", "<leader>o", "<Nop>", { desc = "OrgMode" })

-- tables mode
vim.keymap.set("n", "<leader>t", "<Nop>", { desc = "TableMode" })

-- Copilot Chat keymaps
vim.keymap.set("n", "<leader>a", "<Nop>", { desc = "Copilot" })
vim.keymap.set({ "n", "v" }, "<leader>ac", ":CopilotChat<CR>", { desc = "Open Prompt" })
vim.keymap.set("n", "<leader>ae", ":CopilotChatExplain<CR>", { desc = "Explain code" })
vim.keymap.set("n", "<leader>at", ":CopilotChatTests<CR>", { desc = "Generate tests" })
vim.keymap.set("n", "<leader>ar", ":CopilotChatReview<CR>", { desc = "Review code" })
vim.keymap.set("n", "<leader>af", ":CopilotChatFix<CR>", { desc = "Fix code" })
vim.keymap.set("n", "<leader>ai", ":CopilotChatImplement<CR>", { desc = "Implement code" })
vim.keymap.set("n", "<leader>ad", ":CopilotChatDebug<CR>", { desc = "Debug code" })
vim.keymap.set("n", "<leader>ao", ":CopilotChatOptimize<CR>", { desc = "Optimize code" })
vim.keymap.set("v", "<leader>ae", ":CopilotChatExplain<CR>", { desc = "Explain selected code" })
vim.keymap.set("v", "<leader>ar", ":CopilotChatReview<CR>", { desc = "Review selected code" })
vim.keymap.set("v", "<leader>at", ":CopilotChatTests<CR>", { desc = "Generate tests for selected code" })
