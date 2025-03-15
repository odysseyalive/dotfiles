-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here
vim.keymap.set("n", "<leader>r", ":FzfLua resume<CR>", { desc = "Resume" })

-- Copilot Chat keymaps
vim.keymap.set({ "n", "v" }, "<leader>ac", ":CopilotChat<CR>", { desc = "CopilotChat - Open Prompt" })
vim.keymap.set("n", "<leader>ae", ":CopilotChatExplain<CR>", { desc = "CopilotChat - Explain code" })
vim.keymap.set("n", "<leader>at", ":CopilotChatTests<CR>", { desc = "CopilotChat - Generate tests" })
vim.keymap.set("n", "<leader>ar", ":CopilotChatReview<CR>", { desc = "CopilotChat - Review code" })
vim.keymap.set("n", "<leader>af", ":CopilotChatFix<CR>", { desc = "CopilotChat - Fix code" })
vim.keymap.set("n", "<leader>ai", ":CopilotChatImplement<CR>", { desc = "CopilotChat - Implement code" })
vim.keymap.set("n", "<leader>ad", ":CopilotChatDebug<CR>", { desc = "CopilotChat - Debug code" })
vim.keymap.set("n", "<leader>ao", ":CopilotChatOptimize<CR>", { desc = "CopilotChat - Optimize code" })
vim.keymap.set("v", "<leader>ae", ":CopilotChatExplain<CR>", { desc = "CopilotChat - Explain selected code" })
vim.keymap.set("v", "<leader>ar", ":CopilotChatReview<CR>", { desc = "CopilotChat - Review selected code" })
vim.keymap.set("v", "<leader>at", ":CopilotChatTests<CR>", { desc = "CopilotChat - Generate tests for selected code" })
