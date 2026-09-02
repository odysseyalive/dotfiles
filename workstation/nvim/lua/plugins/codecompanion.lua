-- CodeCompanion: chat + inline + agentic editing, powered by OpenRouter.
-- Replaces CopilotChat for chat/agentic work; Copilot (copilot.lua) still handles
-- inline ghost-text completion via blink-copilot. Requires the OPENROUTER_API_KEY
-- environment variable (e.g. exported from your shell profile / secret manager).
return {
  "olimorris/codecompanion.nvim",
  dependencies = {
    "nvim-lua/plenary.nvim",
    "nvim-treesitter/nvim-treesitter",
  },
  cmd = { "CodeCompanion", "CodeCompanionChat", "CodeCompanionActions", "CodeCompanionCmd" },
  keys = {
    { "<leader>aa", "<cmd>CodeCompanionActions<cr>", mode = { "n", "v" }, desc = "CodeCompanion Actions" },
    { "<leader>ac", "<cmd>CodeCompanionChat Toggle<cr>", mode = { "n", "v" }, desc = "CodeCompanion Chat" },
    { "<leader>ai", "<cmd>CodeCompanion<cr>", mode = { "n", "v" }, desc = "CodeCompanion Inline" },
    { "ga", "<cmd>CodeCompanionChat Add<cr>", mode = "v", desc = "Add selection to CodeCompanion chat" },
  },
  opts = {
    adapters = {
      http = {
        -- Built-in OpenRouter adapter: api_key defaults to $OPENROUTER_API_KEY,
        -- model list is fetched live, so any OpenRouter model is selectable at runtime
        -- (open a chat, press the adapter/model picker). Change the default below as desired.
        openrouter = function()
          return require("codecompanion.adapters").extend("openrouter", {
            schema = {
              model = {
                default = "anthropic/claude-sonnet-4.5", -- e.g. "anthropic/claude-sonnet-5", "openai/gpt-5"
              },
            },
          })
        end,
      },
    },
    interactions = {
      chat = { adapter = "openrouter" },
      inline = { adapter = "openrouter" },
      cmd = { adapter = "openrouter" },
    },
  },
}
