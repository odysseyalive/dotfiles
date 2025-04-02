return {
  "CopilotC-Nvim/CopilotChat.nvim",
  branch = "main",
  cmd = "CopilotChat",
  opts = function()
    return {
      context = {
        -- "buffers:listed", -- Also include listed buffers
        "project", -- Use project files with smart filtering
      },
    }
  end,
  config = function(_, opts)
    local chat = require("CopilotChat")
    chat.setup(opts)
  end,
}
