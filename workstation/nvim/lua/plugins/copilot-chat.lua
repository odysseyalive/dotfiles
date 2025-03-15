return {
  {
    "CopilotC-Nvim/CopilotChat.nvim",
    dependencies = {
      { "zbirenbaum/copilot.vim" }, -- or zbirenbaum/copilot.lua
      { "nvim-lua/plenary.nvim", branch = "master" }, -- for curl, log and async functions
    },
    build = "make tiktoken", -- Only on MacOS or Linux
    opts = {
      context = {
        "files", -- Include all project files
        "buffers:listed", -- Also include listed buffers
      },
      -- You can also add other configuration options here
      show_help = true,
      window = {
        layout = "vertical",
        width = 0.5,
        height = 0.5,
      },
    },
    config = function(_, opts)
      require("CopilotChat").setup(opts)
    end,
  },
}
