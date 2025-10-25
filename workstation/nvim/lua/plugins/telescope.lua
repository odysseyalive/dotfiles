return {
  "nvim-telescope/telescope.nvim",
  opts = {
    defaults = {
      -- Respect .gitignore and add common patterns
      file_ignore_patterns = {
        "^.git/",
        "node_modules/",
        "vendor/",
        "%.jpg",
        "%.jpeg",
        "%.png",
        "%.gif",
        "%.webp",
        "%.svg",
        "%.ico",
        "%.pdf",
        "%.zip",
        "%.tar",
        "%.tar.gz",
        "%.min.js",
        "%.min.css",
      },
      -- Faster sorting
      sorting_strategy = "ascending",
      layout_config = {
        prompt_position = "top",
      },
      -- Don't preview binary files
      preview = {
        filesize_limit = 1, -- MB
        timeout = 250, -- ms
      },
      -- Limit results for performance
      cache_picker = {
        num_pickers = 10,
      },
    },
    pickers = {
      find_files = {
        -- Respects .gitignore by default
        hidden = false,
        follow = false, -- Don't follow symlinks
      },
      live_grep = {
        -- Limit grep results
        additional_args = function()
          return { "--max-count=1000" }
        end,
      },
    },
  },
}
