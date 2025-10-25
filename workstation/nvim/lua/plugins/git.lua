return {
  "lewis6991/gitsigns.nvim",
  opts = {
    max_file_length = 10000, -- Disable for files > 10k lines
    update_debounce = 200, -- Update after 200ms of no typing
    -- Only show signs, disable line blame (expensive)
    current_line_blame = false,
  },
}
