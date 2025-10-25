return {
  "nvim-tree/nvim-tree.lua",
  version = "*",
  lazy = false,
  dependencies = {
    "nvim-tree/nvim-web-devicons",
  },
  keys = {
    { "<leader>e", "<cmd>NvimTreeToggle<cr>", desc = "File Explorer" },
    { "<leader>E", "<cmd>NvimTreeFindFile<cr>", desc = "Find File in Explorer" },
  },
  config = function()
    -- Disable netrw
    vim.g.loaded_netrw = 1
    vim.g.loaded_netrwPlugin = 1

    require("nvim-tree").setup({
      -- Sync with current file but don't auto-open
      sync_root_with_cwd = true,
      respect_buf_cwd = true,
      update_focused_file = {
        enable = false, -- Don't auto-follow (expensive in large projects)
        update_root = false,
      },

      -- Performance: disable file watchers for large projects
      filesystem_watchers = {
        enable = false, -- Manual refresh with R key
      },

      -- View settings
      view = {
        width = 40,
        side = "left",
        preserve_window_proportions = false,
        number = false,
        relativenumber = false,
        signcolumn = "yes",
      },

      -- Renderer settings
      renderer = {
        group_empty = true, -- Group empty folders
        highlight_git = true, -- Highlight files based on git status
        highlight_opened_files = "none",
        highlight_modified = "none",
        root_folder_label = ":~:s?$?/..?",
        indent_width = 2,
        indent_markers = {
          enable = true,
          inline_arrows = true,
          icons = {
            corner = "└",
            edge = "│",
            item = "│",
            bottom = "─",
            none = " ",
          },
        },
        icons = {
          webdev_colors = true,
          git_placement = "before",
          modified_placement = "after",
          show = {
            file = true,
            folder = true,
            folder_arrow = true,
            git = true,
            modified = true,
          },
          glyphs = {
            default = "",
            symlink = "",
            bookmark = "󰆤",
            modified = "●",
            folder = {
              arrow_closed = "",
              arrow_open = "",
              default = "",
              open = "",
              empty = "",
              empty_open = "",
              symlink = "",
              symlink_open = "",
            },
            git = {
              unstaged = "✗",
              staged = "✓",
              unmerged = "",
              renamed = "➜",
              untracked = "★",
              deleted = "",
              ignored = "◌",
            },
          },
        },
      },

      -- Git integration - ENABLED
      git = {
        enable = true,
        show_on_dirs = true,
        show_on_open_dirs = true,
        disable_for_dirs = {},
        timeout = 400, -- Timeout for git queries (ms)
      },

      -- Modified files
      modified = {
        enable = true,
        show_on_dirs = true,
        show_on_open_dirs = true,
      },

      -- Filters
      filters = {
        enable = true,
        dotfiles = false,
        git_clean = false,
        no_buffer = false,
        custom = {
          "^\\.git$",
          "node_modules",
        },
        exclude = {},
      },

      -- Actions
      actions = {
        use_system_clipboard = true,
        change_dir = {
          enable = true,
          global = false,
          restrict_above_cwd = false,
        },
        open_file = {
          quit_on_open = false,
          resize_window = true,
          window_picker = {
            enable = true,
          },
        },
      },

      -- Diagnostics
      diagnostics = {
        enable = false, -- Disable LSP diagnostics in tree (performance)
      },

      -- Performance: exclude massive directories
      filters = {
        custom = {
          "^\\.git$",
          "^node_modules$",
          "^sessions$",
          "^tmp$",
          "^cache$",
          "^backups$",
          "^email-attachments$",
        },
      },

      -- Sort
      sort = {
        sorter = "case_sensitive",
        folders_first = true,
      },

      -- Trash
      trash = {
        cmd = "trash",
      },
    })
  end,
}
