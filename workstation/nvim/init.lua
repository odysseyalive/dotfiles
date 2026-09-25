-- `setup.sh tools` installs nvim, tree-sitter and starship into ~/.local/bin.
-- Put it first so plugins find those even when the shell rc didn't add it,
-- and so they win over older or broken copies further down PATH.
local local_bin = vim.fn.expand("~/.local/bin")
if not vim.startswith(vim.env.PATH or "", local_bin .. ":") then
  vim.env.PATH = local_bin .. ":" .. (vim.env.PATH or "")
end

-- bootstrap lazy.nvim, LazyVim and your plugins
require("config.lazy")
