-- Base theme declaration (SeaShells, provided by the kitty-themes.nvim colorscheme).
-- On an Omarchy machine this file is replaced by a symlink to the current Omarchy
-- theme's neovim.lua (~/.local/state/omarchy/current/theme/neovim.lua), so theme
-- switching is managed by Omarchy; see all-themes.lua + omarchy-theme-hotreload.lua.
-- Off Omarchy, this static file is the fallback that keeps SeaShells available.
return {
	{
		"odysseyalive/kitty-themes.nvim",
		priority = 1000,
		config = function()
			require("kitty-themes").setup({
				transparent = true,
				term_colors = true,
			})
		end,
	},
	{
		"LazyVim/LazyVim",
		opts = {
			colorscheme = "SeaShells",
		},
	},
}
