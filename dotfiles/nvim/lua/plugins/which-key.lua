return {
	"folke/which-key.nvim",
	event = "VeryLazy",
	opts = {
		preset = "helix",
		spec = {
			{ "<leader>f", group = "file" },
			{ "<leader>g", group = "git" },
			{ "<leader>q", group = "quickfix" },
			{ "<leader>s", group = "search" },
			{ "<leader>t", group = "toggle" },
			{ "<leader>w", group = "window" },
		},
	},
}
