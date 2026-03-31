return {
	"folke/which-key.nvim",
	event = "VeryLazy",
	opts = {
		preset = "helix",
		spec = {
			{ "<leader>f", group = "file" },
			{ "<leader>g", group = "git" },
			{ "<leader>h", group = "haunt" },
			{ "<leader>q", group = "quickfix" },
			{ "<leader>s", group = "search" },
			{ "<leader>t", group = "toggle" },
			{ "<leader>w", group = "window" },
			{ "<leader>9", group = "99" },
		},
	},
}
