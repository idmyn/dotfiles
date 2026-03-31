return {
	{
		"NicholasZolton/neojj",
		lazy = true,
		dependencies = {
			"nvim-lua/plenary.nvim",
		},
		cmd = "Neojj",
		keys = {
			{ "<leader>gg", "<cmd>Neojj<cr>", desc = "Show Neojj UI" },
		},
		opts = {
			highlight = {
				red = "#DC322F",
				orange = "#CB4B16",
				yellow = "#B58900",
				green = "#5a7400",
				cyan = "#2AA198",
				blue = "#268BD2",
				purple = "#6C71C4",
				line_red = "#f8d7da",
				line_green = "#d4edda",
			},
		},
	},
	{
		"esmuellert/codediff.nvim",
		cmd = "CodeDiff",
		opts = {
			explorer = {
				initial_focus = "modified",
			},
			keymaps = {
				view = {
					close_on_open_in_prev_tab = true,
					next_file = "<C-j>",
					prev_file = "<C-k>",
				},
			},
			highlights = {
				line_insert = "#d4edda",
				line_delete = "#f8d7da",
				char_insert = "#a6d9ab",
				char_delete = "#e6a8b0",
			},
		},
	},
}
