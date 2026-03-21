return {
	{
		"nicolasgb/jj.nvim",
		version = "*",
		cmd = "J",
		keys = {
			{ "<leader>gg", "<cmd>J diff<cr>", desc = "Jujutsu diff" },
		},
		config = function()
			require("jj").setup({
				editor = {
					auto_insert = false,
				},
				diff = {
					backend = "codediff",
				},
			})
		end,
	},
	{
		"esmuellert/codediff.nvim",
		cmd = "CodeDiff",
		opts = {
			keymaps = {
				view = {
					close_on_open_in_prev_tab = true,
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
