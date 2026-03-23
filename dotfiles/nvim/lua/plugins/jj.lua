return {
	{
		"nicolasgb/jj.nvim",
		version = "*",
		cmd = "J",
		keys = {
			{ "<leader>gg", "<cmd>J diff<cr>", desc = "Jujutsu diff" },
			{ "<leader>gb", "<cmd>J annotate<cr>", desc = "Jujutsu blame" },
			{ "<leader>goo", "<cmd>Jbrowse<cr>", desc = "Jujutsu browse" },
			{ "<leader>gom", "<cmd>Jbrowse trunk()<cr>", desc = "Jujutsu browse trunk" },
		},
		config = function()
			vim.api.nvim_set_hl(0, "JJAnnotateId", { link = "Normal" })
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
