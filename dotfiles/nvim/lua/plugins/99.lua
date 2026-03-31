return {
	"ThePrimeagen/99",
	dependencies = {
		{ "saghen/blink.compat", version = "2.*" },
	},
	keys = {
		{
			"<leader>9v",
			function()
				require("99").visual()
			end,
			mode = "v",
			desc = "99: visual replace",
		},
		{
			"<leader>9x",
			function()
				require("99").stop_all_requests()
			end,
			desc = "99: stop all requests",
		},
		{
			"<leader>9s",
			function()
				require("99").search()
			end,
			desc = "99: search",
		},
	},
	config = function()
		require("99").setup({
			provider = require("99").Providers.ClaudeCodeProvider,
			model = "claude-sonnet-4-6",
			completion = {
				source = "blink",
			},
		})
	end,
}
