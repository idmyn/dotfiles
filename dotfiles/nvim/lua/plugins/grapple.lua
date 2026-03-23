return {
	"cbochs/grapple.nvim",
	opts = {
		scope = "jj_bookmark",
		scopes = {
			{
				name = "jj_bookmark",
				desc = "Jujutsu repo root and current bookmark",
				fallback = "cwd",
				cache = { event = { "BufEnter", "FocusGained" }, debounce = 500 },
				resolver = function()
					local jj_dirs = vim.fs.find(".jj", {
						upward = true,
						stop = vim.loop.os_homedir(),
						type = "directory",
					})
					if #jj_dirs == 0 then
						return nil, nil, "not in a jj repository"
					end
					local root = vim.fn.fnamemodify(jj_dirs[1], ":h")
					local result = vim.fn.system({
						"jj",
						"log",
						"--no-graph",
						"--revisions",
						"closest_bookmark(@)",
						"--template",
						"bookmarks",
					})
					if vim.v.shell_error ~= 0 then
						return nil, nil, "jj command failed"
					end
					local bookmark = vim.trim(string.gsub(result, "\n", " ")):gsub("%*$", "")
					local id = bookmark ~= "" and string.format("%s:%s", root, bookmark) or root
					return id, root
				end,
			},
		},
	},
	keys = {
		{ "<leader>M", "<cmd>Grapple toggle<cr>", desc = "Grapple: toggle tag" },
		--{ "<leader>n", "<cmd>Grapple cycle_tags next<cr>", desc = "Grapple: next tag" },
		--{ "<leader>p", "<cmd>Grapple cycle_tags prev<cr>", desc = "Grapple: prev tag" },
		--{ "<leader>1", "<cmd>Grapple select index=1<cr>", desc = "Grapple: tag 1" },
		--{ "<leader>2", "<cmd>Grapple select index=2<cr>", desc = "Grapple: tag 2" },
		--{ "<leader>3", "<cmd>Grapple select index=3<cr>", desc = "Grapple: tag 3" },
		--{ "<leader>4", "<cmd>Grapple select index=4<cr>", desc = "Grapple: tag 4" },
	},
}
