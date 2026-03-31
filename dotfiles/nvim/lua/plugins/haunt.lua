return {
	"TheNoeTrevino/haunt.nvim",
	opts = {
		storage_id = function()
			local root = vim.trim(vim.fn.system({ "jj", "root" }))
			if vim.v.shell_error ~= 0 then
				return nil
			end
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
				return root
			end
			local bookmark = vim.trim(string.gsub(result, "\n", " ")):gsub("%*$", "")
			if bookmark ~= "" then
				return root .. "|" .. bookmark
			end
			return root
		end,
	},
	init = function()
		local haunt = require("haunt.api")
		local prefix = "<leader>h"
		local map = vim.keymap.set

		map("n", prefix .. "a", haunt.annotate, { desc = "Haunt: annotate" })
		map("n", prefix .. "d", haunt.delete, { desc = "Haunt: delete bookmark" })
		map("n", prefix .. "y", haunt.yank_locations, { desc = "Haunt: yank all bookmarks" })
		map("n", prefix .. "h", function()
			require("haunt.picker").show()
		end, { desc = "Haunt: list bookmarks" })

		vim.api.nvim_create_user_command("HauntStorageId", function()
			local config = require("haunt").get_config()
			if config.storage_id then
				local ok, id = pcall(config.storage_id)
				if ok then
					vim.notify("storage_id: " .. vim.inspect(id), vim.log.levels.INFO)
				else
					vim.notify("storage_id error: " .. tostring(id), vim.log.levels.ERROR)
				end
			else
				vim.notify("No custom storage_id configured", vim.log.levels.INFO)
			end
			local path = require("haunt.persistence").get_storage_path()
			vim.notify("storage path: " .. tostring(path), vim.log.levels.INFO)
		end, { desc = "Show Haunt storage ID and path" })
	end,
}
