return {
	"folke/snacks.nvim",
	priority = 1000,
	lazy = false,
	---@type snacks.Config
	opts = {
		picker = { enabled = true },
	},
	keys = {
		{
			"gr",
			function()
				Snacks.picker.lsp_references()
			end,
			nowait = true,
			desc = "References",
		},
		{
			"<leader>gd",
			function()
				Snacks.picker.git_diff()
			end,
			desc = "Git Diff (Hunks)",
		},
		{
			"<leader>sd",
			function()
				Snacks.picker.diagnostics()
			end,
			desc = "Diagnostics",
		},
		{
			"<leader>fr",
			function()
				Snacks.picker.recent()
			end,
			desc = "recent",
		},
		{
			"<leader>m",
			function()
				local tags, err = require("grapple").tags()
				if not tags then
					vim.notify(err or "No tags", vim.log.levels.WARN)
					return
				end
				local items = {}
				for i, tag in ipairs(tags) do
					local cursor = tag.cursor or { 1, 0 }
					table.insert(items, {
						text = tag.name or tag.path,
						file = tag.path,
						pos = { cursor[1], cursor[2] },
						idx = i,
						label = tag.name,
					})
				end
				local app = require("grapple").app()
				local scope_name = app.settings.scope
				local resolved = app:current_scope()
				local bookmark = resolved and resolved.id:match(":(.+)$") or nil
				local title = "Grapple Tags [" .. scope_name .. (bookmark and " (" .. bookmark .. ")" or "") .. "]"
				Snacks.picker({
					title = title,
					items = items,
					format = function(item)
						local name = item.label or vim.fn.fnamemodify(item.file, ":t")
						return {
							{ string.format("[%d] ", item.idx), "SnacksPickerIdx" },
							{ name },
							{ ":", "SnacksPickerDelim" },
							{ tostring(item.pos[1]), "SnacksPickerRow" },
							{ ":", "SnacksPickerDelim" },
							{ tostring(item.pos[2]), "SnacksPickerCol" },
						}
					end,
					preview = "file",
					confirm = function(picker, item)
						picker:close()
						require("grapple").select({ path = item.file })
					end,
				})
			end,
			desc = "Grapple: pick tags",
		},
	},
}
