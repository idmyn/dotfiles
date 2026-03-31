local function wide_vertical(preview_height)
	return {
		layout = {
			backdrop = false,
			width = 0.8,
			min_width = 80,
			height = 0.8,
			min_height = 30,
			box = "vertical",
			border = true,
			title = "{title} {live} {flags}",
			title_pos = "center",
			{ win = "input", height = 1, border = "bottom" },
			{ win = "list", border = "none" },
			{ win = "preview", title = "{preview}", height = preview_height, border = "top" },
		},
	}
end

return {
	"folke/snacks.nvim",
	priority = 1000,
	lazy = false,
	---@type snacks.Config
	opts = {
		picker = {
			enabled = true,
			formatters = {
				file = {
					truncate = "left",
				},
			},
		},
	},
	keys = {
		{
			"gr",
			function()
				Snacks.picker.lsp_references({ layout = wide_vertical(0.8) })
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
			"<leader>pp",
			function()
				Snacks.picker.projects()
			end,
			desc = "projects",
		},
		{
			"<leader>fr",
			function()
				Snacks.picker.recent({ layout = wide_vertical(0.6) })
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
