vim.keymap.set("n", "<leader>qO", "<cmd>cdo badd %<cr>", { desc = "Add quickfix entries to buffer list" })
vim.keymap.set("n", "<leader>qq", function()
	require("quicker").toggle()
end, {
	desc = "Toggle quickfix",
})

local function remove_qf_item()
	local curqfidx = vim.fn.line(".") - 1
	local qfall = vim.fn.getqflist()
	table.remove(qfall, curqfidx + 1)
	vim.fn.setqflist(qfall, "r")
	vim.cmd((curqfidx + 1) .. "cfirst")
	vim.cmd("copen")
end

vim.api.nvim_create_autocmd("FileType", {
	pattern = "qf",
	callback = function()
		vim.keymap.set("n", "dd", remove_qf_item, { buffer = true })
	end,
})

return {
	{
		"kevinhwang91/nvim-bqf",
		ft = "qf",
		opts = {
			preview = {
				winblend = 0,
			},
		},
	},
	{
		"stevearc/quicker.nvim",
		ft = "qf",
		---@module "quicker"
		---@type quicker.SetupOptions
		opts = {},
		config = function()
			require("quicker").setup({
				keys = {
					{
						">",
						function()
							require("quicker").expand({ before = 2, after = 2, add_to_existing = true })
						end,
						desc = "Expand quickfix context",
					},
					{
						"<",
						function()
							require("quicker").collapse()
						end,
						desc = "Collapse quickfix context",
					},
				},
			})
		end,
	},
}
