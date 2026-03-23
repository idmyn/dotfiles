return {
	{
		"mason-org/mason.nvim",
		opts = {},
	},

	{
		"neovim/nvim-lspconfig",
		lazy = false,
		dependencies = {
			"mason-org/mason.nvim",
		},
		config = function()
			vim.lsp.enable("tsgo")
			vim.lsp.enable("biome")

			-- Filter out specific diagnostics by code
			vim.lsp.handlers["textDocument/publishDiagnostics"] = function(_, result, ctx, config)
				result.diagnostics = vim.tbl_filter(function(d)
					return d.code ~= "assist/source/organizeImports"
				end, result.diagnostics)
				vim.lsp.diagnostic.on_publish_diagnostics(_, result, ctx, config)
			end

			vim.api.nvim_create_autocmd("LspAttach", {
				group = vim.api.nvim_create_augroup("my.lsp", {}),
				callback = function(ev)
					local client = vim.lsp.get_client_by_id(ev.data.client_id)
					if not client then
						return
					end
					local opts = { buffer = ev.buf }
					vim.keymap.set("n", "gd", vim.lsp.buf.definition, opts)
					vim.keymap.set("n", "K", vim.lsp.buf.hover, opts)
					vim.keymap.set("n", "<leader>cr", vim.lsp.buf.rename, opts)
					vim.keymap.set("n", "<leader>ca", vim.lsp.buf.code_action, opts)
					vim.keymap.set("n", "gh", function()
						local diags = vim.diagnostic.get(0, { lnum = vim.api.nvim_win_get_cursor(0)[1] - 1 })
						if #diags > 0 then
							vim.diagnostic.open_float()
						else
							vim.lsp.buf.hover()
						end
					end, opts)
				end,
			})
		end,
	},
}
