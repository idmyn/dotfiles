return {
	"nvim-neotest/neotest",
	dependencies = {
		"nvim-neotest/nvim-nio",
		"nvim-lua/plenary.nvim",
		"antoinemadec/FixCursorHold.nvim",
		"marilari88/neotest-vitest",
	},
	keys = {
		{
			"<leader>tn",
			function()
				require("neotest").output_panel.clear()
				require("neotest").run.run()
				require("neotest").output_panel.toggle()
			end,
			desc = "Run nearest test",
		},
		{
			"<leader>tf",
			function()
				require("neotest").output_panel.clear()
				require("neotest").run.run(vim.fn.expand("%"))
				require("neotest").output_panel.toggle()
			end,
			desc = "Run current file",
		},
		{
			"<leader>ts",
			function()
				require("neotest").summary.toggle()
			end,
			desc = "Toggle test summary",
		},
		-- {
		-- 	"<leader>to",
		-- 	function()
		-- 		require("neotest").output.open({ enter = false })
		-- 	end,
		-- 	desc = "Show test output",
		-- },
		{
			"<leader>to",
			function()
				require("neotest").output_panel.toggle()
			end,
			desc = "Toggle output panel",
		},
		{
			"<leader>ta",
			function()
				require("neotest").run.attach()
			end,
			desc = "Attach to running test",
		},
		{
			"<leader>tS",
			function()
				require("neotest").run.stop()
			end,
			desc = "Stop running tests",
		},
	},
	config = function()
		-- Map path patterns to specific vitest config filenames and optional extra flags
		local vitest_overrides = {
			["api/integration-tests"] = { config = "vitest.integration.config.ts" },
			["web-features"] = { config = "vitest.config.mts", flags = "--browser.headless" },
		}

		require("neotest").setup({
			adapters = {
				require("neotest-vitest")({
					vitestCommand = function(file)
						for pattern, override in pairs(vitest_overrides) do
							if file:find(pattern, 1, true) and override.flags then
								return "node_modules/.bin/vitest " .. override.flags
							end
						end
						return "node_modules/.bin/vitest"
					end,
					-- Run vitest from the directory containing the config
					cwd = function(file)
						local path = vim.fn.fnamemodify(file, ":h")
						while path ~= "/" do
							local matches = vim.fn.glob(path .. "/vitest*.config.*", false, true)
							if #matches > 0 then
								return path
							end
							path = vim.fn.fnamemodify(path, ":h")
						end
						return vim.loop.cwd()
					end,
					-- Walk up from test file to find the nearest vitest config
					vitestConfigFile = function(file)
						local path = vim.fn.fnamemodify(file, ":h")
						while path ~= "/" do
							local matches = vim.fn.glob(path .. "/vitest*.config.*", false, true)
							if #matches == 1 then
								return matches[1]
							elseif #matches > 1 then
								-- Check overrides to disambiguate
								for pattern, override in pairs(vitest_overrides) do
									if file:find(pattern, 1, true) then
										local config = path .. "/" .. override.config
										if vim.fn.filereadable(config) == 1 then
											return config
										end
									end
								end
								vim.notify(
									"Multiple vitest configs found in " .. path .. ":\n" .. table.concat(matches, "\n"),
									vim.log.levels.ERROR
								)
								return nil
							end
							path = vim.fn.fnamemodify(path, ":h")
						end
						return nil
					end,
					filter_dir = function(name)
						return name ~= "node_modules"
							and name ~= "dist"
							and name ~= ".git"
							and name ~= ".next"
							and name ~= ".vite"
					end,
				}),
			},
			discovery = {
				concurrent = 2,
				enabled = true,
			},
			--output = { open_on_run = false },
			status = {
				signs = false,
			},
		})
	end,
}
