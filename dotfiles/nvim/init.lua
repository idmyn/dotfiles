vim.g.vscode_clipboard = vim.g.vscode_clipboard or "unnamedplus"
vim.cmd([[
		set clipboard+=unnamedplus
]])

vim.opt.tabstop = 2
vim.opt.shiftwidth = 2
vim.opt.cursorline = true
vim.opt.signcolumn = "yes"
vim.opt.autoread = true
vim.opt.splitright = true
vim.opt.wrap = false
vim.opt.ignorecase = true
vim.opt.smartcase = true

vim.g.mapleader = " "
local map = vim.keymap.set

map("n", "<esc>", "<cmd>nohl<CR>")
map({ "n", "v" }, ";", ":")

map({ "n", "v" }, "H", "^")
map({ "n", "v" }, "L", "g_")

-- Option+arrow keys for word movement in insert mode
map("i", "<M-f>", "<Esc>ea")
map("i", "<M-b>", "<Esc>bi")
-- Emacs-style C-e/C-f in insert mode
map("i", "<C-e>", "<End>")
map("i", "<C-f>", "<Right>")
-- Option+backspace to delete word backwards
map("i", "<M-BS>", "<C-w>")

if vim.g.vscode then
	require("vscode-config")
	return
end

map("n", "<D-s>", "<cmd>w<CR>")
map("i", "<D-s>", "<cmd>w<CR>")
map("n", "<leader>fs", "<cmd>w<CR>", { desc = "save" })
map("n", "<leader>fy", function()
	local path = vim.fn.fnamemodify(vim.fn.expand("%"), ":~:.")
	vim.fn.setreg("+", path)
	vim.notify(path, vim.log.levels.INFO, { title = "Copied path" })
end, { desc = "yank path" })
map("n", "<leader>wv", "<cmd>vsplit<CR>")
map("n", "<leader>ws", "<cmd>split<CR>")
map("n", "<C-x>1", function()
	local cur = vim.api.nvim_get_current_win()
	for _, win in ipairs(vim.api.nvim_list_wins()) do
		if win ~= cur and vim.fn.getwininfo(win)[1].quickfix == 0 then
			vim.api.nvim_win_close(win, false)
		end
	end
end)
map("n", "<leader>`", "<cmd>b#<CR>")

map("n", "<D-/>", "gcc", { remap = true, desc = "Toggle comment" })
map("v", "<D-/>", "gc<Esc>", { remap = true, desc = "Toggle comment" })
map("n", "<C-j>", "*")
map("n", "<C-k>", "#")

map("n", "<leader>tl", "<cmd>set number!<CR>")
map("n", "<C-n>", "<cmd>cnext<CR>")
map("n", "<C-p>", "<cmd>cprev<CR>")

-- When exiting Neovim, switch Zellij back to normal mode (for zellij-autolock)
vim.api.nvim_create_autocmd("VimLeave", {
	pattern = "*",
	command = "silent !zellij action switch-mode normal",
})

-- Bootstrap lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
	local lazyrepo = "https://github.com/folke/lazy.nvim.git"
	local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
	if vim.v.shell_error ~= 0 then
		vim.api.nvim_echo({
			{ "Failed to clone lazy.nvim:\n", "ErrorMsg" },
			{ out, "WarningMsg" },
			{ "\nPress any key to exit..." },
		}, true, {})
		vim.fn.getchar()
		os.exit(1)
	end
end
vim.opt.rtp:prepend(lazypath)

-- Make sure to setup `mapleader` and `maplocalleader` before
-- loading lazy.nvim so that mappings are correct.
-- This is also a good place to setup other settings (vim.opt)
vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

-- Setup lazy.nvim
require("lazy").setup({
	spec = {
		{ import = "plugins" },
		{
			"nvim-treesitter/nvim-treesitter",
			lazy = false,
			build = ":TSUpdate",
			config = function()
				require("nvim-treesitter").install({ "lua", "javascript", "typescript", "tsx", "html" })
				vim.api.nvim_create_autocmd("FileType", {
					callback = function(args)
						pcall(vim.treesitter.start, args.buf)
					end,
				})
			end,
		},
	},
	install = { colorscheme = { "solarized" } },
	-- don't automatically check for plugin updates
	checker = { enabled = false },
})
