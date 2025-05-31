vim.g.vscode_clipboard = vim.g.vscode_clipboard or "unnamedplus"
vim.cmd([[
		set clipboard+=unnamedplus
		]])

vim.opt.ignorecase = true
vim.opt.smartcase = true

vim.g.mapleader = " "
local map = vim.keymap.set

local function vscode_action(action)
    return string.format("<cmd>lua require('vscode').action('%s')<CR>", action)
end

map("n", "<esc>", "<cmd>nohl<CR>")

map({ "n", "v" }, "H", "^")
map({ "n", "v" }, "L", "g_")

-- Option+arrow keys for word movement in insert mode
map("i", "<M-f>", "<Esc>ea")
map("i", "<M-b>", "<Esc>bi")

if vim.g.vscode then
    -- https://github.com/vscode-neovim/vscode-neovim/issues/1139
    map("n", "u", "<Cmd>call VSCodeNotify('undo')<CR>")
    map("n", "<C-r>", "<Cmd>call VSCodeNotify('redo')<CR>")

    map("n", "<M-h>", vscode_action("workbench.action.focusLeftGroup"))
    map("n", "<M-l>", vscode_action("workbench.action.focusRightGroup"))
    map("n", "<M-j>", vscode_action("workbench.action.focusBelowGroup"))
    map("n", "<M-k>", vscode_action("workbench.action.focusAboveGroup"))

    map("n", "<C-j>", vscode_action("editor.action.wordHighlight.next"))
    map("n", "<C-k>", vscode_action("editor.action.wordHighlight.prev"))

    map("n", "<leader> ", vscode_action("workbench.action.quickOpen"))
    map("n", "<leader>fs", vscode_action("workbench.action.files.save"))
    map("n", "<leader>wv", vscode_action("workbench.action.splitEditorRight"))
    map("n", "<leader>ws", vscode_action("workbench.action.splitEditorDown"))
    map({ "n", "v" }, "<leader>sp", vscode_action("search.action.openNewEditor"))
    map("n", "<leader>gr", vscode_action("editor.action.referenceSearch.trigger"))
    map("n", "<leader>goo", vscode_action("gitlens.openFileOnRemote"))
    map("n", "<leader>pp", vscode_action("projectManager.listProjects"))
    map("n", "<leader>tz", vscode_action("workbench.action.toggleCenteredLayout"))
    map("n", "<leader>tb", vscode_action("gitlens.toggleFileBlame"))
    map("n", "<leader>tc", vscode_action("editor.cpp.toggle"))
    map("n", "<leader>tp", vscode_action("workbench.actions.view.problems"))

    map("n", "<leader>cr", vscode_action("editor.action.rename"))

    map("n", "]q", vscode_action("go-to-next-error.nextInFiles.error"))
    map("n", "[q", vscode_action("go-to-next-error.prevInFiles.error"))

    map("n", "<C-x>",
        vscode_action("workbench.action.closeEditorsInOtherGroups") ..
        vscode_action("workbench.action.closeOtherEditors"))
    map("i", "<C-x>",
        vscode_action("workbench.action.closeEditorsInOtherGroups") ..
        vscode_action("workbench.action.closeOtherEditors"))
end


-- Bootstrap lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
    local lazyrepo = "https://github.com/folke/lazy.nvim.git"
    local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
    if vim.v.shell_error ~= 0 then
        vim.api.nvim_echo({
            { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
            { out,                            "WarningMsg" },
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
    checker = { enabled = true, frequency = 604800 },
    spec = {
        {
            "nvim-treesitter/nvim-treesitter",
            build = ":TSUpdate",
            config = function()
                local configs = require("nvim-treesitter.configs")

                configs.setup({
                    ensure_installed = { "lua", "javascript", "typescript", "tsx", "html" },
                    sync_install = false,
                    highlight = { enable = false },
                    indent = { enable = false },
                })
            end
        },
        {
            'gsuuon/tshjkl.nvim',
            opts = {
                keymaps = {
                    toggle = '<leader>tt',
                },
            }
        },
        {
            "idmyn/eink.vim",
            priority = 1000,
            config = function()
                vim.cmd('colorscheme eink')
            end
        }
    },
    install = { colorscheme = { "eink" } },
    -- don't automatically check for plugin updates
    checker = { enabled = false },
})
