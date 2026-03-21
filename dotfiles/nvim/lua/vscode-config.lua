local map = vim.keymap.set

local function vscode_action(action)
  return string.format("<cmd>lua require('vscode').action('%s')<CR>", action)
end

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
