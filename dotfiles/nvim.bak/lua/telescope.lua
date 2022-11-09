require('paq-nvim').paq {'nvim-telescope/telescope.nvim'}

vim.api.nvim_set_keymap('n', '<leader><SPACE>', '<cmd>Telescope find_files<cr>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>bb', '<cmd>Telescope buffers<cr>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>sp', '<cmd>Telescope live_grep<cr>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>fr', '<cmd>Telescope oldfiles<cr>', { noremap = true, silent = true })
