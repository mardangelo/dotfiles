-- bootstrap lazy.nvim, LazyVim and your plugins
require("config.lazy")

vim.api.nvim_set_keymap("n", "<C-g>", "<Esc>", { noremap = true })
vim.api.nvim_set_keymap("i", "<C-g>", "<Esc>", { noremap = true })
vim.api.nvim_set_keymap("v", "<C-g>", "<Esc>", { noremap = true })
