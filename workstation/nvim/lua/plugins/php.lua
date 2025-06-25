return {
  -- Disable PHPCS linter that comes with PHP extras
  {
    "mfussenegger/nvim-lint",
    optional = true,
    opts = {
      linters_by_ft = {
        php = {}, -- Empty table = no linters for PHP
      },
    },
  },
}
