return {
  "stevearc/conform.nvim",
  opts = {
    formatters_by_ft = {
      php = { "php_cs_fixer" }, -- use PHP-CS-Fixer for PHP files
      -- php = { "php_cs_fixer", "phpcbf" },  -- (optional) run php-cs-fixer, then phpcbf
    },
    formatters = {
      ["php_cs_fixer"] = {
        command = "php-cs-fixer",
        args = { "fix", "--rules=@PSR12", "$FILENAME" }, -- Customize rules if needed
        stdin = false,
        env = {
          PHP_CS_FIXER_IGNORE_ENV = "1", -- Ignore PHP version requirement
        },
      },
    },
  },
}
