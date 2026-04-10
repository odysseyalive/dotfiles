# LazyVim

LazyVim is a modern Neovim distribution that provides a blazing fast IDE experience with sensible defaults. This configuration builds on LazyVim's foundation with PHP development optimizations and carefully selected plugins for web development workflows.

```bash
mkdir -p ~/.config/nvim && rsync -azhLP ~/.yadrlite/workstation/lazyvim/ ~/.config/nvim
```

Requirements: [Neovim 0.9+](https://neovim.io/), [Git](https://git-scm.com/), [Node.js](https://nodejs.org/)

Plugin Requirements: [Ripgrep](https://github.com/BurntSushi/ripgrep), [fd](https://github.com/sharkdp/fd), [PHP CS Fixer](https://cs.symfony.com/), [Intelephense](https://intelephense.com/)

[Return to top](#yadrlite)

## LazyVim Shortcuts

LazyVim uses the comma (`,`) as the leader key, consistent with the YADR package. Most shortcuts are discoverable through the which-key popup that appears after pressing the leader key.

#### Core Navigation

- `,`: Opens which-key menu (shows all available shortcuts)
- `,,`: Command palette (equivalent to `:` in vim)
- `,r`: Resume last fuzzy finder search
- `Ctrl-h,j,k,l`: Navigate between splits and tmux panes
- `Ctrl-/`: Toggle terminal
- `<S-h>`,`<S-l>`: Switch between buffers

#### File Management

- `,ff`: Find files in project
- `,fr`: Recent files
- `,fb`: Browse file system
- `,fg`: Live grep in project
- `,fw`: Find word under cursor
- `,fc`: Find command
- `,fh`: Help tags
- `,fm`: File manager (neo-tree)

#### Buffer Operations

- `,bd`: Delete buffer
- `,bD`: Delete buffer and window
- `,bl`: Delete buffers to the left
- `,br`: Delete buffers to the right
- `,bo`: Delete other buffers
- `,bp`: Toggle pin buffer

#### LSP and Code Actions

- `gd`: Go to definition
- `gr`: Go to references
- `gi`: Go to implementation
- `K`: Show hover documentation
- `,ca`: Code actions
- `,cr`: Rename symbol
- `,cf`: Format document
- `,cd`: Line diagnostics
- `]d`,`[d`: Next/previous diagnostic

#### Git Integration

- `,gg`: LazyGit
- `,gb`: Git blame line
- `,gf`: LazyGit current file history
- `,gl`: Git log
- `,gL`: Git log (current file)
- `]h`,`[h`: Next/previous hunk
- `,ghs`: Stage hunk
- `,ghr`: Reset hunk
- `,ghp`: Preview hunk

#### Debug (DAP)

- `,db`: Toggle breakpoint
- `,dB`: Breakpoint condition
- `,dc`: Continue
- `,dC`: Run to cursor
- `,dg`: Go to line (no execute)
- `,di`: Step into
- `,dj`: Down in stacktrace
- `,dk`: Up in stacktrace
- `,dl`: Run last
- `,do`: Step out
- `,dO`: Step over
- `,dp`: Pause
- `,dr`: Toggle REPL
- `,ds`: Session
- `,dt`: Terminate
- `,dw`: Widgets

#### Window Management

- `,ww`: Other window
- `,wd`: Delete window
- `,w-`: Split window below
- `,w|`: Split window right
- `,wm`: Maximize toggle

#### Search and Replace

- `,sr`: Replace in files
- `,sR`: Replace in files (with confirmation)
- `,sw`: Search word under cursor
- `,sW`: Search word under cursor (exact match)

#### Project and Session

- `,fp`: Find files in config
- `,ft`: Terminal (root dir)
- `,fT`: Terminal (cwd)
- `,qq`: Quit all
- `,qs`: Session save
- `,qr`: Session restore

#### Plugin Management

- `,l`: Open Lazy plugin manager
- `,cm`: Mason (manage LSP servers, formatters, etc.)
- `,ci`: Lazy install
- `,cs`: Lazy sync
- `,cu`: Lazy update
- `,cx`: Lazy extras

#### AI and Copilot

- `,aa`: CopilotChat
- `,ae`: CopilotChat explain
- `,af`: CopilotChat fix
- `,ao`: CopilotChat optimize
- `,ad`: CopilotChat docs
- `,at`: CopilotChat tests
- `Tab`: Accept Copilot suggestion
- `Ctrl-]`: Dismiss Copilot suggestion

#### Table Mode (for Markdown/Org)

- `,tm`: Toggle table mode
- `,tt`: Tableize (convert CSV to table)
- `,tr`: Realign table
- `,tdd`: Delete table row
- `,tdc`: Delete table column

#### Org Mode

- `,oa`: Org agenda
- `,oc`: Org capture
- `,oe`: Org export
- `,oi`: Org clock in
- `,oo`: Org clock out
- `,ot`: Org todo

[Return to top](#yadrlite)

