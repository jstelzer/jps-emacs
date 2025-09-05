# My Emacs Configuration

This is my personal Emacs setup. It's portable (macOS + Linux), works in
GUI and terminal, and gives me a **modern IDE inside Emacs without the
kitchen sink**.

Clone it, run one script, and you've got:\
- A project dashboard on `C-x p` with test/build/deploy/vterm/notes
wired in\
- Dead-simple JSON formatters and timestamp helpers\
- A clean, straight.el--based package setup that Just Works™\
- An environment I can rebuild on a new machine in 5 minutes

------------------------------------------------------------------------

## Features (the real ones)

-   🚀 **Fast startup** -- straight.el + lean config = sub-second loads\
-   🧭 **Project navigation** -- jump into files, grep, magit, tests,
    builds, deploys, compose, and notes with `C-x p`\
-   🛠 **Language support** -- eglot + formatters for Go, Rust, Python,
    plus snippets, company, and flycheck\
-   🐍 **Python environment management** -- automatic pyenv virtualenv activation per project\
-   🧹 **Editing helpers** -- vi-style kill line, JSON pretty/flatten,
    whitespace cleanup, timestamp insert\
-   🔍 **Search everything** -- consult + ripgrep + vertico + orderless
    makes the minibuffer the center of the universe\
-   🎨 **Platform smarts** -- transparency + trackpad scroll on macOS,
    works in X11/Wayland, runs fine over SSH\
-   🤖 **Claude Code Menu** -- drop into AI-assisted coding from inside
    Emacs\
-   🌐 **REST client** -- built-in HTTP testing with token extraction and jq filtering\
-   🗑 **Auto-save cleanup** -- saves buffers, prunes old autosaves (not
    all of them, only crusty ones)

------------------------------------------------------------------------

## Quick Setup

``` bash
git clone <your-repo-url> ~/emacs-config
cd ~/emacs-config
./setup.sh
emacs      # GUI mode
emacs -nw  # Console mode
```

That's it. First run downloads all packages, compiles them, and makes
your CPU sound like it's mining Bitcoin. Don't panic --- only happens
once.

------------------------------------------------------------------------

## File Structure

    ~/emacs-config/
    ├── init.el              # Main Emacs config
    ├── personal/
    │   └── jps.el           # Custom functions and keybindings
    ├── setup.sh             # Symlink/bootstrap script
    ├── .gitignore           # Ignores caches & generated stuff
    └── README.md            # This file

------------------------------------------------------------------------

## Key Bindings

### Global Helpers

  Key        Action
  ---------- ---------------------------
  `C-c d`    Kill line (vi-style `dd`)
  `C-c j`    Pretty-print JSON region
  `C-c J`    Flatten JSON (safe)
  `C-c t`    Insert timestamp
  `C-c w`    Cleanup whitespace
  `C-c f d`  Diff buffer with file
  `C-c T`    Load manoj-dark theme
  `C-c L`    Load adwaita (light) theme
  `C-c F`    Toggle fullscreen
  `C-c X`    Toggle transparency
  `C-c W`    Toggle whitespace-mode
  `C-c a`    Align by regexp
  `C-c s`    Sort lines
  `C-c l`    Add change log entry
  `C-c x`    Configure platform settings
  `C-c p y`  Switch pyenv version
  `C-c C-'`  Claude Code IDE menu
  `C-c D`    Docker dashboard

### ChatGPT

how to use
    Key                 Action
   -----               ------------------------------------------------------------------------
   `C-c g v`           → splits the frame: vterm (left) + chatgpt-shell (right) in your project.
   `C-c g c`           → open a chat buffer anywhere.
   `C-c g r / C-c g b` → send region / whole buffer.
   `C-c g s`           → send region but keep your cursor where you were.

### Project Dashboard (`C-x p`)

These live under the built-in `project-prefix-map`:

  Key   Action
  ----- -----------------------
  `f`   Find file
  `s`   Search (ripgrep)
  `R`   Ripgrep (consult)
  `G`   Deadgrep
  `M`   Magit status
  `T`   Run project tests
  `B`   Build project
  `D`   Deploy
  `C`   Open docker-compose
  `N`   Project notes
  `S`   Shell (vterm) in project root
  `A`   API scratchpad (api.http)
  `r`   Recompile last command

### Eglot (LSP) Commands

When editing code with LSP support:

  Key          Action
  ------------ -----------------------
  `C-c e a`    Code actions
  `C-c e r`    Rename symbol
  `C-c e o`    Organize imports
  `C-c e i`    Find implementation
  `C-c e t`    Find type definition

### RestClient Mode

When editing `.http` files:

  Key          Action
  ------------ -----------------------
  `C-c C-c`    Send request at point
  `C-c C-t`    Extract token from response (legacy)
  `C-c C-b`    Set bearer token from response
  `C-c C-e`    Set environment (dev/stage/prod)
  `C-c C-j`    Filter response with jq

------------------------------------------------------------------------

## Package Management

-   Uses **straight.el only** (package.el is dead to me)\
-   Lockfile at `straight/versions/default.el` → reproducible installs\
-   No `package-refresh-contents`, no random version drift, no "works on
    my machine" nonsense

------------------------------------------------------------------------

## Python Development with pyenv

This config automatically detects and activates pyenv versions/virtualenvs:

1. **Per-project Python versions**: Reads `.python-version` files
2. **Virtualenv support**: Auto-activates pyenv virtualenvs
3. **LSP integration**: Configures Eglot with correct Python paths
4. **Tool integration**: Black formatter uses project's Python

To switch Python versions in a project:
- Use `C-c p y` to interactively select a pyenv version
- Creates/updates `.python-version` in project root
- Automatically restarts LSP with new environment

------------------------------------------------------------------------

## Customization

-   Machine-specific tweaks live in `~/.emacs.d/custom.el` (not
    versioned)\
-   Shared functions + bindings live in `personal/jps.el`

------------------------------------------------------------------------

## Troubleshooting

-   **First run slow?** Totally normal --- straight.el compiles
    packages. Wait it out.\
-   **Packages borked?** Nuke `~/.emacs.d/straight/` and restart.\
-   **Console mode weird?** Make sure `$TERM` is sane (`xterm-256color`
    usually works). On Debian/Ubuntu, install `emacs-nox` if you don't
    want GUI deps.\
-   **Mac PATH hell?** The setup script patches it via
    exec-path-from-shell.

------------------------------------------------------------------------

## Why This Exists

Because every time I switch machines or reinstall, I want to:\
- Type `git clone … && ./setup.sh`\
- Open Emacs\
- Be home.

That's it. No syncing caches, no "mystery dependencies," no crap
creeping into git. Just my environment, reproducible and portable.

------------------------------------------------------------------------

🔥 *Future Me: if you're reading this because you broke your config
again, don't worry --- just reclone and run setup. You'll be fine.*
