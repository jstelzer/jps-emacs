# Personal Emacs Configuration

A clean, portable Emacs configuration designed to work across macOS and Linux systems, supporting both GUI and console modes.

## Features

- **Platform-aware**: Automatically detects macOS/Linux and adjusts accordingly
- **Console/GUI support**: Works seamlessly in terminal and graphical environments  
- **Modern packages**: Uses straight.el, vertico, consult, LSP, and more
- **Project Dashboard**: Full project management using standard Emacs conventions (`C-x p`)
- **Smart Commands**: Auto-detects project type for build/test/deploy operations
- **Clean separation**: Only essential config files are versioned
- **Easy setup**: One-command installation on new systems
- **Online**: This whole setup is predicated on being able to fetch deps from the internet

## Quick Setup

1. Clone this repository:
   ```bash
   git clone <your-repo-url> ~/emacs-config
   ```

2. Run the setup script:
   ```bash
   cd ~/emacs-config
   ./setup.sh
   ```

3. Start Emacs:
   ```bash
   emacs      # GUI mode
   emacs -nw  # Console mode
   ```

## What Gets Installed

The setup script creates symlinks from `~/.emacs.d/` to your config files:
- `init.el` → Main configuration
- `personal/jps.el` → Custom functions and key bindings

## File Structure

```
~/emacs-config/
├── init.el              # Main Emacs configuration
├── personal/
│   └── jps.el          # Personal customizations and functions
├── .gitignore          # Excludes generated/cache files
├── setup.sh            # Installation script
└── README.md           # This file
```

## Platform Differences

### macOS
- Transparent windows (adjustable)
- Smooth scrolling for trackpad/mouse
- Native macOS input method
- Homebrew paths automatically added

### Linux
- Optimized for both X11 and Wayland
- Console mode fully supported
- Standard Linux key bindings

### Console Mode
- All functionality works in terminal
- Optimized for SSH sessions
- No GUI-specific features loaded

## Key Bindings

### Global Bindings
| Key         | Action                      |
|-------------|------------------------------|
| `C-c d`     | Kill line (vi-style dd)     |
| `C-c D`     | Docker management           |
| `C-c j`     | Format JSON region          |
| `C-c J`     | Flatten JSON to one line    |
| `C-c t`     | Insert timestamp            |
| `C-c w`     | Cleanup whitespace          |
| `C-c fd`    | Diff buffer with file       |
| `C-S-p`     | Search in project (ripgrep) |
| `C-x b`     | Switch buffers              |
| `C-c C-'`   | Claude Code Menu            |

### Project Dashboard (C-x p ...)
Uses standard Emacs project.el conventions with uppercase letters for custom commands:

| Key         | Action                      |
|-------------|------------------------------|
| `C-x p f`   | Find file in project        |
| `C-x p p`   | Switch projects             |
| `C-x p R`   | Ripgrep project search      |
| `C-x p S`   | Open shell (vterm)          |
| `C-x p M`   | Magit status                |
| `C-x p T`   | Smart test command          |
| `C-x p B`   | Smart build command         |
| `C-x p D`   | Smart deploy command        |
| `C-x p C`   | Open docker compose file    |
| `C-x p N`   | Open/create project notes   |
| `C-x p G`   | Deadgrep search             |
| `C-x p r`   | Recompile last command      |

## Package Management

Uses **`straight.el` exclusively** for reliable, reproducible package management:

- **First run**: All packages download and install automatically
- **Lockfile**: `straight/versions/default.el` ensures identical package versions across machines  
- **No conflicts**: Completely bypasses `package.el` to avoid version conflicts
- **Reproducible**: Same exact package versions on every machine

### Key Benefits over package.el
- Deterministic package versions (no "it works on my machine")
- Faster startup (no package activation overhead)
- Better debugging (source repos available locally)
- Zero maintenance (no `package-refresh-contents` needed)

## Project Management

The configuration follows standard Emacs project.el conventions while adding useful extensions:

### Smart Project Detection
- **Go**: Detects `go.mod` for Go projects
- **Rust**: Detects `Cargo.toml` for Rust projects  
- **Python**: Detects `pytest.ini`, `pyproject.toml`, or `tox.ini` for Python projects
- **Just**: Detects `justfile`, `Justfile`, or `.justfile` for task automation
- **Make**: Falls back to `Makefile` for compilation projects

### Project Dashboard Commands
All commands run from the project root directory and use intelligent defaults:

- **Test (`C-x p T`)**: `go test ./...`, `cargo test`, `pytest -q`, `just test`, or `make test`
- **Build (`C-x p B`)**: `go build ./...`, `cargo build`, `python -m build`, `just build`, or `make`
- **Deploy (`C-x p D`)**: Looks for `just deploy` recipe, `make deploy` target, or `scripts/deploy.sh`
- **Recompile (`C-x p r`)**: Re-runs the last project command

Priority order: Language-specific tools → Just (task automation) → Make (compilation) → Custom scripts

The project dashboard integrates with which-key to show helpful command descriptions.

## Customization

- Machine-specific settings go in `~/.emacs.d/custom.el`
- This file is automatically created and excluded from git
- Personal functions and keys are in `personal/jps.el`

## Troubleshooting

### First Run is Slow
The first time you start Emacs, it will download and compile packages. This is normal and only happens once.

### Missing Packages
If packages fail to install, delete `~/.emacs.d/straight/` and restart Emacs.

### Console Mode Issues
For SSH or console-only systems:
- Ensure `TERM` is set correctly
- Consider installing `emacs-nox` on Debian/Ubuntu

## Sharing Across Machines

1. Push your changes to git
2. On a new machine, run the setup script
3. All your customizations will be available immediately

This approach keeps your configuration clean and avoids syncing temporary files, caches, or downloaded packages across machines.

## Essential Tools

This configuration includes carefully selected tools to enhance your development workflow:

### Git & Version Control

- **[Magit](https://github.com/magit/magit)** (`C-x g`) - The definitive Git interface for Emacs
- **[Git-Gutter](https://github.com/emacsorphanage/git-gutter)** - Shows git diff indicators in the margin, real-time feedback on changes

### Search & Navigation

- **[Deadgrep](https://github.com/Wilfred/deadgrep)** (`C-x p G`) - Superior ripgrep interface with regex builder and file filtering
  - Better than basic grep for complex searches across large codebases
  - Interactive filtering of results by file type, directory
  - Context controls and match highlighting
  
- **[Consult-Ripgrep](https://github.com/minad/consult)** (`C-S-p`) - Fast incremental search with live preview

### Development Tools

- **[RestClient](https://github.com/pashky/restclient.el)** - Full-featured HTTP client (Postman replacement)
  
  **Core Features:**
  - Text-first `.http` files - version control friendly, diffable, shareable
  - Execute requests with `C-c C-c`, re-run last with `C-c C-r`
  - Variables for DRY requests (`:base_url`, `:token`, etc.)
  - Automatic cookie/session management between requests
  - Response viewing with syntax highlighting
  
  **Enhanced Workflow (Custom Functions):**
  - `C-x p A` - Open/create project API scratchpad with starter template
  - `C-c C-b` - Auto-extract Bearer token from login response
  - `C-c C-e` - Quick environment switcher (dev/stage/prod)
  - `C-c C-j` - Pipe JSON response through jq filters
  - Company autocompletion for HTTP methods, headers, and variables
  
  **Example Workflow:**
  ```http
  # Variables at top
  :env = dev
  :base_url = http://localhost:8080
  :token =
  
  ### Login (run with C-c C-c, then C-c C-b to extract token)
  POST :base_url/login
  Content-Type: application/json
  
  {"username": "demo", "password": "secret"}
  
  ### Use extracted token for protected endpoints
  GET :base_url/api/users
  Authorization: Bearer :token
  
  ### Filter response with jq (C-c C-j then enter '.items[] | {id, name}')
  GET :base_url/api/search?q=emacs
  Authorization: Bearer :token
  ```
  
  **Why It Replaces Postman:**
  - No external app needed - works in your editor
  - Plain text format - commit API tests with your code
  - Keyboard-centric - no mouse clicking through tabs
  - Project-specific `.http` files live with your code
  - Share API examples in PRs and documentation
  - Scriptable with elisp for complex auth flows

- **[Docker](https://github.com/Silex/docker.el)** (`C-c D`) - Complete Docker management
  - Container/image management
  - Dockerfile mode with syntax highlighting
  - Docker-compose integration

### Editing Enhancements

- **[Multiple-Cursors](https://github.com/magnars/multiple-cursors.el)** - Sublime Text style multiple cursors
  - `C-S-c C-S-c` - Edit lines with cursors
  - `C->` - Mark next like this
  - `C-<` - Mark previous like this
  - `C-c C->` - Mark all like this

- **[Markdown-Mode](https://github.com/jrblevin/markdown-mode)** - GitHub-flavored markdown support
  - Auto-enabled for README.md files
  - Preview support with `C-c C-c p`

### Project Extensions

- **Project Notes** (`C-x p N`) - Opens/creates `NOTES.org` in project root
  - Automatic template for new notes files
  - Per-project task tracking and ideas

### Language Support

- **LSP via Eglot** - Built-in language server protocol support
- **Rust** - rust-mode, cargo integration, rust-playground
- **Go** - go-mode with gofmt on save
- **Python** - pyvenv, blacken auto-formatting
- **Docker/YAML** - Syntax highlighting and formatting

## Tool Usage Examples

### Deadgrep Advanced Search
```
C-x p G                   # Launch deadgrep in project
M-x deadgrep-edit-mode    # Edit results directly
g                         # Refresh results
TAB                       # Expand/collapse file results
```

### Multiple Cursors Workflow
```
1. Select a word/symbol
2. C-> to mark next occurrence
3. C-> again for more
4. Edit all simultaneously
5. C-g to exit multi-cursor mode
```

### RestClient Example
Create a file `api-tests.http`:
```http
# Get user info
GET https://api.example.com/user/123
Authorization: Bearer token123

###

# Create new resource  
POST https://api.example.com/items
Content-Type: application/json

{
  "name": "New Item",
  "value": 42
}
```
Execute requests with `C-c C-c`.

### Docker Management
```
C-c D         # Open docker main menu
I             # List images
C             # List containers
F             # Follow container logs
```

## Customization

The `custom.el` on each host is intentionally not versioned or shared. Use that for tweaks to the current machine.
