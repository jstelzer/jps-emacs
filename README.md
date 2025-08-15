# Personal Emacs Configuration

A clean, portable Emacs configuration designed to work across macOS and Linux systems, supporting both GUI and console modes.

## Features

- **Platform-aware**: Automatically detects macOS/Linux and adjusts accordingly
- **Console/GUI support**: Works seamlessly in terminal and graphical environments  
- **Modern packages**: Uses straight.el, vertico, consult, LSP, and more
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

| Key                                        | Action                      |
| ----------+------------------------------- |                             |
| `C-c d`                                    | Kill line (vi-style dd)     |
| `C-c j`                                    | Format JSON region          |
| `C-c J`                                    | Flatten JSON to one line    |
| `C-c t`                                    | Insert timestamp            |
| `C-c w`                                    | Cleanup whitespace          |
| `C-c fd`                                   | Diff buffer with file       |
| `M-p`                                      | Project file finder         |
| `C-S-p`                                    | Search in project (ripgrep) |
| `C-x b`                                    | Switch buffers              |
| `C-c C-'`                                  | Claude Code Menu            |
|                                            |                             |

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

## Customization

The `custom.el` on each host is intentionally not versioned or shared. Use that for tweaks to the current machine.
