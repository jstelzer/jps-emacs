#!/bin/bash

# Emacs Configuration Setup Script
# This script sets up your Emacs configuration by symlinking files
# from your git repo to ~/.emacs.d

set -e

EMACS_DIR="$HOME/.emacs.d"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

echo "Setting up Emacs configuration..."

# Create ~/.emacs.d if it doesn't exist
if [ ! -d "$EMACS_DIR" ]; then
    echo "Creating $EMACS_DIR directory..."
    mkdir -p "$EMACS_DIR"
fi

# Function to safely create symlinks
create_symlink() {
    local source="$1"
    local target="$2"

    if [ -L "$target" ]; then
        echo "Removing existing symlink: $target"
        rm "$target"
    elif [ -d "$target" ]; then
        echo "Backing up existing directory: $target -> $target.backup"
        mv "$target" "$target.backup"
    elif [ -f "$target" ]; then
        echo "Backing up existing file: $target -> $target.backup"
        mv "$target" "$target.backup"
    fi

    echo "Creating symlink: $target -> $source"
    ln -s "$source" "$target"
}

# Symlink main configuration files
create_symlink "$SCRIPT_DIR/init.el" "$EMACS_DIR/init.el"

# Symlink entire personal directory (contains all jps-*.el modules)
create_symlink "$SCRIPT_DIR/personal" "$EMACS_DIR/personal"

# Symlink site-lisp directory (contains vendored dependencies)
create_symlink "$SCRIPT_DIR/site-lisp" "$EMACS_DIR/site-lisp"

# Create necessary directories
echo "Creating necessary directories..."
mkdir -p "$EMACS_DIR/saves"
mkdir -p "$EMACS_DIR/auto-save"
mkdir -p "$EMACS_DIR/templates"

# Symlink sounds directory (for ridiculous-coding and other audio effects)
if [ -d "$SCRIPT_DIR/assets/sounds" ]; then
    create_symlink "$SCRIPT_DIR/assets/sounds" "$EMACS_DIR/sounds"
fi

# Symlink images directory (for ridiculous-coding sprite animations)
if [ -d "$SCRIPT_DIR/assets/images" ]; then
    create_symlink "$SCRIPT_DIR/assets/images" "$EMACS_DIR/images"
fi

# Platform-specific setup
if [[ "$OSTYPE" == "darwin"* ]]; then
    echo "macOS detected - GUI and console modes supported"
elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
    echo "Linux detected - GUI and console modes supported"
    # You might want to install additional packages here
    # sudo apt-get install emacs-nox  # for console-only systems
else
    echo "Platform: $OSTYPE"
fi

# Org-roam notes setup (Linux only, assumes umbrella repo structure)
if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    FORGE_NOTES="$SCRIPT_DIR/../forge/notes"
    HOME_NOTES="$HOME/notes"

    # Only set up if we're in the umbrella repo structure
    if [ -d "$SCRIPT_DIR/../forge" ]; then
        echo ""
        echo "Setting up org-roam notes integration..."

        # Create forge/notes directory structure if it doesn't exist
        if [ ! -d "$FORGE_NOTES" ]; then
            echo "Creating notes directory in forge/..."
            mkdir -p "$FORGE_NOTES"/{concepts,journal,meta}
        fi

        # Symlink ~/notes to forge/notes
        if [ -L "$HOME_NOTES" ] && [ "$(readlink "$HOME_NOTES")" == "$FORGE_NOTES" ]; then
            echo "Notes symlink already exists and is correct"
        elif [ -L "$HOME_NOTES" ]; then
            echo "Removing old symlink: $HOME_NOTES"
            rm "$HOME_NOTES"
            echo "Creating symlink: $HOME_NOTES -> $FORGE_NOTES"
            ln -s "$FORGE_NOTES" "$HOME_NOTES"
        elif [ -d "$HOME_NOTES" ]; then
            echo "WARNING: $HOME_NOTES already exists as a directory"
            echo "Please manually move or backup this directory, then re-run setup"
        else
            echo "Creating symlink: $HOME_NOTES -> $FORGE_NOTES"
            ln -s "$FORGE_NOTES" "$HOME_NOTES"
        fi

        # Copy org-roam template if it doesn't exist
        TEMPLATE_SOURCE="$EMACS_DIR/templates/concept-node.org"
        if [ -f "$SCRIPT_DIR/.emacs.d/templates/concept-node.org" ]; then
            cp "$SCRIPT_DIR/.emacs.d/templates/concept-node.org" "$TEMPLATE_SOURCE"
            echo "Copied org-roam concept template"
        fi
    fi
fi

echo ""
echo "Setup complete! Your Emacs configuration is now ready."
echo ""
echo "Key features:"
echo "- Robust package management via straight.el (no package.el conflicts)"
echo "- Platform-aware configuration (macOS/Linux)"
echo "- Console and GUI support"
echo "- Modern completion with vertico/consult"
echo "- LSP support for development"
echo "- Reproducible builds with lockfile support"
echo "- Ridiculous coding mode (M-x ridiculous-coding-mode)"

if [[ "$OSTYPE" == "linux-gnu"* ]] && [ -d "$SCRIPT_DIR/../forge" ]; then
    echo "- Org-roam notes integration (Linux only, ~/notes -> forge/notes)"
fi

echo ""
echo "First run will take longer as packages download and compile."
echo "Subsequent starts will be fast."
echo ""
echo "To start Emacs:"
echo "  GUI: emacs"
echo "  Console: emacs -nw"
echo ""

if [[ "$OSTYPE" == "linux-gnu"* ]] && [ -d "$SCRIPT_DIR/../forge" ]; then
    echo "Org-roam key bindings (C-c n prefix):"
    echo "  C-c n f - Find/create note"
    echo "  C-c n i - Insert link to note"
    echo "  C-c n c - Capture new concept"
    echo "  C-c n j - Daily journal entry"
    echo "  C-c n b - Show backlinks"
    echo ""
fi

echo "After first run, commit straight/versions/default.el to lock package versions."

