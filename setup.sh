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

# Create personal directory if it doesn't exist
if [ ! -d "$EMACS_DIR/personal" ]; then
    echo "Creating $EMACS_DIR/personal directory..."
    mkdir -p "$EMACS_DIR/personal"
fi

# Function to safely create symlinks
create_symlink() {
    local source="$1"
    local target="$2"
    
    if [ -L "$target" ]; then
        echo "Removing existing symlink: $target"
        rm "$target"
    elif [ -f "$target" ]; then
        echo "Backing up existing file: $target -> $target.backup"
        mv "$target" "$target.backup"
    fi
    
    echo "Creating symlink: $target -> $source"
    ln -s "$source" "$target"
}

# Symlink main configuration files
create_symlink "$SCRIPT_DIR/init.el" "$EMACS_DIR/init.el"
create_symlink "$SCRIPT_DIR/personal/jps.el" "$EMACS_DIR/personal/jps.el"

# Create necessary directories
echo "Creating necessary directories..."
mkdir -p "$EMACS_DIR/saves"
mkdir -p "$EMACS_DIR/auto-save"

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
echo ""
echo "First run will take longer as packages download and compile."
echo "Subsequent starts will be fast."
echo ""
echo "To start Emacs:"
echo "  GUI: emacs"
echo "  Console: emacs -nw"
echo ""
echo "After first run, commit straight/versions/default.el to lock package versions."