# My Emacs Configuration

This is my personal Emacs setup. It's portable (macOS + Linux), works in
GUI and terminal, and gives me a **modern IDE inside Emacs without the
kitchen sink**.

**Now with modular architecture** -- each feature lives in its own file.
Don't use Rust? Comment out one line. Want to add Julia support? Drop in
a new module. Clean separation, easy maintenance.

Clone it, run one script, and you've got:\
- A project dashboard on `C-x p` with test/build/deploy/vterm/notes
wired in\
- Dead-simple JSON formatters and timestamp helpers\
- A clean, straight.el-based package setup that Just Works™\
- An environment I can rebuild on a new machine in 5 minutes

------------------------------------------------------------------------

## Features (the real ones)

-   🚀 **Fast startup** -- straight.el + lean config = sub-second loads\
-   🧭 **Project navigation** -- jump into files, grep, magit, tests,
    builds, deploys, compose, and notes with `C-x p`\
-   🛠 **Language support** -- eglot + formatters for Go, Rust, Python,
    plus snippets, company, and flycheck\
-   🐍 **Python environment management** -- automatic .venv activation per project via uv\
-   🌿 **Per-project environments** -- direnv integration via envrc, buffer-local `.envrc` support\
-   🧹 **Editing helpers** -- vi-style kill line, JSON pretty/flatten,
    whitespace cleanup, timestamp insert\
-   🔍 **Search everything** -- consult + ripgrep + vertico + orderless
    makes the minibuffer the center of the universe\
-   🎨 **Platform smarts** -- transparency + trackpad scroll on macOS,
    works in X11/Wayland, runs fine over SSH\
-   🤖 **Claude Code Menu** -- drop into AI-assisted coding from inside
    Emacs\
-   🦙 **Local LLMs** -- ellama + Ollama with per-machine model profiles
    (`C-c m`); see [Local LLMs](#local-llms-ellama--ollama)\
-   🧘 **Stoic Operator** -- interchangeable OpenAI/Ollama integration for writing, philosophy, and non-code AI work\
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

## External Binary Dependencies

The config auto-detects what's installed, but here's the full list of external tools it can use. Install what you need for your languages:

### AI Agents (agent-shell)

For the configured Claude, Codex, and Gemini integrations, make these four
executables available to Emacs. Installation methods and binary builds can differ
between macOS and Linux; the executable names and PATH requirements do not.
Installing the Emacs packages does **not** install these external programs.

| Executable | Purpose | Where to get it |
| ---------- | ------- | --------------- |
| `claude` | Claude Code CLI, including subscription login and the separate Claude Code IDE integration | [Claude Code](https://code.claude.com/docs/en/setup) |
| `claude-agent-acp` | Claude ACP adapter used by agent-shell | [Claude Agent ACP](https://github.com/agentclientprotocol/claude-agent-acp), or `npm install -g @agentclientprotocol/claude-agent-acp` |
| `codex-acp` | Codex ACP adapter with the built-in `acp_fs` filesystem bridge | Build/install [cola-io/codex-acp](https://github.com/cola-io/codex-acp) for your OS and CPU |
| `gemini` | Gemini CLI, with native ACP support via `--experimental-acp` | [Gemini CLI](https://github.com/google-gemini/gemini-cli), or `npm install -g @google/gemini-cli` |

**The Codex adapter matters:** this configuration expects the **cola-io** adapter,
not just any executable named `codex-acp`. Do not substitute the similarly named
`@agentclientprotocol/codex-acp` package and assume it supplies the same bridge.
The four filesystem tools are `read_text_file`, `write_text_file`,
`edit_text_file`, and `multi_edit_text_file`. These are MCP tools supplied by the
adapter, **not four additional executables to install on PATH**.

Put the executables (or symlinks to them) in a directory on PATH, such as
`~/bin`, and include that directory in your shell startup configuration:

```bash
export PATH="$HOME/bin:$PATH"

for tool in claude claude-agent-acp codex-acp gemini; do
    command -v "$tool" || printf "Missing executable: %s\n" "$tool"
done
```

**Current Codex path exception:** `personal/jps-agent-shell.el` deliberately pins
the bridge-capable adapter to `~/bin/codex-acp`, avoiding another adapter earlier
on PATH. Install or symlink the cola-io binary there on either OS. The other
commands are resolved through Emacs `exec-path`.

A working terminal PATH does not guarantee a GUI Emacs process sees the same
environment. This config uses `exec-path-from-shell`; verify the result inside
Emacs with `M-:`:

```elisp
(mapcar (lambda (tool) (cons tool (executable-find tool)))
        (quote ("claude" "claude-agent-acp" "codex-acp" "gemini")))
```

Each result should contain an absolute path rather than `nil`. On older
agent-shell installations, Claude may still be configured to launch the former
`claude-code-acp` name; update/rebuild agent-shell rather than assuming the new
executable name is already in use.

Complete the relevant login flow before starting an agent. Then use `C-c a C`
for Claude (the preferred agent), `C-c a O` for Codex, or `C-c a G` for Gemini.
After changing executables or configuration, reload `jps-agent-shell.el` and
start a **fresh session**. The bootstrap enables ACP file capabilities and sets
`CODEX_LOG_STDERR=0` to keep this Codex adapter’s console logs off the JSON-RPC
stream; existing sessions do not acquire newly configured tools retroactively.

### Go Development

```bash
# LSP server (required for code intelligence)
go install golang.org/x/tools/gopls@latest

# Formatting (required, called on save)
go install golang.org/x/tools/cmd/goimports@latest

# Debugging (optional, for breakpoint debugging)
go install github.com/go-delve/delve/cmd/dlv@latest

# Linting (optional, C-c C-s in go-mode)
go install honnef.co/go/tools/cmd/staticcheck@latest

# Struct memory layout visualization (optional, C-c C-l in go-mode)
go install honnef.co/go/tools/cmd/structlayout@latest
go install honnef.co/go/tools/cmd/structlayout-pretty@latest
```

### Python Development

```bash
# Install uv (fast Python package/project manager)
# macOS/Linux:
curl -LsSf https://astral.sh/uv/install.sh | sh
# Arch Linux:
pacman -S uv

# Install a Python version
uv python install 3.12

# Per-project setup (run in your project directory):
uv venv                    # Create .venv
uv python pin 3.12         # Create .python-version (optional)
uv sync                    # Install deps from pyproject.toml (if exists)

# Install dev tools (or use C-c C-t i in Emacs):
uv pip install debugpy ruff basedpyright pip-audit mypy pytest
```

### Rust Development

```bash
# Install via rustup (includes cargo, rustc, rust-analyzer)
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# Ensure rust-analyzer is available
rustup component add rust-analyzer

# Debugger (optional, for breakpoint debugging)
# Arch Linux:
yay -S codelldb-bin
# macOS/others: download from https://github.com/vadimcn/codelldb/releases
# or install via VSCode extension and symlink the binary
```

### General Tools

```bash
# Fast search (used by consult-ripgrep, deadgrep)
brew install ripgrep       # macOS
pacman -S ripgrep          # Arch Linux

# Fast file finding (optional, used by consult-find)
brew install fd            # macOS
pacman -S fd               # Arch Linux

# Terminal emulator (vterm)
# macOS: ships with libvterm via emacs-plus
# Arch: pacman -S libvterm

# Per-project environment variables (direnv)
brew install direnv        # macOS
pacman -S direnv           # Arch Linux
```

**Note**: The config works fine without these tools --- you just won't get language-specific features until you install the relevant LSP servers and formatters.

------------------------------------------------------------------------

## File Structure

    ~/emacs-config/
    ├── init.el                      # Bootstrap & entry point
    ├── personal/
    │   ├── jps.el                   # Main loader (orchestrates all modules)
    │   ├── jps-core.el              # Foundation utilities & settings
    │   ├── jps-completion.el        # Company, flycheck, yasnippet
    │   ├── jps-lsp.el               # Eglot LSP configuration
    │   ├── jps-ui.el                # Vertico, consult, embark, treemacs
    │   ├── jps-tools.el             # Vterm, magit, docker, git-gutter
    │   ├── jps-lang-go.el           # Go support (gopls, delve, staticcheck)
    │   ├── jps-lang-rust.el         # Rust support (rust-analyzer, cargo)
    │   ├── jps-lang-python.el       # Python support (uv, basedpyright, ruff)
    │   ├── jps-debug.el             # DAP debugging (Go/Rust/Python)
    │   ├── jps-rest.el              # REST client & API tools
    │   ├── jps-project.el           # Smart project commands
    │   ├── jps-ai.el                # ChatGPT integration
    │   ├── jps-ellama.el            # Local LLMs (ellama + Ollama profiles)
    │   ├── stoic-operator.el        # Stoic Operator (OpenAI/Ollama backend)
    │   └── jps-operator.el          # Stoic Operator config
    ├── site-lisp/
    │   ├── simple-httpd.el          # Vendored HTTP server (for org-roam-ui)
    │   └── stranger-things-theme.el # Vendored theme (no doom-themes deps)
    ├── logos/
    │   └── emacs-logo-stranger-things.png  # Dashboard banner
    ├── setup.sh                     # Symlink/bootstrap script
    ├── .gitignore                   # Ignores caches & generated stuff
    └── README.md                    # This file

**Modular Design**: Each module can be disabled by commenting out its `(require ...)` line in `personal/jps.el`. For example, if you don't use Rust, comment out `(require 'jps-lang-rust)`.

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
  `C-c S`    Load stranger-things theme (default)
  `C-c T`    Load manoj-dark theme
  `C-c L`    Load adwaita (light) theme
  `C-c F`    Toggle fullscreen
  `C-c X`    Toggle transparency
  `C-c W`    Toggle whitespace-mode
  `C-c A`    Align by regexp
  `C-c s`    Sort lines
  `C-c l`    Add change log entry
  `C-c x`    Configure platform settings
  `C-c p s`  Python environment status
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

### Local LLMs (ellama + Ollama)

`personal/jps-ellama.el` sets up [ellama](https://github.com/s-kostyaev/ellama)
against a local [Ollama](https://ollama.com). It is the local counterpart to
agent-shell: Claude, Codex and Gemini go through ACP; local models go through
ellama.

| Key | Action |
| --- | ------ |
| `C-c m` | ellama transient menu (chat, code, summarize, ...) |
| `C-c C-c` (in chat) | send the last message in an org chat buffer |
| `M-x jps-ellama-pull-models` | `ollama pull` the active profile's models |
| `M-x jps-ellama-apply-profile` | re-point ellama after changing the profile |

**Model profiles.** Which models are sensible depends entirely on the
hardware, so the module picks a profile when ellama first loads:

| Profile   | Machine                         | Coding model (MoE, fast)   | Chat model (dense)     | Context |
| --------- | ------------------------------- | -------------------------- | ---------------------- | ------- |
| `mac-48`  | M4 Max, 48GB unified            | `qwen3.6:35b-a3b-coding` (24GB) | `qwen3.8:27b` (18GB) | 32k |
| `mac-64`  | M5, 64GB unified                | `qwen3.6:35b-a3b-q8_0` (39GB)   | `qwen3.8:27b-q8_0` (30GB) | 64k |
| `cuda-32` | Linux, RTX 5090 32GB VRAM, 64GB RAM | `qwen3.6:35b-a3b-coding` (24GB) | `qwen3.8:27b` (18GB) | 32k |
| `small`   | anything else                   | `gemma4:12b`               | `gemma4:12b`           | 16k |

Detection: macOS reads `hw.memsize` (>=60GB is `mac-64`); Linux asks
`nvidia-smi` for VRAM (>=24GB is `cuda-32`). Override per machine in
`~/.emacs.d/custom.el` with `(setq jps-ellama-profile 'mac-64)`, or edit
`jps-ellama-profiles` to change models. On a fresh machine run
`M-x jps-ellama-pull-models` once.

**Sizing guidelines** (why the table looks like that):

- **Memory needed ~= parameters x bits-per-weight / 8, plus KV cache for the
  context.** A 32B model at Q4 is about 18-20GB; at Q8 about 34GB. Long
  contexts add several GB on top, which is why `num_ctx` is set per profile
  (Ollama's default context is too small for agent work).
- **Apple Silicon: the GPU can wire ~75% of unified memory by default** --
  about 36GB on a 48GB Mac and 48GB on a 64GB Mac. Raise it (until reboot) with
  `sudo sysctl iogpu.wired_limit_mb=<MB>`, leaving 8GB+ for macOS and Emacs.
- **NVIDIA: the model must fit in VRAM.** Anything that spills from the 5090's
  32GB into system RAM runs many times slower, so the 64GB of system RAM on
  the Linux box does not make 70B-class or Q8 35B models practical there.
  The 5090 is the fastest machine for models that fit (~1.8TB/s bandwidth);
  the 64GB Mac is the one that can run the larger quantizations.
- **MoE models (`35b-a3b` = 35B total, ~3B active per token) are the speed
  win**: they cost the memory of a 35B model but generate at roughly
  small-model speed. They get the coding, naming and summarizing jobs. Dense
  models are slower per token but steadier for long-form chat.
- **Qwen3.8 is the chat/agent default.** It ships only as a dense 27B
  (tuned for long-horizon agentic work, thinking on by default), so it
  replaces `qwen3.6:27b`. There is no 3.8 MoE yet; when one lands it is the
  obvious swap for the coding slot.
- **Generation speed is bounded by memory bandwidth; prompt processing is
  bounded by compute.** Agent loops resend large contexts every turn, so
  prompt processing matters more than it looks. The M5's GPU matrix units
  mainly speed up that phase.
- **70B dense at Q4 (~40GB)** only becomes realistic on the 64GB Mac, with a
  raised wired limit and a modest context. The profiles prefer a better
  quantization of a 30B-class model over a squeezed 70B.
- **Two models are rarely resident at once.** Ollama swaps between the chat
  and coding models on demand; the first request after a switch pays a load
  delay.
- **On Macs, Ollama also publishes MLX builds** (`*-mlx`, `*-mxfp8` tags),
  which are often faster on Apple Silicon. Try one by editing the profile;
  the defaults stay on the standard GGUF tags.

**Agent mode.** With `jps-ellama-agentic` (default `t`) the module calls
`ellama-setup-agentic-coding`: tools are enabled with DLP enforcement, and
without an SRT sandbox policy every tool call still asks for confirmation.
Small models handle one-shot commands well but are unreliable at multi-step
tool use; expect the 30B-class profiles to be the floor for that.

The older `gptel` Ollama backend in `jps-ai.el` is still configured
(`gemma4:31b`); the two can coexist.

### Stoic Operator (OpenAI/Ollama Integration)

A personal cognitive assistant for writing, philosophy, zettelkasten work, and non-code AI tasks. Supports interchangeable backends (OpenAI or Ollama) for flexibility and privacy.

**Why not Claude Code for this?** This is for content where subject matter might trip safety filters - philosophy, mental health journaling, controversial topics, etc. Local Ollama models give you full control.

  Key        Action
  ---------  ----------------------------------------------------------
  `C-c o m`  Open transient menu with all commands
  `C-c o d`  Refine region as Field Manual doctrine
  `C-c o o`  Outline chapter from region notes
  `C-c o t`  Translate region into different communication register

**Available registers**: `operator-technical`, `partner-friendly`, `therapist-brief`, `public-readable`

**Backend Configuration**:
- **Ollama** (default): Local models, full privacy, no API costs
  - Models: `mistral:7b-instruct-v0.2-q6_K`, `deepseek-r1:14b`, `llama3.1:8b`
  - Configure in `personal/jps-operator.el`
- **OpenAI**: API-based, requires `openai` CLI tool
  - Switch by setting `stoic-operator-backend` to `'openai`

**System Contract**: Place your custom system prompt in `~/.stoic-operator-system.txt` to define the AI's voice/personality across all commands.

### Project Dashboard (`C-x p`)

**Per-project environments**: Projects with `.envrc` files automatically get buffer-local environment variables via `envrc-global-mode`. Each tab/project maintains its own environment. Use `M-x envrc-allow` to authorize a new `.envrc`, `M-x envrc-reload` to refresh.

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

### Go-Specific Tools

Additional commands available in `go-mode`:

  Key            Action
  -------------- ---------------------------------
  `C-c C-t`      Open Go tools menu (transient)
  `C-c C-s p`    Run staticcheck on project
  `C-c C-s .`    Run staticcheck on package
  `C-c C-l l`    Show struct memory layout (raw)
  `C-c C-l p`    Show struct layout (pretty ASCII)
  `C-c C-l o`    Optimize struct field ordering

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

### Vendored Dependencies

Some stable, unmaintained libraries are vendored directly into the repo for self-containment:

-   **simple-httpd.el** (`site-lisp/simple-httpd.el`)
    -   Source: https://github.com/skeeto/emacs-web-server
    -   Author: Christopher Wellons <wellons@nullprogram.com>
    -   License: Public Domain (Unlicense)
    -   Purpose: HTTP server for org-roam-ui graph visualization
    -   Last upstream commit: Years ago (stable, no recent changes)
    -   Rationale: Small (33KB), stable, public domain, easier to vendor than manage via package system

-   **stranger-things-theme.el** (`site-lisp/stranger-things-theme.el`)
    -   Source: Adapted from doom-stranger-things-theme
    -   License: MIT (doom-themes)
    -   Purpose: Dark neon 80s theme inspired by Stranger Things (default theme)
    -   Rationale: Vendored without doom-themes dependency, trimmed to only include faces for packages used in this config

------------------------------------------------------------------------

## Python Development with uv

This config automatically detects and activates `.venv` directories:

1. **Per-project venvs**: Auto-detects `.venv` in project root (or parent dirs)
2. **LSP integration**: Configures basedpyright with correct venv paths
3. **Tool integration**: All tools run via `uv run` for automatic venv activation
4. **One-command setup**: `C-c C-t i` installs all dev tools into the venv

**Workflow:**
```bash
cd your-project
uv venv                    # Create .venv
uv python pin 3.12         # Optional: pin Python version
```

Then open a `.py` file in Emacs. The venv activates automatically.

**Transient menu** (`C-c C-t` in Python buffers):
- `v` - Create .venv
- `S` - Sync deps (uv sync)
- `p` - Pin Python version
- `i` - Install dev tools (debugpy, ruff, basedpyright, etc.)
- `s` - Show environment status
- `c` - Lint (ruff)
- `m` - Typecheck (mypy)
- `a` - Audit vulnerabilities (pip-audit)
- `t`/`T` - Run tests (project/file)

------------------------------------------------------------------------

## Debugging (Go, Rust, Python)

Full breakpoint debugging via **dap-mode** for Go, Rust, and Python. Set breakpoints, step through code, inspect variables --- just like Windsurf/VSCode, but inside Emacs.

### Prerequisites

Install the debugger for your language:

```bash
# Go - delve
go install github.com/go-delve/delve/cmd/dlv@latest

# Rust - CodeLLDB
# Arch: yay -S codelldb-bin
# Others: https://github.com/vadimcn/codelldb/releases

# Python - debugpy (install in each venv, or use C-c C-t i)
uv pip install debugpy
```

### Key Bindings

**Identical across all three languages** --- all debug commands start with `C-c d`:

  Key          Action
  ------------ -----------------------
  `C-c d b`    Toggle breakpoint
  `C-c d d`    Start debugging
  `C-c d n`    Next (step over)
  `C-c d i`    Step in
  `C-c d o`    Step out
  `C-c d c`    Continue
  `C-c d l`    View locals
  `C-c d s`    View sessions
  `C-c d r`    Restart frame
  `C-c d q`    Quit/disconnect
  `C-c d h`    Debug hydra menu

### Workflow

1. **Open a file** in your project (`.go`, `.rs`, or `.py`)
2. **Set breakpoints**: Navigate to a line, hit `C-c d b`
3. **Start debugging**: `C-c d d`, select debug template
4. **Step through**: Use `C-c d n/i/o` to navigate
5. **Inspect**: `C-c d l` shows local variables, hover over vars with mouse

### Debug Templates

Pre-configured templates available for each language:

**Go:**
- "PAM Test Server" (custom template, see `personal/jps-debug.el:48`)

**Rust:**
- "Rust::Run" - debug cargo binary
- "Rust::Test" - debug cargo test

**Python:**
- "Python :: Run file" - debug current Python file
- "Python :: Run module" - debug a Python module
- "Python :: Pytest current file" - debug pytest on current file

Python debugging automatically uses your active `.venv` environment!

### Adding Custom Templates

Add your own debug configurations:

```elisp
;; Go
(with-eval-after-load 'dap-dlv-go
  (dap-register-debug-template "My Service"
    (list :type "go"
          :request "launch"
          :name "My Service (Debug)"
          :mode "debug"
          :program "/path/to/your/cmd/main.go"
          :args '("--flag" "value"))))

;; Rust
(with-eval-after-load 'dap-codelldb
  (dap-register-debug-template "My Rust Binary"
    (list :type "lldb"
          :request "launch"
          :name "My Rust Binary"
          :program "${workspaceFolder}/target/debug/mybinary"
          :args [])))

;; Python
(with-eval-after-load 'dap-python
  (dap-register-debug-template "My Python Script"
    (list :type "python"
          :request "launch"
          :name "My Python Script"
          :program "/path/to/script.py"
          :args [])))
```

------------------------------------------------------------------------

## Customization

-   **Machine-specific tweaks** live in `~/.emacs.d/custom.el` (not versioned)\
-   **Modular configuration** lives in `personal/jps-*.el` files:
    -   Add/remove features by editing `personal/jps.el` (just comment out modules you don't need)
    -   Each module is self-contained and documented
    -   No dependencies between language modules (jps-lang-*)
    -   Core modules (jps-core, jps-lsp, jps-ui) are required by other modules
-   **Add your own modules**: Create `personal/jps-mynewfeature.el`, add `(provide 'jps-mynewfeature)` at the end, then `(require 'jps-mynewfeature)` in `personal/jps.el`

------------------------------------------------------------------------

## Troubleshooting

-   **First run slow?** Totally normal --- straight.el compiles
    packages. Wait it out.\
-   **Icons not showing?** First run auto-installs `all-the-icons` fonts
    (creates `~/.emacs.d/.all-the-icons-installed` guard file). If icons
    still look broken, restart Emacs or run `M-x all-the-icons-install-fonts`
    manually.\
-   **Packages borked?** Nuke `~/.emacs.d/straight/` and restart.\
-   **Console mode weird?** Make sure `$TERM` is sane (`xterm-256color`
    usually works). On Arch, install `emacs-nox` if you don't want GUI deps.\
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

## Agent Shell
Install claude-code-acp and put it in your PATH.

```
npm install @zed-industries/claude-code-acp
```                                              
                                                 
🔥 *Future Me: if you're reading this because you broke your config
again, don't worry --- just reclone and run setup. You'll be fine.*
