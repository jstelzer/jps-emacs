# Agent-Shell Integration Guide

## Overview

This Emacs configuration now uses **agent-shell** instead of vterm as the primary terminal interface. Agent-shell provides LLM-powered terminal sessions with support for multiple AI backends.

## What Changed

### Replaced Components

- **Old:** vterm (traditional terminal emulator)
- **New:** agent-shell (LLM-powered terminal with multiple AI provider support)

### Modified Files

1. **`personal/jps-agent-shell.el`** (NEW) - Agent-shell configuration
2. **`personal/jps-project.el`** - Updated `jps-project-vterm` → `jps-project-agent-shell`
3. **`personal/jps-ai.el`** - Updated split-window helper to use agent-shell
4. **`personal/jps-tools.el`** - Vterm now deferred (optional fallback)
5. **`personal/jps.el`** - Added `(require 'jps-agent-shell)` to load order

## Supported Backends

Agent-shell supports multiple LLM providers:

- **Anthropic Claude Code** - Default backend (via `@zed-industries/claude-code-acp`)
- **Google Gemini CLI** - Requires `--experimental-acp` flag
- **OpenAI Codex** - Via `@zed-industries/codex-acp`
- **Goose CLI** - Standalone agent
- **Cursor Agent** - Via `@blowmage/cursor-agent-acp-npm`
- **Qwen Code** - Via `@qwen-code/qwen-code@latest`

## Installation

Agent-shell and its dependencies will be automatically installed via straight.el when you restart Emacs or reload your configuration.

### Backend Requirements

#### Claude Code (Default)
```bash
# Install via npm
npm install -g @zed-industries/claude-code-acp

# Authentication: Login-based (default)
# Or use API key (set in jps-agent-shell.el)
```

#### Gemini CLI
```bash
# Requires recent Gemini CLI with ACP support
gemini --experimental-acp
```

#### OpenAI Codex
```bash
npm install -g @zed-industries/codex-acp
```

## Configuration

### Authentication

**Claude Code (Login-based - Default):**
```elisp
;; No configuration needed - uses login flow
```

**Claude Code (API Key):**
```elisp
;; In personal/jps-agent-shell.el, uncomment and set:
(setq agent-shell-anthropic-authentication
  (agent-shell-anthropic-make-authentication
    :api-key (getenv "ANTHROPIC_API_KEY")))
```

**Environment Variables:**
```bash
# Add to your shell profile (~/.bashrc, ~/.zshrc, etc.)
export ANTHROPIC_API_KEY="your-key-here"
export OPENAI_API_KEY="your-key-here"
```

### Switching Backends

Edit `personal/jps-agent-shell.el` and change the preferred agent configuration:

```elisp
;; Claude Code (default)
(setq agent-shell-preferred-agent-config
      (agent-shell-anthropic-make-claude-code-config))

;; Or use other backends:
;; (setq agent-shell-preferred-agent-config
;;       (agent-shell-google-make-gemini-config))
;; (setq agent-shell-preferred-agent-config
;;       (agent-shell-openai-make-codex-config))
;; (setq agent-shell-preferred-agent-config
;;       (agent-shell-goose-make-config))
```

## Keybindings

### Agent-Shell Commands

| Key         | Command                                   | Description                          |
|-------------|-------------------------------------------|--------------------------------------|
| `C-c a s`   | `jps-agent-shell`                        | Start/switch to agent-shell          |
| `C-c a n`   | `jps-agent-shell-new`                    | Start new agent-shell session        |
| `C-c a r`   | `jps-agent-shell-send-region`            | Send selected region to agent        |
| `C-c a b`   | `jps-agent-shell-send-buffer`            | Send entire buffer to agent          |
| `C-c a f`   | `agent-shell-insert-file`                | Insert file into conversation        |
| `C-c a c`   | `agent-shell-insert-shell-command-output`| Insert command output                |
| `C-c a C`   | `agent-shell-anthropic-start-claude-code`| Launch Claude Code directly          |
| `C-c a G`   | `agent-shell-google-start-gemini`        | Launch Gemini directly               |

### Project Commands (Updated)

| Key       | Command                     | Description                    |
|-----------|----------------------------|--------------------------------|
| `C-x p S` | `jps-project-agent-shell`  | Open agent-shell in project    |
| `C-c g v` | `jps-chatgpt-agent-and-chat`| Split: agent-shell + ChatGPT  |

### Within Agent-Shell Buffer

| Key     | Command             | Description                |
|---------|---------------------|----------------------------|
| `C-c C-c` | Interrupt operation | Stop current LLM request  |

## Usage Examples

### Basic Shell Session
```
M-x jps-agent-shell  (or C-c a s)
```
Opens agent-shell in project root with LLM assistance.

### Send Code for Review
1. Select region in code buffer
2. Press `C-c a r`
3. Agent receives code and can analyze/suggest improvements

### Insert File into Conversation
```
C-c a f
```
Select a file to provide context to the agent.

### Run Command and Share Output
```
C-c a c
```
Run a shell command and insert output into conversation.

### Split Workflow (Agent + ChatGPT)
```
C-c g v
```
Opens agent-shell (left) and ChatGPT (right) for dual AI workflow.

## Advanced Features

### File Capabilities
Agent-shell can access files in your project:
```elisp
(setq agent-shell-text-file-capabilities t)  ;; Enabled by default
```

### Container Support (Experimental)
For devcontainer/Docker execution:
```elisp
(setq agent-shell-container-command-runner "docker exec -it mycontainer")
```

### Custom Display Behavior
Control where agent-shell buffers appear:
```elisp
;; Same window (default)
(setq agent-shell-display-action '(display-buffer-same-window))

;; Bottom window
(setq agent-shell-display-action '(display-buffer-at-bottom))

;; Side window
(setq agent-shell-display-action '(display-buffer-in-side-window))
```

## Migration Notes

### From vterm

**Old workflow:**
```elisp
C-x p S  → jps-project-vterm  → Traditional terminal
```

**New workflow:**
```elisp
C-x p S  → jps-project-agent-shell  → LLM-powered terminal
```

### Fallback to vterm

If you need traditional terminal without LLM:
```elisp
M-x vterm  ;; Still available, just not bound by default
```

Or add a keybinding in `personal/jps-agent-shell.el`:
```elisp
(global-set-key (kbd "C-c v") #'vterm)
```

## Troubleshooting

### "Agent not found"
Ensure backend is installed:
```bash
npm install -g @zed-industries/claude-code-acp
```

### Authentication Issues

**Claude Code:**
- Login-based: Follow prompts in Emacs
- API key: Set `ANTHROPIC_API_KEY` environment variable

**OpenAI:**
- Set `OPENAI_API_KEY` environment variable

### Agent-shell buffer not opening

1. Check `*Messages*` buffer for errors
2. Verify package installation: `M-x straight-rebuild-package RET agent-shell`
3. Check backend availability: Run backend command manually in terminal

### Performance Issues

Agent-shell buffers can accumulate large conversation histories:
- Start new sessions with `C-c a n`
- Clear buffer with `M-x erase-buffer` (within agent-shell buffer)

## Integration with Stoic Operator

The agent-shell integration complements the Stoic Operator workflow:

- **Stoic Operator:** Structured, grammar-governed batch processing
- **Agent-Shell:** Interactive LLM terminal for exploratory work

Use cases:
- **Stoic Operator:** Code analysis, refactoring, architecture design
- **Agent-Shell:** Quick questions, debugging, exploration, command assistance

Both can coexist and serve different purposes in your workflow.

## Future Enhancements

Potential additions to consider:

1. **Custom agent configurations** - Define project-specific agent behaviors
2. **Session persistence** - Save/restore agent conversations
3. **Multi-agent workflows** - Coordinate multiple agents on complex tasks
4. **Integration with Forge** - Route interactive queries to Stoic Operator backend

## References

- [agent-shell GitHub](https://github.com/xenodium/agent-shell)
- [ACP Protocol Spec](https://github.com/zed-industries/acp)
- Claude Code: `@zed-industries/claude-code-acp`
- Emacs integration: `personal/jps-agent-shell.el`

## Support

For issues:
1. Check agent-shell issues: https://github.com/xenodium/agent-shell/issues
2. Verify backend installation and authentication
3. Review Emacs `*Messages*` buffer for errors
4. Test backend independently outside Emacs

---

**Note:** Agent-shell is in early development. Expect rough edges and active feature development.
