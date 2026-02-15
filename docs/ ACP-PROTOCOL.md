# Agent Client Protocol (ACP) - Complete Specification

## Table of Contents

1. [Introduction](#introduction)
2. [Architecture](#architecture)
3. [Protocol Overview](#protocol-overview)
4. [Initialization](#initialization)
5. [Session Management](#session-management)
6. [Prompt Turn Flow](#prompt-turn-flow)
7. [Content Blocks](#content-blocks)
8. [Tool Calls](#tool-calls)
9. [File System Operations](#file-system-operations)
10. [Terminal Operations](#terminal-operations)
11. [Session Modes](#session-modes)
12. [Capabilities](#capabilities)
13. [Authentication](#authentication)
14. [Transports](#transports)
15. [Error Handling](#error-handling)
16. [Message Reference](#message-reference)
17. [Examples](#examples)

---

## Introduction

The **Agent Client Protocol (ACP)** is a standardized communication framework that enables interoperability between code editors/IDEs (clients) and AI coding agents. It solves the problem of fragmented agent-editor integrations by providing a universal protocol, similar to how the Language Server Protocol (LSP) standardized language tooling.

### Problems ACP Solves

1. **Reduced Integration Overhead**: Agents don't need custom implementations for each editor
2. **Increased Compatibility**: One agent works across multiple editors (VS Code, JetBrains, etc.)
3. **No Vendor Lock-in**: Users aren't restricted to specific agent-editor combinations
4. **Standardized Features**: Common capabilities (file operations, terminal access, tool execution)

### Key Features

- **Bidirectional Communication**: JSON-RPC 2.0 over stdio (local) or HTTP/WebSocket (remote)
- **Rich Content Support**: Text, images, audio, embedded resources, and diffs
- **Tool Execution Framework**: Standardized tool calls with permission requests
- **Session Management**: Multiple independent conversation contexts
- **Progress Streaming**: Real-time updates during agent operations
- **Extensibility**: Custom methods and metadata fields

---

## Architecture

### Core Components

#### Agents
Autonomous AI programs that:
- Process user requests and modify code
- Execute tools (read/write files, run commands)
- Maintain conversation context and history
- Run as subprocesses (local) or services (remote)

#### Clients
Code editors/IDEs that:
- Provide user interfaces for agent interaction
- Manage file system and terminal resources
- Control permissions for sensitive operations
- Stream updates to users in real-time

#### MCP Servers
Message Control Protocol servers that extend agent capabilities:
- Provide additional tools and resources
- Support stdio, HTTP, and SSE transports
- Enable third-party integrations

### Communication Model

```
┌─────────┐                ┌─────────┐                ┌─────────────┐
│ Client  │◄──JSON-RPC────►│  Agent  │◄───stdio──────►│ MCP Server  │
│  (IDE)  │                │   (AI)  │                │   (Tools)   │
└─────────┘                └─────────┘                └─────────────┘
```

---

## Protocol Overview

ACP uses **JSON-RPC 2.0** for all communication. Two message categories exist:

1. **Request-Response Methods**: Require acknowledgment from receiver
2. **Notifications**: One-way messages with no response expected

### Protocol Lifecycle

```
1. Initialization
   ↓
2. Authentication (optional)
   ↓
3. Session Creation/Loading
   ↓
4. Prompt Turns (repeated)
   ↓
5. Session Termination
```

### Message Format

All messages follow JSON-RPC 2.0 format:

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "session/prompt",
  "params": {
    "sessionId": "sess_abc123",
    "prompt": [{"type": "text", "text": "Hello"}]
  }
}
```

---

## Initialization

Initialization establishes protocol version and capability negotiation between client and agent.

### Initialization Sequence

1. **Client** sends `initialize` request
2. **Agent** processes capabilities and responds
3. Both parties agree on protocol version
4. Connection ready for session operations

### Initialize Request

**Method**: `initialize`

**Parameters**:

```json
{
  "protocolVersion": 1,
  "clientInfo": {
    "name": "VS Code",
    "version": "1.80.0"
  },
  "clientCapabilities": {
    "fs": {
      "readTextFile": true,
      "writeTextFile": true
    },
    "terminal": {
      "create": true,
      "output": true,
      "waitForExit": true,
      "kill": true,
      "release": true
    }
  }
}
```

### Initialize Response

**Returns**:

```json
{
  "protocolVersion": 1,
  "agentInfo": {
    "name": "Claude Code",
    "version": "1.0.0"
  },
  "agentCapabilities": {
    "loadSession": true,
    "promptCapabilities": {
      "image": true,
      "audio": true,
      "embeddedContext": true
    },
    "mcpCapabilities": {
      "supportedTransports": ["stdio", "http"]
    },
    "sessionModes": [
      {
        "modeId": "code",
        "title": "Code Mode",
        "description": "Write and modify code with full tool access"
      },
      {
        "modeId": "architect",
        "title": "Architect Mode",
        "description": "Design and plan software systems"
      },
      {
        "modeId": "ask",
        "title": "Ask Mode",
        "description": "Request permission before making changes"
      }
    ]
  },
  "authenticationMethods": []
}
```

### Version Negotiation Rules

- Client requests highest protocol version it supports
- Agent **MUST** respond with same version if supported
- If versions don't match, client **SHOULD** disconnect
- Current protocol version: **1**

---

## Session Management

Sessions represent independent conversation contexts with separate history, working directory, and MCP servers.

### Creating New Sessions

**Method**: `session/new`

**Parameters**:

```json
{
  "cwd": "/absolute/path/to/project",
  "mcpServers": [
    {
      "transport": "stdio",
      "command": "/usr/local/bin/mcp-server",
      "args": ["--verbose"],
      "env": {
        "API_KEY": "secret"
      }
    }
  ]
}
```

**Returns**:

```json
{
  "sessionId": "sess_abc123def456"
}
```

### Loading Existing Sessions

**Method**: `session/load` (requires `loadSession` capability)

**Parameters**:

```json
{
  "sessionId": "sess_abc123def456",
  "mcpServers": []
}
```

**Behavior**:
- Agent replays entire conversation through `session/update` notifications
- Response sent after history is fully transmitted
- MCP servers reconnected with provided configuration

### Working Directory Requirements

- **MUST** be an absolute path
- **MUST** be used for all file system operations in the session
- Acts as security boundary for file access
- Independent of agent subprocess spawn location

### Session Configuration

**Method**: `session/set_config_option`

**Parameters**:

```json
{
  "sessionId": "sess_abc123",
  "option": "autoApproveTools",
  "value": true
}
```

---

## Prompt Turn Flow

A prompt turn represents one complete interaction cycle: client sends message → agent processes → agent responds.

### Prompt Turn Lifecycle

```
1. Client sends session/prompt request
   ↓
2. Agent sends session/update notifications (progress)
   ↓
3. Agent may request permissions via session/request_permission
   ↓
4. Agent executes tools and sends tool updates
   ↓
5. Agent returns PromptResponse with stop reason
```

### Sending Prompts

**Method**: `session/prompt`

**Parameters**:

```json
{
  "sessionId": "sess_abc123",
  "prompt": [
    {
      "type": "text",
      "text": "Fix the bug in login.js"
    },
    {
      "type": "resource",
      "resource": {
        "uri": "file:///path/to/login.js",
        "text": "// file contents..."
      }
    }
  ]
}
```

### Receiving Updates

**Method**: `session/update` (notification from agent)

**Update Types**:

1. **Message Updates**: Agent response text
2. **Tool Call Updates**: Tool execution progress
3. **Plan Updates**: Task breakdown and status
4. **Mode Updates**: Session mode changes
5. **Slash Command Updates**: Available commands

**Example Message Update**:

```json
{
  "method": "session/update",
  "params": {
    "sessionId": "sess_abc123",
    "update": {
      "type": "message",
      "role": "agent",
      "content": [
        {
          "type": "text",
          "text": "I'll fix the authentication bug."
        }
      ]
    }
  }
}
```

### Prompt Response

**Returns**:

```json
{
  "stopReason": "end_turn"
}
```

**Stop Reasons**:

- `end_turn`: Normal completion
- `max_tokens`: Token limit reached
- `refusal`: Agent declined request
- `cancelled`: User cancelled operation

### Cancellation

**Method**: `session/cancel` (notification)

**Parameters**:

```json
{
  "sessionId": "sess_abc123"
}
```

Agents **MUST** abort current operations and return `cancelled` stop reason.

---

## Content Blocks

Content blocks represent different types of information in messages.

### Baseline Content Types

All agents **MUST** support:

1. **Text Content**
2. **Resource Links**

### Text Content

```json
{
  "type": "text",
  "text": "This is a message"
}
```

### Image Content

Requires `image` prompt capability.

```json
{
  "type": "image",
  "mimeType": "image/png",
  "data": "base64encodeddata...",
  "uri": "file:///path/to/image.png",
  "annotations": {
    "title": "Screenshot"
  }
}
```

### Audio Content

Requires `audio` prompt capability.

```json
{
  "type": "audio",
  "mimeType": "audio/wav",
  "data": "base64encodeddata..."
}
```

### Embedded Resource

Requires `embeddedContext` capability. Full resource contents included in message.

```json
{
  "type": "resource",
  "resource": {
    "uri": "file:///path/to/file.js",
    "mimeType": "text/javascript",
    "text": "const x = 1;"
  }
}
```

**Binary Resources**:

```json
{
  "type": "resource",
  "resource": {
    "uri": "file:///path/to/image.png",
    "mimeType": "image/png",
    "blob": "base64encodeddata..."
  }
}
```

### Resource Link

Reference to accessible resource (not embedded).

```json
{
  "type": "resource_link",
  "uri": "file:///path/to/document.pdf",
  "name": "Documentation",
  "mimeType": "application/pdf",
  "size": 1024000,
  "description": "API documentation"
}
```

### Annotations

Optional metadata for all content blocks:

```json
{
  "type": "text",
  "text": "Important message",
  "annotations": {
    "title": "Warning",
    "priority": "high",
    "_custom": "custom metadata"
  }
}
```

---

## Tool Calls

Tool calls represent operations the agent performs (file reads, edits, searches, commands).

### Tool Call Structure

```json
{
  "type": "tool_call",
  "toolCallId": "tool_123",
  "title": "Read config.json",
  "kind": "read",
  "status": "pending",
  "location": {
    "path": "/absolute/path/to/config.json",
    "line": 10
  },
  "content": []
}
```

### Tool Call Kinds

1. **read**: Data retrieval (reading files, fetching resources)
2. **edit**: Content modification (editing files, refactoring)
3. **delete**: Removal operations (deleting files, removing code)
4. **move**: File reorganization (renaming, moving files)
5. **search**: Information discovery (grep, find, code search)
6. **execute**: Command/code execution (bash, tests, build)
7. **think**: Internal reasoning (planning, analysis)
8. **fetch**: External data retrieval (web fetch, API calls)
9. **other**: Miscellaneous operations

### Tool Call Status Lifecycle

```
pending → in_progress → completed
                     ↘ failed
```

**Status Definitions**:

- `pending`: Awaiting approval or input
- `in_progress`: Actively executing
- `completed`: Successfully finished
- `failed`: Error encountered

### Tool Call Updates

**Method**: `session/update` with `tool_call_update` type

```json
{
  "method": "session/update",
  "params": {
    "sessionId": "sess_abc123",
    "update": {
      "type": "tool_call_update",
      "toolCallId": "tool_123",
      "status": "completed",
      "content": [
        {
          "type": "text",
          "text": "File read successfully"
        }
      ]
    }
  }
}
```

### Tool Call Content Types

#### Standard Content
Regular text, images, or resource links.

#### Diff Content
File modifications showing changes:

```json
{
  "type": "diff",
  "path": "/path/to/file.js",
  "oldText": "const x = 1;",
  "newText": "const x = 2;",
  "mimeType": "text/javascript"
}
```

#### Terminal Content
Live command output:

```json
{
  "type": "terminal",
  "terminalId": "term_123",
  "command": "npm test",
  "exitCode": 0
}
```

### Permission Requests

**Method**: `session/request_permission`

**Parameters**:

```json
{
  "sessionId": "sess_abc123",
  "title": "Delete old logs?",
  "description": "Remove logs older than 30 days",
  "options": [
    {
      "optionId": "allow_once",
      "kind": "allow_once",
      "label": "Allow once"
    },
    {
      "optionId": "allow_always",
      "kind": "allow_always",
      "label": "Always allow"
    },
    {
      "optionId": "reject",
      "kind": "reject_once",
      "label": "Cancel"
    }
  ]
}
```

**Returns**:

```json
{
  "selectedOptionId": "allow_once"
}
```

**Permission Kinds**:

- `allow_once`: Approve this operation only
- `allow_always`: Auto-approve this type of operation
- `reject_once`: Deny this operation only
- `reject_always`: Auto-deny this type of operation

---

## File System Operations

File system methods enable agents to read and write files in the client environment.

### Reading Files

**Method**: `fs/read_text_file` (requires `readTextFile` capability)

**Parameters**:

```json
{
  "sessionId": "sess_abc123",
  "path": "/absolute/path/to/file.txt",
  "startLine": 10,
  "lineLimit": 50
}
```

**Returns**:

```json
{
  "content": "File contents here..."
}
```

**Features**:
- Reads current editor state (includes unsaved changes)
- Optional line-based partial reads
- Absolute paths required

### Writing Files

**Method**: `fs/write_text_file` (requires `writeTextFile` capability)

**Parameters**:

```json
{
  "sessionId": "sess_abc123",
  "path": "/absolute/path/to/file.txt",
  "content": "New file contents"
}
```

**Returns**: `null` on success

**Behavior**:
- Creates file if it doesn't exist
- Overwrites existing file contents
- Client **MUST** create parent directories if needed

### Capability Verification

Before calling file system methods, agents **MUST** check `clientCapabilities`:

```json
{
  "fs": {
    "readTextFile": true,
    "writeTextFile": true
  }
}
```

If capability is `false` or not present, agent **MUST NOT** call the method.

### Path Requirements

- All paths **MUST** be absolute
- Paths should be within session working directory
- Line numbers use 1-based indexing

---

## Terminal Operations

Terminal operations enable agents to execute commands and interact with running processes.

### Creating Terminals

**Method**: `terminal/create` (requires `create` capability)

**Parameters**:

```json
{
  "sessionId": "sess_abc123",
  "command": "npm",
  "args": ["test"],
  "cwd": "/absolute/path/to/project",
  "env": {
    "NODE_ENV": "test"
  }
}
```

**Returns**:

```json
{
  "terminalId": "term_abc123"
}
```

### Reading Terminal Output

**Method**: `terminal/output` (requires `output` capability)

**Parameters**:

```json
{
  "sessionId": "sess_abc123",
  "terminalId": "term_abc123"
}
```

**Returns**:

```json
{
  "output": "Terminal output text...",
  "exitCode": null
}
```

**Behavior**:
- Non-blocking: Returns immediately with current output
- `exitCode` is `null` if process still running
- Returns cumulative output from process start

### Waiting for Completion

**Method**: `terminal/wait_for_exit` (requires `waitForExit` capability)

**Parameters**:

```json
{
  "sessionId": "sess_abc123",
  "terminalId": "term_abc123",
  "timeout": 30000
}
```

**Returns**:

```json
{
  "output": "Complete terminal output...",
  "exitCode": 0
}
```

**Behavior**:
- Blocks until process exits or timeout
- Returns full output and exit code
- Timeout in milliseconds (optional)

### Killing Processes

**Method**: `terminal/kill` (requires `kill` capability)

**Parameters**:

```json
{
  "sessionId": "sess_abc123",
  "terminalId": "term_abc123"
}
```

**Returns**: `null`

**Behavior**:
- Terminates running process
- Terminal remains accessible for output retrieval
- Does not release terminal ID

### Releasing Terminals

**Method**: `terminal/release` (requires `release` capability)

**Parameters**:

```json
{
  "sessionId": "sess_abc123",
  "terminalId": "term_abc123"
}
```

**Returns**: `null`

**Behavior**:
- Deallocates terminal resources
- Invalidates terminal ID
- Cannot retrieve output after release

---

## Session Modes

Session modes modify agent behavior and available tools.

### Available Modes

#### Code Mode
**Mode ID**: `code`

Full implementation mode with all tools available. Agent can:
- Read and write files
- Execute commands
- Make changes without restrictions

#### Architect Mode
**Mode ID**: `architect`

Planning mode for system design. Agent can:
- Analyze code structure
- Design architecture
- Create implementation plans
- Cannot modify code directly

#### Ask Mode
**Mode ID**: `ask`

Permission-based mode. Agent must:
- Request approval before changes
- Wait for user confirmation
- Explain each action

### Setting Modes

**Method**: `session/set_mode`

**Parameters**:

```json
{
  "sessionId": "sess_abc123",
  "modeId": "architect"
}
```

**Returns**: `null`

### Mode Updates

**Method**: `session/update` with `current_mode_update` type

```json
{
  "method": "session/update",
  "params": {
    "sessionId": "sess_abc123",
    "update": {
      "type": "current_mode_update",
      "modeId": "code"
    }
  }
}
```

### Exit Plan Mode

Special feature allowing agents to request mode transition:

1. Agent creates plan in architect mode
2. Agent offers "exit plan mode" tool
3. User chooses to approve and switch to code mode
4. Agent implements the plan with chosen permission level

---

## Capabilities

Capabilities enable feature negotiation between clients and agents.

### Client Capabilities

```json
{
  "fs": {
    "readTextFile": true,
    "writeTextFile": true
  },
  "terminal": {
    "create": true,
    "output": true,
    "waitForExit": true,
    "kill": true,
    "release": true
  }
}
```

### Agent Capabilities

```json
{
  "loadSession": true,
  "promptCapabilities": {
    "image": true,
    "audio": true,
    "embeddedContext": true
  },
  "mcpCapabilities": {
    "supportedTransports": ["stdio", "http", "sse"]
  },
  "sessionModes": [
    {
      "modeId": "code",
      "title": "Code Mode",
      "description": "Full implementation mode"
    }
  ]
}
```

### Prompt Capabilities

Defines supported content types in prompts:

- `image`: Supports image content blocks
- `audio`: Supports audio content blocks
- `embeddedContext`: Supports embedded resource blocks

### MCP Capabilities

Defines supported MCP server transports:

- `stdio`: Required baseline transport
- `http`: Optional HTTP transport
- `sse`: Deprecated SSE transport

### Capability Rules

1. Omitted capabilities are treated as unsupported
2. Clients and agents **SHOULD** support all capability combinations
3. Features must be checked before use
4. Protocol allows safe feature expansion

---

## Authentication

Optional authentication for secure agent connections.

### Authentication Methods

Advertised in `initialize` response:

```json
{
  "authenticationMethods": [
    {
      "methodId": "api_key",
      "title": "API Key",
      "description": "Authenticate with API key"
    }
  ]
}
```

### Authenticating

**Method**: `authenticate`

**Parameters**:

```json
{
  "methodId": "api_key",
  "credentials": {
    "apiKey": "secret_key_here"
  }
}
```

**Returns**: `null` on success

**Error Codes**:

- `-32001`: Authentication failed
- `-32002`: Invalid method ID

### Authentication Flow

```
1. Client receives authentication methods in initialize response
2. Client prompts user for credentials (if needed)
3. Client calls authenticate method
4. Agent validates credentials
5. On success, proceed to session creation
6. On failure, return error and disconnect
```

---

## Transports

Transport mechanisms for agent-client communication.

### Stdio Transport (Required)

Local agents communicate via JSON-RPC over stdin/stdout.

**Configuration**:

```json
{
  "transport": "stdio",
  "command": "/path/to/agent",
  "args": ["--verbose"],
  "env": {
    "LOG_LEVEL": "debug"
  }
}
```

**Behavior**:
- Agent runs as subprocess
- Newline-delimited JSON messages
- Stderr for agent logging
- Process exit terminates connection

### HTTP Transport (Optional)

Remote agents accessible via HTTP/HTTPS.

**Configuration**:

```json
{
  "transport": "http",
  "url": "https://agent.example.com/api",
  "headers": {
    "Authorization": "Bearer token123"
  }
}
```

**Behavior**:
- Standard HTTP POST requests
- JSON-RPC messages in request body
- Notifications sent async (fire-and-forget)

### WebSocket Transport (Optional)

Bidirectional communication for remote agents.

**Configuration**:

```json
{
  "transport": "websocket",
  "url": "wss://agent.example.com/ws",
  "headers": {
    "Authorization": "Bearer token123"
  }
}
```

**Behavior**:
- Persistent connection
- Full-duplex communication
- Efficient for streaming updates

### SSE Transport (Deprecated)

Server-Sent Events for remote agents. **Not recommended for new implementations.**

---

## Error Handling

ACP uses standard JSON-RPC 2.0 error handling.

### Error Structure

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "error": {
    "code": -32001,
    "message": "Authentication failed",
    "data": {
      "reason": "Invalid API key"
    }
  }
}
```

### Standard Error Codes

JSON-RPC 2.0 standard codes:

- `-32700`: Parse error
- `-32600`: Invalid request
- `-32601`: Method not found
- `-32602`: Invalid params
- `-32603`: Internal error

### ACP-Specific Error Codes

- `-32001`: Authentication failed
- `-32002`: Invalid authentication method
- `-32003`: Resource not found
- `-32004`: Permission denied
- `-32005`: Session not found
- `-32006`: Invalid session state
- `-32007`: Capability not supported

### Error Handling Best Practices

1. **Always include message**: Clear error description
2. **Use data field**: Additional context and details
3. **Handle gracefully**: Don't crash on errors
4. **Log errors**: Aid debugging and monitoring
5. **User-friendly messages**: Avoid technical jargon in UI

---

## Message Reference

Complete reference of all ACP methods and notifications.

### Agent Methods

| Method | Description | Parameters | Returns |
|--------|-------------|------------|---------|
| `initialize` | Establish connection | `InitializeRequest` | `InitializeResponse` |
| `authenticate` | Validate credentials | `AuthenticateRequest` | `null` |
| `session/new` | Create new session | `NewSessionRequest` | `NewSessionResponse` |
| `session/load` | Resume session | `LoadSessionRequest` | `null` |
| `session/prompt` | Send user message | `PromptRequest` | `PromptResponse` |
| `session/set_config_option` | Update configuration | `SetConfigRequest` | `null` |
| `session/set_mode` | Change session mode | `SetModeRequest` | `null` |

### Agent Notifications

| Notification | Description | Parameters |
|--------------|-------------|------------|
| `session/cancel` | Cancel operation | `CancelRequest` |

### Client Methods

| Method | Description | Parameters | Returns |
|--------|-------------|------------|---------|
| `fs/read_text_file` | Read file contents | `ReadFileRequest` | `ReadFileResponse` |
| `fs/write_text_file` | Write file contents | `WriteFileRequest` | `null` |
| `terminal/create` | Start command | `CreateTerminalRequest` | `CreateTerminalResponse` |
| `terminal/output` | Get output | `TerminalOutputRequest` | `TerminalOutputResponse` |
| `terminal/wait_for_exit` | Wait for completion | `WaitForExitRequest` | `WaitForExitResponse` |
| `terminal/kill` | Kill process | `TerminalKillRequest` | `null` |
| `terminal/release` | Release terminal | `TerminalReleaseRequest` | `null` |
| `session/request_permission` | Request approval | `PermissionRequest` | `PermissionResponse` |

### Client Notifications

| Notification | Description | Parameters |
|--------------|-------------|------------|
| `session/update` | Progress update | `SessionUpdate` |

---

## Examples

### Complete Initialization Flow

```json
// 1. Client sends initialize
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "initialize",
  "params": {
    "protocolVersion": 1,
    "clientInfo": {
      "name": "Pharo IDE",
      "version": "14.0"
    },
    "clientCapabilities": {
      "fs": {
        "readTextFile": true,
        "writeTextFile": true
      }
    }
  }
}

// 2. Agent responds
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "protocolVersion": 1,
    "agentInfo": {
      "name": "Pharo ACP Agent",
      "version": "1.0.0"
    },
    "agentCapabilities": {
      "loadSession": true,
      "promptCapabilities": {
        "image": false,
        "audio": false,
        "embeddedContext": true
      }
    },
    "authenticationMethods": []
  }
}
```

### Create Session and Send Prompt

```json
// 1. Create session
{
  "jsonrpc": "2.0",
  "id": 2,
  "method": "session/new",
  "params": {
    "cwd": "/home/user/my-project",
    "mcpServers": []
  }
}

// 2. Session created
{
  "jsonrpc": "2.0",
  "id": 2,
  "result": {
    "sessionId": "sess_abc123"
  }
}

// 3. Send prompt
{
  "jsonrpc": "2.0",
  "id": 3,
  "method": "session/prompt",
  "params": {
    "sessionId": "sess_abc123",
    "prompt": [
      {
        "type": "text",
        "text": "Create a HelloWorld class"
      }
    ]
  }
}

// 4. Agent sends updates (notification)
{
  "jsonrpc": "2.0",
  "method": "session/update",
  "params": {
    "sessionId": "sess_abc123",
    "update": {
      "type": "message",
      "role": "agent",
      "content": [
        {
          "type": "text",
          "text": "I'll create a HelloWorld class for you."
        }
      ]
    }
  }
}

// 5. Tool call update
{
  "jsonrpc": "2.0",
  "method": "session/update",
  "params": {
    "sessionId": "sess_abc123",
    "update": {
      "type": "tool_call",
      "toolCallId": "tool_1",
      "title": "Write HelloWorld.class.st",
      "kind": "edit",
      "status": "pending",
      "content": []
    }
  }
}

// 6. Tool completed
{
  "jsonrpc": "2.0",
  "method": "session/update",
  "params": {
    "sessionId": "sess_abc123",
    "update": {
      "type": "tool_call_update",
      "toolCallId": "tool_1",
      "status": "completed",
      "content": [
        {
          "type": "diff",
          "path": "/home/user/my-project/HelloWorld.class.st",
          "oldText": "",
          "newText": "Object subclass: #HelloWorld\n\tinstanceVariableNames: ''\n\tclassVariableNames: ''\n\tpackage: 'MyPackage'"
        }
      ]
    }
  }
}

// 7. Prompt response
{
  "jsonrpc": "2.0",
  "id": 3,
  "result": {
    "stopReason": "end_turn"
  }
}
```

### File System Operations

```json
// Read file
{
  "jsonrpc": "2.0",
  "id": 10,
  "method": "fs/read_text_file",
  "params": {
    "sessionId": "sess_abc123",
    "path": "/home/user/my-project/config.json"
  }
}

// Response
{
  "jsonrpc": "2.0",
  "id": 10,
  "result": {
    "content": "{\"version\": \"1.0.0\"}"
  }
}

// Write file
{
  "jsonrpc": "2.0",
  "id": 11,
  "method": "fs/write_text_file",
  "params": {
    "sessionId": "sess_abc123",
    "path": "/home/user/my-project/output.txt",
    "content": "Hello from ACP!"
  }
}

// Response
{
  "jsonrpc": "2.0",
  "id": 11,
  "result": null
}
```

### Terminal Operations

```json
// Create terminal and run command
{
  "jsonrpc": "2.0",
  "id": 20,
  "method": "terminal/create",
  "params": {
    "sessionId": "sess_abc123",
    "command": "echo",
    "args": ["Hello Terminal"],
    "cwd": "/home/user/my-project"
  }
}

// Response
{
  "jsonrpc": "2.0",
  "id": 20,
  "result": {
    "terminalId": "term_xyz789"
  }
}

// Wait for completion
{
  "jsonrpc": "2.0",
  "id": 21,
  "method": "terminal/wait_for_exit",
  "params": {
    "sessionId": "sess_abc123",
    "terminalId": "term_xyz789"
  }
}

// Response
{
  "jsonrpc": "2.0",
  "id": 21,
  "result": {
    "output": "Hello Terminal\n",
    "exitCode": 0
  }
}

// Release terminal
{
  "jsonrpc": "2.0",
  "id": 22,
  "method": "terminal/release",
  "params": {
    "sessionId": "sess_abc123",
    "terminalId": "term_xyz789"
  }
}
```

### Permission Request

```json
// Agent requests permission
{
  "jsonrpc": "2.0",
  "id": 30,
  "method": "session/request_permission",
  "params": {
    "sessionId": "sess_abc123",
    "title": "Delete test files?",
    "description": "Remove 5 temporary test files",
    "options": [
      {
        "optionId": "allow",
        "kind": "allow_once",
        "label": "Delete files"
      },
      {
        "optionId": "reject",
        "kind": "reject_once",
        "label": "Keep files"
      }
    ]
  }
}

// Client responds
{
  "jsonrpc": "2.0",
  "id": 30,
  "result": {
    "selectedOptionId": "allow"
  }
}
```

---

## Implementation Notes for Pharo

### Package Structure

The Pharo-ACP implementation should include:

1. **Core Protocol Classes**
   - `ACPConnection`: Manages JSON-RPC communication
   - `ACPClient`: Client-side implementation
   - `ACPAgent`: Agent-side implementation
   - `ACPSession`: Session management

2. **Message Classes**
   - `ACPMessage`: Base message class
   - `ACPRequest`: Request messages
   - `ACPResponse`: Response messages
   - `ACPNotification`: One-way notifications
   - `ACPError`: Error handling

3. **Content Block Classes**
   - `ACPContentBlock`: Abstract base
   - `ACPTextContent`: Text content
   - `ACPImageContent`: Image content
   - `ACPResourceContent`: Embedded resources
   - `ACPResourceLink`: Resource references

4. **Tool Call Classes**
   - `ACPToolCall`: Tool call representation
   - `ACPToolCallUpdate`: Status updates
   - `ACPDiffContent`: File diffs
   - `ACPTerminalContent`: Terminal output

5. **Transport Classes**
   - `ACPTransport`: Abstract transport
   - `ACPStdioTransport`: Standard IO
   - `ACPHTTPTransport`: HTTP transport
   - `ACPWebSocketTransport`: WebSocket transport

6. **Capability Classes**
   - `ACPCapabilities`: Capability negotiation
   - `ACPClientCapabilities`: Client features
   - `ACPAgentCapabilities`: Agent features

### Design Considerations

1. **Immutability**: Message objects should be immutable
2. **Type Safety**: Use strong typing for message fields
3. **Validation**: Validate all incoming messages
4. **Error Handling**: Graceful error recovery
5. **Logging**: Comprehensive logging for debugging
6. **Testing**: Unit tests for all message types
7. **Documentation**: Clear API documentation

### Pharo-Specific Features

- Integration with Pharo's process model
- Support for Pharo's file system abstraction
- Integration with Pharo's UI framework
- Support for Pharo's debugging tools

---

## Resources

- **Official Website**: https://agentclientprotocol.com/
- **Specification**: https://agentclientprotocol.com/llms.txt
- **GitHub**: https://github.com/pharo-llm/pharo-acp
- **JSON-RPC 2.0**: https://www.jsonrpc.org/specification
