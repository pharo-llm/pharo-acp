# Pharo-ACP Implementation Guide

## Overview

This guide provides detailed information for implementing and using the Agent Client Protocol (ACP) in Pharo. It covers architecture decisions, class design, usage patterns, and best practices specific to the Pharo environment.

## Table of Contents

1. [Architecture](#architecture)
2. [Class Hierarchy](#class-hierarchy)
3. [Core Components](#core-components)
4. [Usage Examples](#usage-examples)
5. [Testing Strategy](#testing-strategy)
6. [Best Practices](#best-practices)
7. [Integration Patterns](#integration-patterns)

---

## Architecture

### Design Principles

The Pharo-ACP implementation follows these principles:

1. **Modularity**: Clear separation of concerns
2. **Immutability**: Message objects are immutable
3. **Type Safety**: Strong typing with validation
4. **Extensibility**: Easy to add new features
5. **Testability**: Comprehensive test coverage
6. **Pharo Integration**: Natural fit with Pharo idioms

### Package Structure

```
LLM-Pharo-ACP/
├── Core/
│   ├── ACPConnection
│   ├── ACPSession
│   ├── ACPMessage
│   ├── ACPRequest
│   ├── ACPResponse
│   ├── ACPNotification
│   └── ACPError
├── Client/
│   ├── ACPClient
│   ├── ACPClientCapabilities
│   └── ACPClientSession
├── Agent/
│   ├── ACPAgent
│   ├── ACPAgentCapabilities
│   └── ACPAgentSession
├── Messages/
│   ├── ACPInitializeRequest
│   ├── ACPInitializeResponse
│   ├── ACPNewSessionRequest
│   ├── ACPPromptRequest
│   └── ...
├── Content/
│   ├── ACPContentBlock
│   ├── ACPTextContent
│   ├── ACPImageContent
│   ├── ACPResourceContent
│   ├── ACPResourceLink
│   └── ACPDiffContent
├── ToolCalls/
│   ├── ACPToolCall
│   ├── ACPToolCallUpdate
│   └── ACPToolCallKind
├── Transport/
│   ├── ACPTransport
│   ├── ACPStdioTransport
│   ├── ACPHTTPTransport
│   └── ACPWebSocketTransport
├── Capabilities/
│   ├── ACPCapabilities
│   ├── ACPPromptCapabilities
│   └── ACPMCPCapabilities
└── Utilities/
    ├── ACPJSON
    ├── ACPValidator
    └── ACPLogger

LLM-Pharo-ACP-Tests/
├── Core/
│   ├── ACPConnectionTest
│   ├── ACPSessionTest
│   └── ACPMessageTest
├── Client/
│   └── ACPClientTest
├── Agent/
│   └── ACPAgentTest
├── Messages/
│   └── ACPMessageTests
├── Content/
│   └── ACPContentBlockTests
├── Integration/
│   └── ACPIntegrationTests
└── Examples/
    └── ACPExampleTests
```

---

## Class Hierarchy

### Core Message Hierarchy

```
Object
└── ACPMessage
    ├── ACPRequest
    │   ├── ACPInitializeRequest
    │   ├── ACPAuthenticateRequest
    │   ├── ACPNewSessionRequest
    │   ├── ACPLoadSessionRequest
    │   ├── ACPPromptRequest
    │   ├── ACPSetConfigOptionRequest
    │   ├── ACPSetModeRequest
    │   ├── ACPReadFileRequest
    │   ├── ACPWriteFileRequest
    │   ├── ACPCreateTerminalRequest
    │   ├── ACPTerminalOutputRequest
    │   ├── ACPWaitForExitRequest
    │   ├── ACPTerminalKillRequest
    │   ├── ACPTerminalReleaseRequest
    │   └── ACPPermissionRequest
    ├── ACPResponse
    │   ├── ACPInitializeResponse
    │   ├── ACPNewSessionResponse
    │   ├── ACPPromptResponse
    │   ├── ACPReadFileResponse
    │   ├── ACPCreateTerminalResponse
    │   ├── ACPTerminalOutputResponse
    │   ├── ACPWaitForExitResponse
    │   └── ACPPermissionResponse
    ├── ACPNotification
    │   ├── ACPSessionUpdate
    │   └── ACPCancelRequest
    └── ACPError
```

### Content Block Hierarchy

```
Object
└── ACPContentBlock
    ├── ACPTextContent
    ├── ACPImageContent
    ├── ACPAudioContent
    ├── ACPResourceContent
    ├── ACPResourceLink
    ├── ACPDiffContent
    └── ACPTerminalContent
```

### Transport Hierarchy

```
Object
└── ACPTransport
    ├── ACPStdioTransport
    ├── ACPHTTPTransport
    └── ACPWebSocketTransport
```

---

## Core Components

### ACPConnection

Manages the JSON-RPC connection between client and agent.

```smalltalk
Object subclass: #ACPConnection
    instanceVariableNames: 'transport messageId handlers logger'
    classVariableNames: ''
    package: 'LLM-Pharo-ACP-Core'
```

**Key Methods**:

```smalltalk
ACPConnection>>initialize
    "Initialize the connection"
    super initialize.
    messageId := 0.
    handlers := Dictionary new.
    logger := ACPLogger default

ACPConnection>>sendRequest: aRequest
    "Send a request and return a promise for the response"
    | id |
    id := self nextMessageId.
    aRequest id: id.
    ^ self sendMessage: aRequest

ACPConnection>>sendNotification: aNotification
    "Send a notification (no response expected)"
    ^ self sendMessage: aNotification

ACPConnection>>handleIncoming: aMessage
    "Handle incoming message from transport"
    aMessage isResponse
        ifTrue: [ self handleResponse: aMessage ]
        ifFalse: [ self handleRequest: aMessage ]

ACPConnection>>nextMessageId
    "Generate next message ID"
    messageId := messageId + 1.
    ^ messageId
```

### ACPSession

Represents a conversation session.

```smalltalk
Object subclass: #ACPSession
    instanceVariableNames: 'sessionId cwd mcpServers mode config history connection'
    classVariableNames: ''
    package: 'LLM-Pharo-ACP-Core'
```

**Key Methods**:

```smalltalk
ACPSession>>sendPrompt: aContentBlockCollection
    "Send a prompt to the agent"
    | request |
    request := ACPPromptRequest new
        sessionId: self sessionId;
        prompt: aContentBlockCollection;
        yourself.
    ^ connection sendRequest: request

ACPSession>>onUpdate: aBlock
    "Register callback for session updates"
    connection onNotification: 'session/update' do: [ :update |
        (update sessionId = self sessionId)
            ifTrue: [ aBlock value: update ] ]

ACPSession>>cancel
    "Cancel current operation"
    | notification |
    notification := ACPCancelRequest new
        sessionId: self sessionId;
        yourself.
    connection sendNotification: notification

ACPSession>>setMode: aModeId
    "Change session mode"
    | request |
    request := ACPSetModeRequest new
        sessionId: self sessionId;
        modeId: aModeId;
        yourself.
    ^ connection sendRequest: request
```

### ACPClient

Client-side ACP implementation.

```smalltalk
Object subclass: #ACPClient
    instanceVariableNames: 'connection capabilities info sessions'
    classVariableNames: ''
    package: 'LLM-Pharo-ACP-Client'
```

**Key Methods**:

```smalltalk
ACPClient>>initialize: anAgentTransport
    "Initialize connection with agent"
    | request response |
    connection := ACPConnection transport: anAgentTransport.

    request := ACPInitializeRequest new
        protocolVersion: 1;
        clientInfo: self clientInfo;
        clientCapabilities: self clientCapabilities;
        yourself.

    response := connection sendRequest: request.
    self negotiateCapabilities: response.
    ^ response

ACPClient>>newSession: aWorkingDirectory
    "Create new session"
    | request response session |
    request := ACPNewSessionRequest new
        cwd: aWorkingDirectory asAbsolute pathString;
        mcpServers: #();
        yourself.

    response := connection sendRequest: request.
    session := ACPClientSession new
        sessionId: response sessionId;
        connection: connection;
        cwd: aWorkingDirectory;
        yourself.

    sessions at: response sessionId put: session.
    ^ session

ACPClient>>readFile: aPath inSession: aSession
    "Read file contents"
    | request response |
    request := ACPReadFileRequest new
        sessionId: aSession sessionId;
        path: aPath asAbsolute pathString;
        yourself.

    response := connection sendRequest: request.
    ^ response content

ACPClient>>writeFile: aPath content: aString inSession: aSession
    "Write file contents"
    | request |
    request := ACPWriteFileRequest new
        sessionId: aSession sessionId;
        path: aPath asAbsolute pathString;
        content: aString;
        yourself.

    ^ connection sendRequest: request
```

### ACPAgent

Agent-side ACP implementation.

```smalltalk
Object subclass: #ACPAgent
    instanceVariableNames: 'connection capabilities info sessions'
    classVariableNames: ''
    package: 'LLM-Pharo-ACP-Agent'
```

**Key Methods**:

```smalltalk
ACPAgent>>start
    "Start the agent and listen for requests"
    connection := ACPConnection transport: ACPStdioTransport new.
    self registerHandlers.
    connection start

ACPAgent>>handleInitialize: aRequest
    "Handle initialize request"
    | response |
    response := ACPInitializeResponse new
        protocolVersion: 1;
        agentInfo: self agentInfo;
        agentCapabilities: self agentCapabilities;
        authenticationMethods: #();
        yourself.
    ^ response

ACPAgent>>handleNewSession: aRequest
    "Handle session creation"
    | session response |
    session := ACPAgentSession new
        sessionId: self generateSessionId;
        cwd: aRequest cwd asFileReference;
        connection: connection;
        yourself.

    sessions at: session sessionId put: session.

    response := ACPNewSessionResponse new
        sessionId: session sessionId;
        yourself.
    ^ response

ACPAgent>>handlePrompt: aRequest
    "Handle prompt request"
    | session |
    session := sessions at: aRequest sessionId.
    ^ session processPrompt: aRequest prompt

ACPAgent>>sendUpdate: anUpdate toSession: aSession
    "Send session update notification"
    | notification |
    notification := ACPSessionUpdate new
        sessionId: aSession sessionId;
        update: anUpdate;
        yourself.
    connection sendNotification: notification
```

---

## Usage Examples

### Client Example: Connecting to an Agent

```smalltalk
"Create client and connect to agent"
| client agent session response |

"Create agent transport (stdio)"
agent := ACPStdioTransport command: '/path/to/agent'.

"Initialize client"
client := ACPClient new.
client initialize: agent.

"Create session"
session := client newSession: '/home/user/project' asFileReference.

"Send prompt"
response := session sendPrompt: {
    ACPTextContent new text: 'Create a HelloWorld class'.
}.

"Handle updates"
session onUpdate: [ :update |
    update type = 'message' ifTrue: [
        Transcript show: update content; cr.
    ].
    update type = 'tool_call' ifTrue: [
        Transcript show: 'Tool: ', update title; cr.
    ].
].

"Wait for response"
response wait.
Transcript show: 'Done: ', response stopReason; cr.
```

### Agent Example: Implementing a Simple Agent

```smalltalk
"Create and start agent"
| agent |

agent := ACPAgent new
    agentInfo: (ACPAgentInfo new
        name: 'Pharo Assistant';
        version: '1.0.0';
        yourself);
    agentCapabilities: (ACPAgentCapabilities new
        loadSession: false;
        promptCapabilities: (ACPPromptCapabilities new
            image: false;
            audio: false;
            embeddedContext: true;
            yourself);
        yourself);
    yourself.

"Register prompt handler"
agent onPrompt: [ :session :prompt |
    | text |
    text := prompt first text.

    "Send thinking update"
    agent sendUpdate: (ACPMessageUpdate new
        role: 'agent';
        content: { ACPTextContent new text: 'Processing request...' };
        yourself) toSession: session.

    "Process request"
    text = 'hello' ifTrue: [
        agent sendUpdate: (ACPMessageUpdate new
            role: 'agent';
            content: { ACPTextContent new text: 'Hello! How can I help?' };
            yourself) toSession: session.
    ].

    "Return response"
    ACPPromptResponse new stopReason: 'end_turn'; yourself
].

"Start agent"
agent start.
```

### Creating Content Blocks

```smalltalk
"Text content"
textContent := ACPTextContent new
    text: 'This is a message';
    yourself.

"Image content"
imageContent := ACPImageContent new
    mimeType: 'image/png';
    data: imageData base64Encoded;
    uri: 'file:///path/to/image.png';
    yourself.

"Embedded resource"
resourceContent := ACPResourceContent new
    resource: (ACPResource new
        uri: 'file:///path/to/file.js';
        text: fileContents;
        mimeType: 'text/javascript';
        yourself);
    yourself.

"Resource link"
resourceLink := ACPResourceLink new
    uri: 'file:///path/to/document.pdf';
    name: 'Documentation';
    mimeType: 'application/pdf';
    size: 1024000;
    yourself.

"Diff content"
diffContent := ACPDiffContent new
    path: '/path/to/file.js';
    oldText: 'const x = 1;';
    newText: 'const x = 2;';
    mimeType: 'text/javascript';
    yourself.
```

### Tool Calls

```smalltalk
"Create tool call"
toolCall := ACPToolCall new
    toolCallId: 'tool_', UUID new asString;
    title: 'Read config.json';
    kind: #read;
    status: #pending;
    location: (ACPLocation new
        path: '/absolute/path/to/config.json';
        yourself);
    content: #();
    yourself.

"Send tool call update"
agent sendUpdate: toolCall toSession: session.

"Update tool status"
toolCallUpdate := ACPToolCallUpdate new
    toolCallId: toolCall toolCallId;
    status: #completed;
    content: { ACPTextContent new text: 'File read successfully' };
    yourself.

agent sendUpdate: toolCallUpdate toSession: session.
```

### Permission Requests

```smalltalk
"Request permission"
permissionRequest := ACPPermissionRequest new
    sessionId: session sessionId;
    title: 'Delete test files?';
    description: 'Remove 5 temporary test files';
    options: {
        ACPPermissionOption new
            optionId: 'allow';
            kind: #allow_once;
            label: 'Delete files';
            yourself.
        ACPPermissionOption new
            optionId: 'reject';
            kind: #reject_once;
            label: 'Keep files';
            yourself.
    };
    yourself.

"Send request and wait for response"
permissionResponse := connection sendRequest: permissionRequest.
permissionResponse selectedOptionId = 'allow' ifTrue: [
    "Proceed with deletion"
].
```

### File Operations

```smalltalk
"Read file"
content := client readFile: '/path/to/config.json' asFileReference
    inSession: session.

"Read partial file"
request := ACPReadFileRequest new
    sessionId: session sessionId;
    path: '/path/to/large-file.txt';
    startLine: 100;
    lineLimit: 50;
    yourself.
content := connection sendRequest: request.

"Write file"
client writeFile: '/path/to/output.txt' asFileReference
    content: 'Hello from Pharo-ACP!'
    inSession: session.
```

### Terminal Operations

```smalltalk
"Create terminal and run command"
createRequest := ACPCreateTerminalRequest new
    sessionId: session sessionId;
    command: 'echo';
    args: #('Hello Terminal');
    cwd: session cwd pathString;
    yourself.

createResponse := connection sendRequest: createRequest.
terminalId := createResponse terminalId.

"Wait for completion"
waitRequest := ACPWaitForExitRequest new
    sessionId: session sessionId;
    terminalId: terminalId;
    yourself.

waitResponse := connection sendRequest: waitRequest.
Transcript show: 'Output: ', waitResponse output; cr.
Transcript show: 'Exit code: ', waitResponse exitCode asString; cr.

"Release terminal"
releaseRequest := ACPTerminalReleaseRequest new
    sessionId: session sessionId;
    terminalId: terminalId;
    yourself.

connection sendRequest: releaseRequest.
```

---

## Testing Strategy

### Unit Tests

Test individual components in isolation:

```smalltalk
TestCase subclass: #ACPMessageTest
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'LLM-Pharo-ACP-Tests-Core'

ACPMessageTest>>testInitializeRequestSerialization
    | request json |
    request := ACPInitializeRequest new
        protocolVersion: 1;
        clientInfo: (ACPClientInfo new name: 'Test'; yourself);
        yourself.

    json := request asJSON.

    self assert: (json at: 'jsonrpc') equals: '2.0'.
    self assert: (json at: 'method') equals: 'initialize'.
    self assert: ((json at: 'params') at: 'protocolVersion') equals: 1.

ACPMessageTest>>testInitializeResponseDeserialization
    | json response |
    json := '{
        "jsonrpc": "2.0",
        "id": 1,
        "result": {
            "protocolVersion": 1,
            "agentInfo": {"name": "Test Agent"},
            "agentCapabilities": {},
            "authenticationMethods": []
        }
    }'.

    response := ACPMessage fromJSON: json.

    self assert: response class equals: ACPInitializeResponse.
    self assert: response protocolVersion equals: 1.
    self assert: response agentInfo name equals: 'Test Agent'.
```

### Integration Tests

Test complete workflows:

```smalltalk
TestCase subclass: #ACPIntegrationTest
    instanceVariableNames: 'client agent session'
    classVariableNames: ''
    package: 'LLM-Pharo-ACP-Tests-Integration'

ACPIntegrationTest>>setUp
    super setUp.
    agent := self createMockAgent.
    client := ACPClient new.
    client initialize: agent transport.
    session := client newSession: FileSystem workingDirectory.

ACPIntegrationTest>>testCompletePromptFlow
    | response updates |
    updates := OrderedCollection new.

    session onUpdate: [ :update | updates add: update ].

    response := session sendPrompt: {
        ACPTextContent new text: 'test prompt'
    }.

    self assert: updates notEmpty.
    self assert: response stopReason equals: 'end_turn'.

ACPIntegrationTest>>testFileOperations
    | content |
    "Write file"
    client writeFile: self testFile
        content: 'test content'
        inSession: session.

    "Read file back"
    content := client readFile: self testFile inSession: session.

    self assert: content equals: 'test content'.
```

### Example Tests

Executable examples that serve as documentation:

```smalltalk
TestCase subclass: #ACPExampleTest
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'LLM-Pharo-ACP-Tests-Examples'

ACPExampleTest>>exampleBasicClientUsage
    "Demonstrates basic client usage"
    | client session response |

    client := ACPClient new.
    client initialize: self mockAgent.

    session := client newSession: FileSystem workingDirectory.

    response := session sendPrompt: {
        ACPTextContent new text: 'Hello'
    }.

    self assert: response isKindOf: ACPPromptResponse.
```

### Test Coverage Goals

- **Unit Tests**: 90%+ coverage of all classes
- **Integration Tests**: All major workflows covered
- **Example Tests**: All usage patterns documented
- **Performance Tests**: Response time benchmarks
- **Error Tests**: All error conditions handled

---

## Best Practices

### Error Handling

```smalltalk
"Always handle errors gracefully"
[
    response := connection sendRequest: request.
] on: ACPError do: [ :error |
    error code = -32003 ifTrue: [
        "Handle resource not found"
        Transcript show: 'Resource not found: ', error message; cr.
    ] ifFalse: [
        "Handle other errors"
        error signal.
    ]
].
```

### Validation

```smalltalk
"Validate all incoming messages"
ACPMessage>>validate
    self protocolVersion = 1 ifFalse: [
        ACPValidationError signal: 'Invalid protocol version'
    ].
    self method ifNil: [
        ACPValidationError signal: 'Missing method'
    ].
```

### Logging

```smalltalk
"Use structured logging"
connection logger
    info: 'Sending request'
    context: {
        'method' -> request method.
        'id' -> request id.
        'sessionId' -> request sessionId
    } asDictionary.
```

### Resource Management

```smalltalk
"Always release resources"
[
    terminal := client createTerminal: command inSession: session.
    output := terminal waitForExit.
] ensure: [
    terminal release.
].
```

### Capability Checking

```smalltalk
"Always check capabilities before use"
client capabilities fs readTextFile ifTrue: [
    content := client readFile: path inSession: session.
] ifFalse: [
    self error: 'File reading not supported by client'.
].
```

---

## Integration Patterns

### Pharo IDE Integration

```smalltalk
"Integrate with Pharo's UI"
ACPBrowser>>initialize
    super initialize.
    self initializeAgent.
    self setupUI.

ACPBrowser>>setupUI
    "Create browser UI with agent chat"
    self buildChatPanel.
    self buildCodePanel.
    self buildToolCallPanel.

ACPBrowser>>sendPromptFromSelection
    | selection prompt |
    selection := self selectedText.
    prompt := {
        ACPTextContent new text: 'Explain this code:'.
        ACPResourceContent new
            resource: (ACPResource new
                uri: 'selection://current';
                text: selection;
                yourself);
            yourself
    }.
    session sendPrompt: prompt.
```

### File System Integration

```smalltalk
"Use Pharo's file system abstraction"
ACPClient>>readFile: aFileReference inSession: aSession
    | request |
    request := ACPReadFileRequest new
        sessionId: aSession sessionId;
        path: aFileReference fullName;
        yourself.
    ^ connection sendRequest: request

ACPClient>>writeFile: aFileReference content: aString inSession: aSession
    | request |
    request := ACPWriteFileRequest new
        sessionId: aSession sessionId;
        path: aFileReference fullName;
        content: aString;
        yourself.
    ^ connection sendRequest: request
```

### Process Integration

```smalltalk
"Integrate with Pharo's process model"
ACPStdioTransport>>start
    process := PipeableOSProcess command: command arguments: args.
    process errorPipelineContents ifNotNil: [ :error |
        self logger error: 'Agent error: ', error.
    ].
    self startReadLoop.

ACPStdioTransport>>startReadLoop
    [
        [ process isRunning ] whileTrue: [
            line := process outputPipelineContents.
            line ifNotNil: [ self handleLine: line ].
        ]
    ] forkNamed: 'ACP Transport Reader'.
```

---

## Performance Considerations

### Message Batching

```smalltalk
"Batch multiple updates for efficiency"
ACPSession>>sendBatchUpdate: aCollection
    aCollection do: [ :update |
        self sendUpdate: update
    ] separatedBy: [
        1 milliSeconds wait "Small delay for batching"
    ].
```

### Caching

```smalltalk
"Cache file contents to reduce reads"
ACPClient>>readFileCached: aPath inSession: aSession
    ^ fileCache at: aPath ifAbsentPut: [
        self readFile: aPath inSession: aSession
    ]
```

### Async Operations

```smalltalk
"Use promises for async operations"
ACPConnection>>sendRequest: aRequest
    | promise |
    promise := Promise new.
    handlers at: aRequest id put: promise.
    self sendMessage: aRequest.
    ^ promise
```

---

## Extending the Protocol

### Custom Methods

```smalltalk
"Define custom method with underscore prefix"
ACPClient>>registerMethod: methodName handler: aBlock
    connection registerHandler: methodName do: aBlock.

"Example: Custom code analysis method"
client registerMethod: '_custom/analyze_code' handler: [ :request |
    "Implement custom analysis"
    ACPResponse new result: analysisResult
].
```

### Custom Content Blocks

```smalltalk
"Define custom content block type"
ACPContentBlock subclass: #ACPCustomContent
    instanceVariableNames: 'customField'
    classVariableNames: ''
    package: 'LLM-Pharo-ACP-Extensions'

ACPCustomContent>>asJSON
    ^ super asJSON
        at: 'type' put: 'custom';
        at: 'customField' put: customField;
        yourself
```

### Metadata Extensions

```smalltalk
"Add custom metadata using _meta field"
ACPMessage>>addMetadata: aKey value: aValue
    self metadata ifNil: [ metadata := Dictionary new ].
    metadata at: ('_', aKey) put: aValue.

"Example usage"
request addMetadata: 'priority' value: 'high'.
request addMetadata: 'source' value: 'user_action'.
```

---

## Deployment

### Packaging

```smalltalk
"Define baseline for Metacello"
baseline: spec
    <baseline>
    spec for: #common do: [
        "Dependencies"
        spec
            baseline: 'NeoJSON'
            with: [ spec repository: 'github://svenvc/NeoJSON/repository' ].

        "Packages"
        spec
            package: 'LLM-Pharo-ACP' with: [ spec requires: #('NeoJSON') ];
            package: 'LLM-Pharo-ACP-Tests' with: [ spec requires: #('LLM-Pharo-ACP') ].

        "Groups"
        spec
            group: 'Core' with: #('LLM-Pharo-ACP');
            group: 'Tests' with: #('LLM-Pharo-ACP-Tests');
            group: 'default' with: #('Core' 'Tests').
    ]
```

### Distribution

```smalltalk
"Load in Pharo image"
Metacello new
    githubUser: 'pharo-llm'
    project: 'pharo-acp'
    commitish: 'main'
    path: 'src';
    baseline: 'LLMPharoACP';
    load.
```

---

## Troubleshooting

### Common Issues

**Issue**: Connection timeout

```smalltalk
"Solution: Increase timeout"
connection timeout: 30 seconds.
```

**Issue**: Invalid JSON parsing

```smalltalk
"Solution: Enable debug logging"
connection logger level: #debug.
```

**Issue**: Process not found

```smalltalk
"Solution: Check agent path and permissions"
agent := ACPStdioTransport command: '/full/path/to/agent'.
agent verifyExecutable ifFalse: [
    self error: 'Agent not found or not executable'
].
```

### Debugging

```smalltalk
"Enable protocol logging"
ACPLogger default
    level: #debug;
    output: Transcript.

"Inspect messages"
connection onMessage: [ :msg |
    msg inspect.
].

"Break on errors"
ACPError signal inspect.
```

---

## Resources

- **Protocol Specification**: See `ACP-PROTOCOL.md`
- **API Reference**: See `API-REFERENCE.md`
- **Examples**: See `examples/` directory
- **Tests**: See `LLM-Pharo-ACP-Tests` package
