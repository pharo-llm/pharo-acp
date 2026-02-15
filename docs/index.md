# Pharo-ACP Quick Start Guide

## Installation

### Prerequisites

- Pharo 13 or Pharo 14
- Git (for loading from GitHub)

### Loading Pharo-ACP

#### From Metacello (Recommended)

```smalltalk
"Load stable version"
Metacello new
  githubUser: 'pharo-llm'
  project: 'pharo-acp'
  commitish: 'v1.0.0'
  path: 'src';
  baseline: 'LLMPharoACP';
  load.

"Load development version"
Metacello new
  githubUser: 'pharo-llm'
  project: 'pharo-acp'
  commitish: 'main'
  path: 'src';
  baseline: 'LLMPharoACP';
  load.
```

#### Manual Installation

1. Clone the repository:
```bash
git clone https://github.com/pharo-llm/pharo-acp.git
```

2. In Pharo:
```smalltalk
"Load from local clone"
Metacello new
  repository: 'tonel:///path/to/pharo-acp/src';
  baseline: 'LLMPharoACP';
  load.
```

---

## 5-Minute Tutorial

### Example 1: Creating a Simple Client

```smalltalk
"1. Create client and connect to agent"
client := ACPClient new.

"2. Initialize with agent transport"
agent := ACPStdioTransport command: '/path/to/agent-executable'.
client initialize: agent.

"3. Create a session"
session := client newSession: '/home/user/my-project' asFileReference.

"4. Send a prompt"
response := session sendPrompt: {
    ACPTextContent new text: 'Hello, agent!'
}.

"5. Check response"
Transcript show: 'Response: ', response stopReason; cr.
```

### Example 2: Reading and Writing Files

```smalltalk
"Create session"
session := client newSession: FileSystem workingDirectory.

"Read a file"
content := client
    readFile: 'config.json' asFileReference
    inSession: session.
Transcript show: 'File content: ', content; cr.

"Write a file"
client
    writeFile: 'output.txt' asFileReference
    content: 'Hello from Pharo-ACP!'
    inSession: session.
```

### Example 3: Handling Session Updates

```smalltalk
"Create session with update handler"
session := client newSession: FileSystem workingDirectory.

"Register update handler"
session onUpdate: [ :update |
    update type = 'message' ifTrue: [
        Transcript show: 'Agent: ', update content first text; cr.
    ].

    update type = 'tool_call' ifTrue: [
        Transcript show: 'Tool: ', update title, ' (', update kind, ')'; cr.
    ].

    update type = 'tool_call_update' ifTrue: [
        Transcript show: 'Tool ', update toolCallId, ': ', update status; cr.
    ].
].

"Send prompt"
session sendPrompt: {
    ACPTextContent new text: 'Create a HelloWorld class'
}.
```

### Example 4: Running Terminal Commands

```smalltalk
"Create terminal"
createRequest := ACPCreateTerminalRequest new
    sessionId: session sessionId;
    command: 'ls';
    args: #('-la');
    cwd: session cwd pathString;
    yourself.

response := client connection sendRequest: createRequest.
terminalId := response terminalId.

"Wait for completion"
waitRequest := ACPWaitForExitRequest new
    sessionId: session sessionId;
    terminalId: terminalId;
    yourself.

result := client connection sendRequest: waitRequest.
Transcript show: result output; cr.
Transcript show: 'Exit code: ', result exitCode asString; cr.

"Release terminal"
client connection sendRequest: (ACPTerminalReleaseRequest new
    sessionId: session sessionId;
    terminalId: terminalId;
    yourself).
```

### Example 5: Requesting Permissions

```smalltalk
"Create permission request"
permRequest := ACPPermissionRequest new
    sessionId: session sessionId;
    title: 'Delete files?';
    description: 'Remove 3 temporary files';
    options: {
        ACPPermissionOption new
            optionId: 'allow';
            kind: #allow_once;
            label: 'Yes, delete';
            yourself.
        ACPPermissionOption new
            optionId: 'reject';
            kind: #reject_once;
            label: 'No, keep files';
            yourself.
    };
    yourself.

"Send request"
response := client connection sendRequest: permRequest.

"Check user choice"
response selectedOptionId = 'allow' ifTrue: [
    Transcript show: 'User approved deletion'; cr.
] ifFalse: [
    Transcript show: 'User rejected deletion'; cr.
].
```

---

## Common Use Cases

### Use Case 1: Code Analysis Agent

```smalltalk
"Create analysis session"
session := client newSession: '/path/to/project' asFileReference.

"Read source file"
sourceCode := client
    readFile: 'MyClass.class.st' asFileReference
    inSession: session.

"Send for analysis"
response := session sendPrompt: {
    ACPTextContent new text: 'Analyze this code for potential issues:'.
    ACPResourceContent new
        resource: (ACPResource new
            uri: 'file:///path/to/project/MyClass.class.st';
            text: sourceCode;
            mimeType: 'text/x-smalltalk';
            yourself);
        yourself.
}.
```

### Use Case 2: Automated Refactoring

```smalltalk
"Create refactoring session"
session := client newSession: '/path/to/project' asFileReference.

"Setup update handler for diffs"
diffs := OrderedCollection new.
session onUpdate: [ :update |
    (update type = 'tool_call_update' and: [
        update content anySatisfy: [ :c | c type = 'diff' ]
    ]) ifTrue: [
        update content
            select: [ :c | c type = 'diff' ]
            thenDo: [ :diff | diffs add: diff ].
    ].
].

"Request refactoring"
response := session sendPrompt: {
    ACPTextContent new text: 'Refactor all methods to use camelCase naming'
}.

"Apply changes"
diffs do: [ :diff |
    Transcript show: 'Applying change to: ', diff path; cr.
    "Apply diff to file"
].
```

### Use Case 3: Test Generation

```smalltalk
"Create test generation session"
session := client newSession: '/path/to/project' asFileReference.

"Read class to test"
classCode := client
    readFile: 'Calculator.class.st' asFileReference
    inSession: session.

"Generate tests"
response := session sendPrompt: {
    ACPTextContent new text: 'Generate comprehensive unit tests for this class:'.
    ACPResourceContent new
        resource: (ACPResource new
            uri: 'file:///path/to/project/Calculator.class.st';
            text: classCode;
            yourself);
        yourself.
}.

"Monitor test file creation"
session onUpdate: [ :update |
    (update type = 'tool_call' and: [ update kind = #edit ]) ifTrue: [
        Transcript show: 'Creating test: ', update location path; cr.
    ].
].
```

### Use Case 4: Documentation Generator

```smalltalk
"Create documentation session"
session := client newSession: '/path/to/project' asFileReference.

"Scan all classes"
classes := '/path/to/project/src' asFileReference allChildrenMatching: '*.class.st'.

"Generate documentation for each"
classes do: [ :classFile |
    | classCode |
    classCode := classFile contents.

    session sendPrompt: {
        ACPTextContent new text: 'Generate markdown documentation for this class:'.
        ACPResourceContent new
            resource: (ACPResource new
                uri: 'file://', classFile fullName;
                text: classCode;
                yourself);
            yourself.
    }.
].
```

### Use Case 5: Interactive Code Review

```smalltalk
"Create review session"
session := client newSession: '/path/to/project' asFileReference.

"Set to ask mode for interactive review"
session setMode: 'ask'.

"Start review"
response := session sendPrompt: {
    ACPTextContent new text: 'Review all changes in the working directory'
}.

"Handle permission requests"
client connection onRequest: 'session/request_permission' do: [ :request |
    "Show permission dialog to user"
    userChoice := self showPermissionDialog: request.

    ACPPermissionResponse new
        selectedOptionId: userChoice;
        yourself
].
```

---

## Configuration

### Client Capabilities

```smalltalk
"Configure client capabilities"
capabilities := ACPClientCapabilities new
    fs: (ACPFSCapabilities new
        readTextFile: true;
        writeTextFile: true;
        yourself);
    terminal: (ACPTerminalCapabilities new
        create: true;
        output: true;
        waitForExit: true;
        kill: true;
        release: true;
        yourself);
    yourself.

client clientCapabilities: capabilities.
```

### Logging

```smalltalk
"Enable debug logging"
ACPLogger default
    level: #debug;
    output: Transcript.

"Log to file"
ACPLogger default
    level: #info;
    output: '/tmp/acp.log' asFileReference writeStream.

"Disable logging"
ACPLogger default level: #none.
```

### Timeouts

```smalltalk
"Set connection timeout"
client connection timeout: 30 seconds.

"Set terminal timeout"
waitRequest := ACPWaitForExitRequest new
    sessionId: session sessionId;
    terminalId: terminalId;
    timeout: 60000; "60 seconds in milliseconds"
    yourself.
```

---

## Testing

### Running Tests

```smalltalk
"Run all tests"
ACPTestSuite run.

"Run specific test"
ACPConnectionTest run.

"Run with coverage"
TestRunner new
    coverage: {ACPConnection. ACPSession. ACPClient};
    run: ACPTestSuite.
```

### Creating Custom Tests

```smalltalk
TestCase subclass: #MyACPTest
    instanceVariableNames: 'client session'
    classVariableNames: ''
    package: 'MyPackage-Tests'

MyACPTest>>setUp
    super setUp.
    client := self createMockClient.
    session := client newSession: FileSystem workingDirectory.

MyACPTest>>testMyFeature
    | response |
    response := session sendPrompt: {
        ACPTextContent new text: 'test'
    }.
    self assert: response stopReason equals: 'end_turn'.
```

---

## Troubleshooting

### Problem: Agent not starting

```smalltalk
"Check agent path"
agent := ACPStdioTransport command: '/path/to/agent'.
agent executable exists ifFalse: [
    self error: 'Agent executable not found'
].

"Check permissions"
agent executable isExecutable ifFalse: [
    self error: 'Agent not executable'
].
```

### Problem: Connection timeout

```smalltalk
"Increase timeout"
client connection timeout: 60 seconds.

"Check agent stderr"
agent := ACPStdioTransport command: '/path/to/agent'.
agent onError: [ :error |
    Transcript show: 'Agent error: ', error; cr.
].
```

### Problem: Invalid JSON

```smalltalk
"Enable protocol logging"
client connection logger level: #debug.

"Validate messages"
message validate ifFalse: [
    Transcript show: 'Invalid message: ', message asString; cr.
].
```

### Problem: Missing capability

```smalltalk
"Check capabilities before use"
client capabilities fs readTextFile ifFalse: [
    self error: 'File reading not supported'
].

client capabilities terminal create ifFalse: [
    self error: 'Terminal creation not supported'
].
```

---

## Next Steps

1. **Read the full specification**: See [ACP-PROTOCOL.md](ACP-PROTOCOL.md)
2. **Study implementation details**: See [IMPLEMENTATION-GUIDE.md](IMPLEMENTATION-GUIDE.md)

---

## Resources

- **Repository**: https://github.com/pharo-llm/pharo-acp
- **Issues**: https://github.com/pharo-llm/pharo-acp/issues
- **Official ACP Site**: https://agentclientprotocol.com/
- **Pharo**: https://pharo.org/
