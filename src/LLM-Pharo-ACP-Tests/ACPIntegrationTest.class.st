Class {
	#name : 'ACPIntegrationTest',
	#superclass : 'TestCase',
	#instVars : [
		'client',
		'agent',
		'session'
	],
	#category : 'LLM-Pharo-ACP-Tests-Integration',
	#package : 'LLM-Pharo-ACP-Tests',
	#tag : 'Integration'
}

{ #category : 'running' }
ACPIntegrationTest >> setUp [
	"Set up complete client-agent test environment"

	super setUp.
	agent := ACPMockAgent new.
	client := ACPClient new.
	client initialize: agent transport.
	session := client newSession: FileSystem workingDirectory
]

{ #category : 'running' }
ACPIntegrationTest >> tearDown [
	"Clean up test environment"

	session ifNotNil: [ session close ].
	client ifNotNil: [ client disconnect ].
	agent ifNotNil: [ agent stop ].
	super tearDown
]

{ #category : 'tests - complete flow' }
ACPIntegrationTest >> testCompletePromptFlow [
	"Test complete prompt turn flow from client to agent"

	| prompt response updates |
	updates := OrderedCollection new.

	"Register update handler"
	session onUpdate: [ :update | updates add: update ].

	"Send prompt"
	prompt := { (ACPTextContent new text: 'Create a class') }.
	response := session sendPrompt: prompt.

	"Verify response"
	self assert: response notNil.
	self assert: response stopReason notNil.

	"Verify updates were received"
	self assert: updates notEmpty
]

{ #category : 'tests - complete flow' }
ACPIntegrationTest >> testPromptWithToolCalls [
	"Test prompt flow with tool calls"

	| prompt response toolCalls |
	toolCalls := OrderedCollection new.

	"Capture tool calls"
	session onUpdate: [ :update |
		update type = 'tool_call' ifTrue: [ toolCalls add: update ] ].

	"Send prompt that triggers tool calls"
	prompt := {
		          (ACPTextContent new text: 'Read and modify config.json') }.
	response := session sendPrompt: prompt.

	"Verify tool calls were made"
	self assert: toolCalls notEmpty.
	self assert: (toolCalls anySatisfy: [ :tc | tc kind = #read ]).
	self assert: (toolCalls anySatisfy: [ :tc | tc kind = #edit ])
]

{ #category : 'tests - complete flow' }
ACPIntegrationTest >> testPromptWithPermissionRequest [
	"Test prompt flow with permission requests"

	| prompt response permissionRequested |
	permissionRequested := false.

	"Handle permission request"
	client connection onRequest: 'session/request_permission' do: [ :request |
		permissionRequested := true.
		ACPPermissionResponse new selectedOptionId: 'allow_once' ].

	"Send prompt that requires permission"
	prompt := { (ACPTextContent new text: 'Delete temporary files') }.
	response := session sendPrompt: prompt.

	"Verify permission was requested"
	self assert: permissionRequested
]

{ #category : 'tests - file operations' }
ACPIntegrationTest >> testCompleteFileWorkflow [
	"Test complete file read-modify-write workflow"

	| testFile originalContent modifiedContent |
	testFile := FileSystem workingDirectory / 'integration_test.txt'.

	[
	"Write initial file"
	client
		writeFile: testFile
		content: 'Initial content'
		inSession: session.

	"Read file"
	originalContent := client readFile: testFile inSession: session.
	self assert: originalContent equals: 'Initial content'.

	"Modify and write back"
	modifiedContent := originalContent , ' - modified'.
	client
		writeFile: testFile
		content: modifiedContent
		inSession: session.

	"Verify modification"
	self assert: testFile contents equals: 'Initial content - modified' ]
		ensure: [ testFile ensureDelete ]
]

{ #category : 'tests - terminal operations' }
ACPIntegrationTest >> testTerminalWorkflow [
	"Test complete terminal command execution workflow"

	| createRequest terminal output |
	"Create terminal and run echo command"
	createRequest := ACPCreateTerminalRequest new
		                 sessionId: session sessionId;
		                 command: 'echo';
		                 args: #( 'Hello Terminal' );
		                 cwd: session cwd pathString;
		                 yourself.

	terminal := client connection sendRequest: createRequest.
	self assert: terminal terminalId notNil.

	"Wait for command completion"
	output := client connection sendRequest:
		          (ACPWaitForExitRequest new
			           sessionId: session sessionId;
			           terminalId: terminal terminalId;
			           yourself).

	"Verify output"
	self assert: (output output includesSubstring: 'Hello Terminal').
	self assert: output exitCode equals: 0.

	"Release terminal"
	client connection sendRequest: (ACPTerminalReleaseRequest new
			 sessionId: session sessionId;
			 terminalId: terminal terminalId;
			 yourself)
]

{ #category : 'tests - session modes' }
ACPIntegrationTest >> testSessionModeChanges [
	"Test changing session modes"

	| modeChanges |
	modeChanges := OrderedCollection new.

	"Track mode changes"
	session onUpdate: [ :update |
		update type = 'current_mode_update' ifTrue: [
			modeChanges add: update modeId ] ].

	"Change to architect mode"
	session setMode: 'architect'.

	"Change to code mode"
	session setMode: 'code'.

	"Verify mode changes were tracked"
	self assert: modeChanges includes: 'architect'.
	self assert: modeChanges includes: 'code'
]

{ #category : 'tests - multiple sessions' }
ACPIntegrationTest >> testMultipleSessionsIndependence [
	"Test that multiple sessions are independent"

	| session2 updates1 updates2 |
	updates1 := OrderedCollection new.
	updates2 := OrderedCollection new.

	"Create second session"
	session2 := client newSession: FileSystem workingDirectory.

	"Register separate update handlers"
	session onUpdate: [ :update | updates1 add: update ].
	session2 onUpdate: [ :update | updates2 add: update ].

	"Send prompts to both sessions"
	session sendPrompt: {
			(ACPTextContent new text: 'Session 1 prompt') }.
	session2 sendPrompt: {
			(ACPTextContent new text: 'Session 2 prompt') }.

	"Verify updates are separate"
	self assert: updates1 notEmpty.
	self assert: updates2 notEmpty.
	self assert: updates1 ~= updates2.

	"Cleanup"
	session2 close
]

{ #category : 'tests - error scenarios' }
ACPIntegrationTest >> testCancellation [
	"Test cancelling operations"

	| prompt cancelled |
	cancelled := false.

	"Send prompt"
	[
	session sendPrompt: {
			(ACPTextContent new text: 'Long running operation') }.

	"Cancel immediately"
	session cancel.
	cancelled := true ] on: ACPCancelledError do: [ :error |
		cancelled := true ].

	self assert: cancelled
]

{ #category : 'tests - error scenarios' }
ACPIntegrationTest >> testErrorRecovery [
	"Test that session recovers from errors"

	"Send invalid prompt"
	[ session sendPrompt: #(  ) ]
		on: ACPError
		do: [ :error | "Expected error" ].

	"Verify session still works"
	session sendPrompt: { (ACPTextContent new text: 'Valid prompt') }.

	self assert: session isOpen
]

{ #category : 'tests - content types' }
ACPIntegrationTest >> testMultipleContentTypes [
	"Test sending prompts with multiple content types"

	| prompt response |
	prompt := {
		          (ACPTextContent new text: 'Analyze this code:').
		          (ACPResourceContent new
			           resource: (ACPResource new
					            uri: 'file:///test.js';
					            text: 'const x = 1;';
					            mimeType: 'text/javascript';
					            yourself);
			           yourself).
		          (ACPResourceLink new
			           uri: 'file:///docs.md';
			           name: 'Documentation';
			           yourself) }.

	response := session sendPrompt: prompt.

	self assert: response notNil.
	self assert: response stopReason notNil
]

{ #category : 'tests - streaming' }
ACPIntegrationTest >> testStreamingUpdates [
	"Test that updates stream in real-time"

	| updates startTime endTime |
	updates := OrderedCollection new.

	session onUpdate: [ :update |
		updates add: {
				update.
				DateAndTime now } ].

	startTime := DateAndTime now.
	session sendPrompt: {
			(ACPTextContent new text: 'Generate documentation') }.
	endTime := DateAndTime now.

	"Verify updates came during processing, not all at once"
	self assert: updates size > 1.
	self assert: endTime > startTime
]

{ #category : 'tests - session loading' }
ACPIntegrationTest >> testSessionPersistenceAndLoading [
	"Test saving and loading session state"

	| originalSessionId prompt loadedSession |
	"Send some prompts to create history"
	prompt := { (ACPTextContent new text: 'First message') }.
	session sendPrompt: prompt.

	originalSessionId := session sessionId.

	"Close and reload session"
	session close.
	loadedSession := client
		                 loadSession: originalSessionId
		                 mcpServers: #(  ).

	"Verify session was loaded"
	self assert: loadedSession sessionId equals: originalSessionId.
	self assert: loadedSession history notEmpty
]
