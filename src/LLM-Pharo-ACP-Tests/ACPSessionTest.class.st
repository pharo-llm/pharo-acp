Class {
	#name : 'ACPSessionTest',
	#superclass : 'TestCase',
	#instVars : [
		'session',
		'connection',
		'transport'
	],
	#category : 'LLM-Pharo-ACP-Tests-Core',
	#package : 'LLM-Pharo-ACP-Tests',
	#tag : 'Core'
}

{ #category : 'running' }
ACPSessionTest >> setUp [
	"Set up test fixtures"

	super setUp.
	transport := ACPMockTransport new.
	connection := ACPConnection new
		              transport: transport;
		              yourself.

	session := ACPSession new
		           sessionId: 'sess_test123';
		           cwd: FileSystem workingDirectory;
		           connection: connection;
		           yourself
]

{ #category : 'running' }
ACPSessionTest >> tearDown [
	"Clean up after tests"

	session ifNotNil: [ session close ].
	connection ifNotNil: [ connection close ].
	super tearDown
]

{ #category : 'tests - cancellation' }
ACPSessionTest >> testCancelOperation [
	"Test cancelling current operation"

	session cancel.

	self assert: transport sentMessages size equals: 1.
	self
		assert: transport sentMessages first method
		equals: 'session/cancel'.
	self
		assert: transport sentMessages first sessionId
		equals: session sessionId
]

{ #category : 'tests - history' }
ACPSessionTest >> testClearHistory [
	"Test clearing session history"

	| prompt |
	prompt := { (ACPTextContent new text: 'Test message') }.

	session sendPrompt: prompt.
	self assert: session history notEmpty.

	session clearHistory.
	self assert: session history isEmpty
]

{ #category : 'tests - updates' }
ACPSessionTest >> testMultipleUpdateCallbacks [
	"Test that multiple callbacks can be registered"

	| callback1Calls callback2Calls update |
	callback1Calls := 0.
	callback2Calls := 0.

	session onUpdate: [ :upd | callback1Calls := callback1Calls + 1 ].
	session onUpdate: [ :upd | callback2Calls := callback2Calls + 1 ].

	update := ACPSessionUpdate new
		          sessionId: session sessionId;
		          update: Dictionary new;
		          yourself.

	connection handleIncoming: update.

	self assert: callback1Calls equals: 1.
	self assert: callback2Calls equals: 1
]

{ #category : 'tests - prompts' }
ACPSessionTest >> testPromptWithEmptyContent [
	"Test that empty prompt content is rejected"

	self should: [ session sendPrompt: #(  ) ] raise: ACPValidationError
]

{ #category : 'tests - prompts' }
ACPSessionTest >> testSendPromptWithMultipleContent [
	"Test sending a prompt with multiple content blocks"

	| prompt response |
	prompt := {
		          (ACPTextContent new text: 'Analyze this:').
		          (ACPResourceContent new
			           resource: (ACPResource new
					            uri: 'file:///test.js';
					            text: 'const x = 1;';
					            yourself);
			           yourself) }.

	response := session sendPrompt: prompt.

	self assert: response notNil.
	self assert: prompt size equals: 2
]

{ #category : 'tests - prompts' }
ACPSessionTest >> testSendPromptWithTextContent [
	"Test sending a simple text prompt"

	| prompt response |
	prompt := { (ACPTextContent new text: 'Hello agent') }.

	response := session sendPrompt: prompt.

	self assert: response notNil.
	self assert: transport sentMessages size equals: 1.
	self
		assert: transport sentMessages first method
		equals: 'session/prompt'.
	self
		assert: transport sentMessages first sessionId
		equals: session sessionId
]

{ #category : 'tests - lifecycle' }
ACPSessionTest >> testSessionClose [
	"Test closing a session"

	session close.

	self assert: session isClosed.
	self should: [ session sendPrompt: #(  ) ] raise: ACPSessionClosedError
]

{ #category : 'tests - basic' }
ACPSessionTest >> testSessionCreation [
	"Test that session can be created with required attributes"

	self assert: session notNil.
	self assert: session sessionId equals: 'sess_test123'.
	self assert: session cwd equals: FileSystem workingDirectory.
	self assert: session connection equals: connection
]

{ #category : 'tests - history' }
ACPSessionTest >> testSessionHistory [
	"Test that session maintains message history"

	| prompt1 prompt2 |
	prompt1 := { (ACPTextContent new text: 'First message') }.
	prompt2 := { (ACPTextContent new text: 'Second message') }.

	session sendPrompt: prompt1.
	session sendPrompt: prompt2.

	self assert: session history size equals: 2
]

{ #category : 'tests - basic' }
ACPSessionTest >> testSessionIdValidation [
	"Test that session ID must be a valid string"

	self should: [
		ACPSession new
			sessionId: nil;
			yourself ] raise: ACPValidationError.

	self should: [
		ACPSession new
			sessionId: '';
			yourself ] raise: ACPValidationError
]

{ #category : 'tests - updates' }
ACPSessionTest >> testSessionUpdateCallback [
	"Test registering and receiving session updates"

	| receivedUpdates update |
	receivedUpdates := OrderedCollection new.

	session onUpdate: [ :upd | receivedUpdates add: upd ].

	update := ACPSessionUpdate new
		          sessionId: session sessionId;
		          update: (Dictionary new
				           at: 'type' put: 'message';
				           yourself);
		          yourself.

	connection handleIncoming: update.

	self assert: receivedUpdates size equals: 1.
	self assert: receivedUpdates first equals: update
]

{ #category : 'tests - configuration' }
ACPSessionTest >> testSetConfigOption [
	"Test setting configuration option"

	session setConfigOption: 'autoApproveTools' value: true.

	self assert: transport sentMessages size equals: 1.
	self
		assert: transport sentMessages first method
		equals: 'session/set_config_option'.
	self
		assert: transport sentMessages first option
		equals: 'autoApproveTools'.
	self assert: transport sentMessages first value equals: true
]

{ #category : 'tests - modes' }
ACPSessionTest >> testSetMode [
	"Test changing session mode"

	session setMode: 'architect'.

	self assert: transport sentMessages size equals: 1.
	self
		assert: transport sentMessages first method
		equals: 'session/set_mode'.
	self
		assert: transport sentMessages first modeId
		equals: 'architect'
]

{ #category : 'tests - modes' }
ACPSessionTest >> testSetModeValidation [
	"Test that mode ID must be valid"

	self should: [ session setMode: nil ] raise: ACPValidationError.
	self should: [ session setMode: '' ] raise: ACPValidationError
]
