Class {
	#name : 'ACPToolCallTest',
	#superclass : 'TestCase',
	#category : 'LLM-Pharo-ACP-Tests-ToolCalls',
	#package : 'LLM-Pharo-ACP-Tests',
	#tag : 'ToolCalls'
}

{ #category : 'tests - creation' }
ACPToolCallTest >> testToolCallCreation [
	"Test creating a tool call"

	| toolCall |
	toolCall := ACPToolCall new
		            toolCallId: 'tool_123';
		            title: 'Read config.json';
		            kind: #read;
		            status: #pending;
		            yourself.

	self assert: toolCall toolCallId equals: 'tool_123'.
	self assert: toolCall title equals: 'Read config.json'.
	self assert: toolCall kind equals: #read.
	self assert: toolCall status equals: #pending
]

{ #category : 'tests - creation' }
ACPToolCallTest >> testToolCallWithLocation [
	"Test tool call with location information"

	| toolCall location json |
	location := ACPLocation new
		            path: '/path/to/file.js';
		            line: 42;
		            yourself.

	toolCall := ACPToolCall new
		            toolCallId: 'tool_123';
		            title: 'Edit file';
		            kind: #edit;
		            status: #pending;
		            location: location;
		            yourself.

	json := toolCall asJSON.

	self assert: (json at: 'location' ifPresent: [ :l | l at: 'path' ]) equals: '/path/to/file.js'.
	self assert: (json at: 'location' ifPresent: [ :l | l at: 'line' ]) equals: 42
]

{ #category : 'tests - kinds' }
ACPToolCallTest >> testToolCallKinds [
	"Test all valid tool call kinds"

	| validKinds |
	validKinds := #( #read #edit #delete #move #search #execute #think
	                 #fetch #other ).

	validKinds do: [ :kind |
		| toolCall |
		toolCall := ACPToolCall new
			            toolCallId: 'tool_test';
			            title: 'Test';
			            kind: kind;
			            status: #pending;
			            yourself.

		self assert: toolCall kind equals: kind ]
]

{ #category : 'tests - kinds' }
ACPToolCallTest >> testInvalidToolCallKind [
	"Test that invalid kind raises error"

	self should: [
		ACPToolCall new
			toolCallId: 'tool_test';
			title: 'Test';
			kind: #invalid;
			status: #pending;
			yourself ] raise: ACPValidationError
]

{ #category : 'tests - status' }
ACPToolCallTest >> testToolCallStatusTransitions [
	"Test tool call status lifecycle"

	| toolCall |
	toolCall := ACPToolCall new
		            toolCallId: 'tool_123';
		            title: 'Test';
		            kind: #read;
		            status: #pending;
		            yourself.

	self assert: toolCall status equals: #pending.

	toolCall status: #in_progress.
	self assert: toolCall status equals: #in_progress.

	toolCall status: #completed.
	self assert: toolCall status equals: #completed
]

{ #category : 'tests - status' }
ACPToolCallTest >> testInvalidStatusTransition [
	"Test that invalid status raises error"

	self should: [
		ACPToolCall new
			toolCallId: 'tool_test';
			title: 'Test';
			kind: #read;
			status: #invalid_status;
			yourself ] raise: ACPValidationError
]

{ #category : 'tests - updates' }
ACPToolCallTest >> testToolCallUpdate [
	"Test creating tool call update"

	| update json |
	update := ACPToolCallUpdate new
		          toolCallId: 'tool_123';
		          status: #completed;
		          content: { (ACPTextContent new text: 'Done') };
		          yourself.

	json := update asJSON.

	self assert: (json at: 'type') equals: 'tool_call_update'.
	self assert: (json at: 'toolCallId') equals: 'tool_123'.
	self assert: (json at: 'status') equals: 'completed'
]

{ #category : 'tests - updates' }
ACPToolCallTest >> testPartialToolCallUpdate [
	"Test that updates can contain only changed fields"

	| update json |
	update := ACPToolCallUpdate new
		          toolCallId: 'tool_123';
		          status: #in_progress;
		          yourself.

	json := update asJSON.

	self assert: (json includesKey: 'toolCallId').
	self assert: (json includesKey: 'status').
	self deny: (json includesKey: 'content')
]

{ #category : 'tests - content' }
ACPToolCallTest >> testToolCallWithDiffContent [
	"Test tool call with diff content"

	| toolCall diff |
	diff := ACPDiffContent new
		        path: '/path/to/file.js';
		        oldText: 'const x = 1;';
		        newText: 'const x = 2;';
		        yourself.

	toolCall := ACPToolCall new
		            toolCallId: 'tool_123';
		            title: 'Update variable';
		            kind: #edit;
		            status: #completed;
		            content: { diff };
		            yourself.

	self assert: toolCall content first class equals: ACPDiffContent
]

{ #category : 'tests - content' }
ACPToolCallTest >> testToolCallWithTerminalContent [
	"Test tool call with terminal content"

	| toolCall terminal |
	terminal := ACPTerminalContent new
		            terminalId: 'term_123';
		            command: 'npm test';
		            exitCode: 0;
		            yourself.

	toolCall := ACPToolCall new
		            toolCallId: 'tool_123';
		            title: 'Run tests';
		            kind: #execute;
		            status: #completed;
		            content: { terminal };
		            yourself.

	self assert: toolCall content first class equals: ACPTerminalContent
]

{ #category : 'tests - serialization' }
ACPToolCallTest >> testToolCallSerialization [
	"Test serializing complete tool call"

	| toolCall json |
	toolCall := ACPToolCall new
		            toolCallId: 'tool_123';
		            title: 'Read file';
		            kind: #read;
		            status: #completed;
		            location: (ACPLocation new path: '/test.js');
		            content: { (ACPTextContent new text: 'Success') };
		            yourself.

	json := toolCall asJSON.

	self assert: (json at: 'type') equals: 'tool_call'.
	self assert: (json at: 'toolCallId') equals: 'tool_123'.
	self assert: (json at: 'title') equals: 'Read file'.
	self assert: (json at: 'kind') equals: 'read'.
	self assert: (json at: 'status') equals: 'completed'.
	self assert: (json at: 'content') notEmpty
]

{ #category : 'tests - validation' }
ACPToolCallTest >> testToolCallValidation [
	"Test tool call validation requirements"

	"Missing toolCallId"
	self should: [
		ACPToolCall new
			title: 'Test';
			kind: #read;
			status: #pending;
			yourself ] raise: ACPValidationError.

	"Missing title"
	self should: [
		ACPToolCall new
			toolCallId: 'tool_123';
			kind: #read;
			status: #pending;
			yourself ] raise: ACPValidationError.

	"Missing kind"
	self should: [
		ACPToolCall new
			toolCallId: 'tool_123';
			title: 'Test';
			status: #pending;
			yourself ] raise: ACPValidationError
]
