Class {
	#name : 'ACPMockAgent',
	#superclass : 'Object',
	#instVars : [
		'transport',
		'receivedClientInfo',
		'sessions',
		'running',
		'updateCallbacks'
	],
	#category : 'LLM-Pharo-ACP-Tests-Mock',
	#package : 'LLM-Pharo-ACP-Tests',
	#tag : 'Mock'
}

{ #category : 'initialization' }
ACPMockAgent >> initialize [

	super initialize.
	transport := ACPMockTransport new.
	transport autoRespond: true.
	sessions := Dictionary new.
	running := true.
	updateCallbacks := OrderedCollection new.
	self setupMockBehavior
]

{ #category : 'accessing' }
ACPMockAgent >> receivedClientInfo [

	^ receivedClientInfo
]

{ #category : 'private' }
ACPMockAgent >> sendUpdate: aSessionId update: aDictionary [
	"Send an update notification"

	| notification |
	notification := ACPSessionUpdate new
		                sessionId: aSessionId;
		                update: aDictionary;
		                yourself.
	"Store it so the connection can pick it up"
	transport responseQueue add: notification
]

{ #category : 'accessing' }
ACPMockAgent >> sessions [

	^ sessions
]

{ #category : 'private' }
ACPMockAgent >> setupMockBehavior [
	"Configure the transport to capture client info during initialization"

	| originalHandler |
	originalHandler := transport responseHandler.
	transport responseHandler: [ :request |
		| response |
		request method = 'initialize' ifTrue: [
			request clientInfo ifNotNil: [ :ci |
				receivedClientInfo := ci ] ].
		request method = 'session/prompt' ifTrue: [
			self simulatePromptUpdates: request ].
		request method = 'session/set_mode' ifTrue: [
			self simulateModeUpdate: request ].
		response := transport defaultResponseFor: request.
		response ]
]

{ #category : 'private' }
ACPMockAgent >> simulateModeUpdate: aRequest [
	"Simulate mode change update"

	self sendUpdate: aRequest sessionId update: (Dictionary new
			 at: 'type' put: 'current_mode_update';
			 at: 'modeId' put: aRequest modeId;
			 yourself)
]

{ #category : 'private' }
ACPMockAgent >> simulatePermissionRequest: aRequest [
	"Simulate a permission request from agent to client"

	| permRequest |
	permRequest := ACPPermissionRequest new
		                sessionId: aRequest sessionId;
		                title: 'Permission Required';
		                description: 'Allow file deletion?';
		                options: {
				                (ACPPermissionOption new
					                 optionId: 'allow_once';
					                 kind: 'allow';
					                 label: 'Allow Once';
					                 yourself).
				                (ACPPermissionOption new
					                 optionId: 'deny';
					                 kind: 'deny';
					                 label: 'Deny';
					                 yourself) };
		                yourself.
	permRequest id: 9999.
	transport responseQueue add: permRequest
]

{ #category : 'private' }
ACPMockAgent >> simulatePromptUpdates: aRequest [
	"Simulate sending updates during prompt processing"

	| promptText |
	promptText := ''.
	aRequest prompt ifNotNil: [ :p |
		p do: [ :block |
			(block isKindOf: ACPContentBlock) ifTrue: [
				(block isKindOf: ACPTextContent) ifTrue: [
					promptText := block text ] ] ] ].

	"Simulate tool calls for prompts that mention file operations"
	(promptText includesSubstring: 'Read and modify') ifTrue: [
		self simulateToolCallUpdates: aRequest ].

	"Simulate permission request for delete operations"
	(promptText includesSubstring: 'Delete') ifTrue: [
		self simulatePermissionRequest: aRequest ].

	"Always send at least two text updates for streaming tests"
	self sendUpdate: aRequest sessionId update: (Dictionary new
			 at: 'type' put: 'message';
			 at: 'content' put: 'Processing your request...';
			 yourself).
	self sendUpdate: aRequest sessionId update: (Dictionary new
			 at: 'type' put: 'message';
			 at: 'content' put: 'Completed processing.';
			 yourself)
]

{ #category : 'private' }
ACPMockAgent >> simulateToolCallUpdates: aRequest [
	"Simulate tool call updates for file operations"

	"Read tool call"
	self sendUpdate: aRequest sessionId update: (Dictionary new
			 at: 'type' put: 'tool_call';
			 at: 'toolCallId' put: 'tc_read_1';
			 at: 'title' put: 'Read config.json';
			 at: 'kind' put: 'read';
			 at: 'status' put: 'completed';
			 yourself).

	"Edit tool call"
	self sendUpdate: aRequest sessionId update: (Dictionary new
			 at: 'type' put: 'tool_call';
			 at: 'toolCallId' put: 'tc_edit_1';
			 at: 'title' put: 'Edit config.json';
			 at: 'kind' put: 'edit';
			 at: 'status' put: 'completed';
			 yourself)
]

{ #category : 'lifecycle' }
ACPMockAgent >> stop [

	running := false.
	transport close
]

{ #category : 'accessing' }
ACPMockAgent >> transport [

	^ transport
]
