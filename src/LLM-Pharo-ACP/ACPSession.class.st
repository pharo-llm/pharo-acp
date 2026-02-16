Class {
	#name : 'ACPSession',
	#superclass : 'Object',
	#instVars : [
		'sessionId',
		'cwd',
		'connection',
		'updateCallbacks',
		'history',
		'closed'
	],
	#category : 'LLM-Pharo-ACP-Core',
	#package : 'LLM-Pharo-ACP',
	#tag : 'Core'
}

{ #category : 'initialization' }
ACPSession >> initialize [

	super initialize.
	updateCallbacks := OrderedCollection new.
	history := OrderedCollection new.
	closed := false
]

{ #category : 'accessing' }
ACPSession >> sessionId [

	^ sessionId
]

{ #category : 'accessing' }
ACPSession >> sessionId: aString [

	(aString isNil or: [ aString isEmpty ]) ifTrue: [
		ACPValidationError signal: 'Session ID must be a non-empty string' ].
	sessionId := aString
]

{ #category : 'accessing' }
ACPSession >> cwd [

	^ cwd
]

{ #category : 'accessing' }
ACPSession >> cwd: aPathOrString [

	cwd := aPathOrString
]

{ #category : 'accessing' }
ACPSession >> connection [

	^ connection
]

{ #category : 'accessing' }
ACPSession >> connection: anACPConnection [

	connection := anACPConnection.
	"Register for session updates"
	connection onNotification: 'session/update' do: [ :notif |
		(notif sessionId = sessionId) ifTrue: [
			self handleUpdate: notif ] ]
]

{ #category : 'accessing' }
ACPSession >> history [

	^ history
]

{ #category : 'prompts' }
ACPSession >> sendPrompt: contentBlocks [
	"Send a prompt with content blocks and return the response"

	closed ifTrue: [
		ACPSessionClosedError signal: 'Session is closed' ].
	(contentBlocks isNil or: [ contentBlocks isEmpty ]) ifTrue: [
		ACPValidationError signal: 'Prompt content cannot be empty' ].

	| request response |
	request := ACPPromptRequest new
		           sessionId: sessionId;
		           prompt: contentBlocks;
		           yourself.

	response := connection sendRequestAndWait: request.
	history add: request.
	^ response
]

{ #category : 'updates' }
ACPSession >> onUpdate: aBlock [
	"Register a callback for session updates"

	updateCallbacks add: aBlock
]

{ #category : 'private' }
ACPSession >> handleUpdate: anUpdate [
	"Dispatch update to all registered callbacks"

	updateCallbacks do: [ :callback | callback value: anUpdate ]
]

{ #category : 'operations' }
ACPSession >> cancel [
	"Cancel the current operation"

	| notification |
	notification := ACPCancelRequest new
		                sessionId: sessionId;
		                yourself.
	connection sendNotification: notification
]

{ #category : 'operations' }
ACPSession >> setMode: aModeId [
	"Change the session mode"

	(aModeId isNil or: [ aModeId isEmpty ]) ifTrue: [
		ACPValidationError signal: 'Mode ID must be a non-empty string' ].

	| notification |
	notification := ACPSetModeRequest new
		                sessionId: sessionId;
		                modeId: aModeId;
		                yourself.
	connection sendNotification: notification
]

{ #category : 'operations' }
ACPSession >> setConfigOption: anOption value: aValue [
	"Set a configuration option"

	| notification |
	notification := ACPSetConfigOptionRequest new
		                sessionId: sessionId;
		                option: anOption;
		                value: aValue;
		                yourself.
	connection sendNotification: notification
]

{ #category : 'history' }
ACPSession >> clearHistory [

	history removeAll
]

{ #category : 'lifecycle' }
ACPSession >> close [

	closed := true
]

{ #category : 'testing' }
ACPSession >> isClosed [

	^ closed ifNil: [ false ]
]

{ #category : 'testing' }
ACPSession >> isOpen [

	^ self isClosed not
]
