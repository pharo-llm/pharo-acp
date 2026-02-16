Class {
	#name : 'ACPMessage',
	#superclass : 'Object',
	#instVars : [
		'id'
	],
	#category : 'LLM-Pharo-ACP-Messages',
	#package : 'LLM-Pharo-ACP',
	#tag : 'Messages'
}

{ #category : 'instance creation' }
ACPMessage class >> fromDictionary: aDictionary [
	"Create appropriate message subclass from a dictionary"

	"Check if it's an error response"
	aDictionary at: 'error' ifPresent: [ :errorDict |
		^ ACPError new
			  code: (errorDict at: 'code');
			  message: (errorDict at: 'message');
			  id: (aDictionary at: 'id' ifAbsent: [ nil ]);
			  yourself ].

	"Check if it's a response (has result)"
	aDictionary at: 'result' ifPresent: [ :result |
		^ self responseFromResult: result id: (aDictionary at: 'id') ].

	"Check if it's a request or notification (has method)"
	aDictionary at: 'method' ifPresent: [ :method |
		^ self requestFromMethod: method params: (aDictionary at: 'params' ifAbsent: [ Dictionary new ]) id: (aDictionary at: 'id' ifAbsent: [ nil ]) ].

	ACPProtocolError signal: 'Invalid message format'
]

{ #category : 'instance creation' }
ACPMessage class >> fromJSON: aString [
	"Deserialize a message from a JSON string"

	| dict |
	dict := STONJSON fromString: aString.
	^ self fromDictionary: dict
]

{ #category : 'private' }
ACPMessage class >> requestFromMethod: method params: params id: anId [
	"Create appropriate request from method name"

	| message |
	method = 'initialize' ifTrue: [
		message := ACPInitializeRequest new.
		params at: 'protocolVersion' ifPresent: [ :v | message protocolVersion: v ].
		params at: 'clientInfo' ifPresent: [ :ci |
			message clientInfo: (ACPClientInfo new
				name: (ci at: 'name');
				version: (ci at: 'version' ifAbsent: [ nil ]);
				yourself) ] ].
	method = 'session/new' ifTrue: [
		message := ACPNewSessionRequest new.
		params at: 'cwd' ifPresent: [ :v | message cwd: v ].
		params at: 'mcpServers' ifPresent: [ :v | message mcpServers: v ] ].
	method = 'session/prompt' ifTrue: [
		message := ACPPromptRequest new.
		params at: 'sessionId' ifPresent: [ :v | message sessionId: v ].
		params at: 'prompt' ifPresent: [ :v | message prompt: v ] ].
	method = 'session/cancel' ifTrue: [
		message := ACPCancelRequest new.
		params at: 'sessionId' ifPresent: [ :v | message sessionId: v ] ].

	message ifNil: [
		"Generic request"
		message := ACPGenericRequest new method: method; params: params ].

	anId ifNotNil: [ message id: anId ].
	^ message
]

{ #category : 'private' }
ACPMessage class >> responseFromResult: result id: anId [
	"Create appropriate response from result dictionary"

	| response |
	"Detect type based on result contents"
	(result includesKey: 'protocolVersion') ifTrue: [
		response := ACPInitializeResponse new.
		response protocolVersion: (result at: 'protocolVersion').
		result at: 'agentInfo' ifPresent: [ :ai |
			response agentInfo: (ACPAgentInfo fromDictionary: ai) ].
		result at: 'agentCapabilities' ifPresent: [ :ac |
			response agentCapabilities: (ACPAgentCapabilities fromDictionary: ac) ].
		result at: 'authenticationMethods' ifPresent: [ :am |
			response authenticationMethods: am ].
		response id: anId.
		^ response ].

	"Generic response - try session response"
	(result includesKey: 'sessionId') ifTrue: [
		response := ACPNewSessionResponse new.
		response sessionId: (result at: 'sessionId').
		response id: anId.
		^ response ].

	"Prompt response"
	(result includesKey: 'stopReason') ifTrue: [
		response := ACPPromptResponse new.
		response stopReason: (result at: 'stopReason').
		response id: anId.
		^ response ].

	"Generic response"
	response := ACPGenericResponse new.
	response result: result.
	response id: anId.
	^ response
]

{ #category : 'serialization' }
ACPMessage >> asJSON [
	"Subclasses must override"

	^ self subclassResponsibility
]

{ #category : 'accessing' }
ACPMessage >> id [

	^ id
]

{ #category : 'accessing' }
ACPMessage >> id: anObject [

	id := anObject
]

{ #category : 'testing' }
ACPMessage >> isError [

	^ false
]

{ #category : 'testing' }
ACPMessage >> isNotification [

	^ false
]

{ #category : 'testing' }
ACPMessage >> isRequest [

	^ false
]

{ #category : 'testing' }
ACPMessage >> isResponse [

	^ false
]

{ #category : 'accessing' }
ACPMessage >> method [
	"Subclasses should override"

	^ nil
]
