Class {
	#name : 'ACPMockTransport',
	#superclass : 'ACPTransport',
	#instVars : [
		'sentMessages',
		'responseQueue',
		'autoRespond',
		'responseHandler',
		'sessionCounter',
		'sessionStore',
		'terminalStore'
	],
	#category : 'LLM-Pharo-ACP-Tests-Mock',
	#package : 'LLM-Pharo-ACP-Tests',
	#tag : 'Mock'
}

{ #category : 'accessing' }
ACPMockTransport >> autoRespond [

	^ autoRespond
]

{ #category : 'accessing' }
ACPMockTransport >> autoRespond: aBoolean [

	autoRespond := aBoolean
]

{ #category : 'private' }
ACPMockTransport >> defaultResponseFor: aRequest [
	"Generate default mock response based on request type"

	| method |
	method := aRequest method.
	method = 'initialize' ifTrue: [ ^ self mockInitializeResponse: aRequest ].
	method = 'session/new' ifTrue: [ ^ self mockNewSessionResponse: aRequest ].
	method = 'session/prompt' ifTrue: [ ^ self mockPromptResponse: aRequest ].
	method = 'session/load' ifTrue: [ ^ self mockLoadSessionResponse: aRequest ].
	method = 'fs/read_text_file' ifTrue: [ ^ self mockReadFileResponse: aRequest ].
	method = 'fs/write_text_file' ifTrue: [ ^ self mockWriteFileResponse: aRequest ].
	method = 'terminal/create' ifTrue: [ ^ self mockCreateTerminalResponse: aRequest ].
	method = 'terminal/wait_for_exit' ifTrue: [ ^ self mockWaitForExitResponse: aRequest ].
	method = 'terminal/release' ifTrue: [ ^ self mockTerminalReleaseResponse: aRequest ].
	method = 'session/request_permission' ifTrue: [ ^ self mockPermissionResponse: aRequest ].
	^ self mockGenericResponse: aRequest
]

{ #category : 'private' }
ACPMockTransport >> generateResponseFor: aRequest [
	"Generate a mock response for a request"

	responseHandler ifNotNil: [ ^ responseHandler value: aRequest ].
	^ self defaultResponseFor: aRequest
]

{ #category : 'testing' }
ACPMockTransport >> hasResponse [

	^ responseQueue notEmpty
]

{ #category : 'initialization' }
ACPMockTransport >> initialize [

	super initialize.
	sentMessages := OrderedCollection new.
	responseQueue := OrderedCollection new.
	autoRespond := false.
	sessionCounter := 0.
	sessionStore := Dictionary new.
	terminalStore := Dictionary new
]

{ #category : 'mock responses' }
ACPMockTransport >> mockCreateTerminalResponse: aRequest [

	| response termId |
	sessionCounter := sessionCounter + 1.
	termId := 'term_mock' , sessionCounter asString.
	"Store terminal info for later use"
	terminalStore at: termId put: (Dictionary new
			 at: 'command' put: aRequest command;
			 at: 'args' put: aRequest args;
			 at: 'cwd' put: aRequest cwd;
			 yourself).
	response := ACPGenericResponse new.
	response id: aRequest id.
	response result: (Dictionary new
			 at: 'terminalId' put: termId;
			 yourself).
	^ response
]

{ #category : 'mock responses' }
ACPMockTransport >> mockGenericResponse: aRequest [

	| response |
	response := ACPGenericResponse new.
	response id: aRequest id.
	response result: (Dictionary new
			 at: 'ok' put: true;
			 yourself).
	^ response
]

{ #category : 'mock responses' }
ACPMockTransport >> mockInitializeResponse: aRequest [

	| response |
	response := ACPInitializeResponse new.
	response id: aRequest id.
	response protocolVersion: 1.
	response agentInfo: (ACPAgentInfo new
			 name: 'Mock Agent';
			 version: '1.0.0';
			 yourself).
	response agentCapabilities: (ACPAgentCapabilities new
			 loadSession: true;
			 promptCapabilities: (ACPPromptCapabilities new
					  image: true;
					  audio: true;
					  embeddedContext: true;
					  yourself);
			 sessionModes: {
					 (ACPSessionMode new
						  modeId: 'code';
						  title: 'Code Mode';
						  yourself).
					 (ACPSessionMode new
						  modeId: 'architect';
						  title: 'Architect Mode';
						  yourself) };
			 yourself).
	response authenticationMethods: #().
	^ response
]

{ #category : 'mock responses' }
ACPMockTransport >> mockLoadSessionResponse: aRequest [

	| response |
	response := ACPGenericResponse new.
	response id: aRequest id.
	response result: (Dictionary new
			 at: 'sessionId' put: aRequest sessionId;
			 at: 'loaded' put: true;
			 yourself).
	sessionStore at: aRequest sessionId ifAbsent: [
		sessionStore at: aRequest sessionId put: (Dictionary new
				 at: 'history' put: (OrderedCollection with: 'loaded session');
				 yourself) ].
	^ response
]

{ #category : 'mock responses' }
ACPMockTransport >> mockNewSessionResponse: aRequest [

	| response sid |
	sessionCounter := sessionCounter + 1.
	sid := 'sess_mock' , sessionCounter asString.
	sessionStore at: sid put: (Dictionary new
			 at: 'cwd' put: aRequest cwd;
			 at: 'history' put: OrderedCollection new;
			 yourself).
	response := ACPNewSessionResponse new.
	response id: aRequest id.
	response sessionId: sid.
	^ response
]

{ #category : 'mock responses' }
ACPMockTransport >> mockPermissionResponse: aRequest [

	| response |
	response := ACPPermissionResponse new.
	response id: aRequest id.
	response selectedOptionId: 'allow_once'.
	^ response
]

{ #category : 'mock responses' }
ACPMockTransport >> mockPromptResponse: aRequest [

	| response |
	"Store in session history"
	sessionStore at: aRequest sessionId ifPresent: [ :sess |
		(sess at: 'history') add: aRequest ].
	response := ACPPromptResponse new.
	response id: aRequest id.
	response stopReason: 'end_turn'.
	^ response
]

{ #category : 'mock responses' }
ACPMockTransport >> mockReadFileResponse: aRequest [

	| response fileRef fileContent |
	response := ACPGenericResponse new.
	response id: aRequest id.
	fileRef := aRequest path asFileReference.
	fileRef exists ifFalse: [
		| error |
		error := ACPError new.
		error code: -32002.
		error message: 'File not found: ' , aRequest path.
		error id: aRequest id.
		^ error ].
	fileContent := fileRef contents.
	"Handle line range if specified"
	(aRequest startLine notNil and: [ aRequest startLine > 0 ]) ifTrue: [
		| lines startIdx endIdx |
		lines := fileContent lines.
		startIdx := aRequest startLine.
		endIdx := aRequest lineLimit
			          ifNotNil: [ (startIdx + aRequest lineLimit - 1) min: lines size ]
			          ifNil: [ lines size ].
		fileContent := String cr join: (lines copyFrom: startIdx to: endIdx) ].
	response result: (Dictionary new
			 at: 'content' put: fileContent;
			 yourself).
	^ response
]

{ #category : 'mock responses' }
ACPMockTransport >> mockTerminalReleaseResponse: aRequest [

	| response |
	response := ACPGenericResponse new.
	response id: aRequest id.
	response result: (Dictionary new
			 at: 'released' put: true;
			 yourself).
	^ response
]

{ #category : 'mock responses' }
ACPMockTransport >> mockWaitForExitResponse: aRequest [

	| response output termInfo |
	"Simulate command execution using stored terminal info"
	termInfo := terminalStore
		            at: aRequest terminalId
		            ifAbsent: [ Dictionary new ].
	output := (termInfo at: 'command' ifAbsent: [ 'echo' ]) = 'echo'
		          ifTrue: [ ' ' join: (termInfo at: 'args' ifAbsent: [ #() ]) ]
		          ifFalse: [ 'Mock terminal output' ].
	response := ACPGenericResponse new.
	response id: aRequest id.
	response result: (Dictionary new
			 at: 'output' put: output;
			 at: 'exitCode' put: 0;
			 yourself).
	^ response
]

{ #category : 'mock responses' }
ACPMockTransport >> mockWriteFileResponse: aRequest [

	| response fileRef |
	response := ACPGenericResponse new.
	response id: aRequest id.
	fileRef := aRequest path asFileReference.
	fileRef writeStreamDo: [ :stream |
		stream nextPutAll: aRequest content ].
	response result: (Dictionary new
			 at: 'success' put: true;
			 yourself).
	^ response
]

{ #category : 'operations' }
ACPMockTransport >> nextResponse [
	"Return and remove the next response from the queue"

	responseQueue ifEmpty: [ ^ nil ].
	^ responseQueue removeFirst
]

{ #category : 'accessing' }
ACPMockTransport >> responseHandler [

	^ responseHandler
]

{ #category : 'accessing' }
ACPMockTransport >> responseHandler: aBlock [

	responseHandler := aBlock
]

{ #category : 'accessing' }
ACPMockTransport >> responseQueue [

	^ responseQueue
]

{ #category : 'operations' }
ACPMockTransport >> send: aMessage [

	sentMessages add: aMessage.
	(autoRespond and: [ aMessage isKindOf: ACPMessage ]) ifTrue: [
		aMessage id ifNotNil: [
			responseQueue add: (self generateResponseFor: aMessage) ] ]
]

{ #category : 'accessing' }
ACPMockTransport >> sentMessages [

	^ sentMessages
]

{ #category : 'accessing' }
ACPMockTransport >> sessionStore [

	^ sessionStore
]
