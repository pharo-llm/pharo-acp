Class {
	#name : 'ACPClient',
	#superclass : 'Object',
	#instVars : [
		'connection',
		'sessions',
		'capabilities',
		'agentCapabilities',
		'initialized',
		'clientInfo'
	],
	#category : 'LLM-Pharo-ACP-Core',
	#package : 'LLM-Pharo-ACP',
	#tag : 'Core'
}

{ #category : 'accessing' }
ACPClient >> agentCapabilities [

	^ agentCapabilities
]

{ #category : 'private' }
ACPClient >> buildNegotiatedCapabilities [
	"Build the negotiated capabilities"

	| clientCaps |
	clientCaps := ACPClientCapabilities new
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

	^ ACPCapabilities negotiate: clientCaps with: agentCapabilities
]

{ #category : 'accessing' }
ACPClient >> capabilities [

	^ capabilities
]

{ #category : 'accessing' }
ACPClient >> connection [

	^ connection
]

{ #category : 'lifecycle' }
ACPClient >> disconnect [
	"Disconnect the client"

	sessions valuesDo: [ :session | session close ].
	sessions removeAll.
	connection ifNotNil: [ connection close ].
	initialized := false
]

{ #category : 'initialization' }
ACPClient >> initialize [

	super initialize.
	sessions := Dictionary new.
	initialized := false.
	clientInfo := ACPClientInfo new
		              name: 'Pharo ACP Client';
		              version: '1.0.0';
		              yourself
]

{ #category : 'initialization' }
ACPClient >> initialize: aTransport [
	"Initialize the client with a transport, performing protocol handshake"

	| request response |
	connection := ACPConnection new
		              transport: aTransport;
		              yourself.

	request := ACPInitializeRequest new
		           protocolVersion: 1;
		           clientInfo: clientInfo;
		           yourself.

	response := connection sendRequestAndWait: request.

	"Store capabilities"
	agentCapabilities := response agentCapabilities.
	capabilities := self buildNegotiatedCapabilities.
	initialized := true.

	^ response
]

{ #category : 'testing' }
ACPClient >> isInitialized [

	^ initialized ifNil: [ false ]
]

{ #category : 'sessions' }
ACPClient >> loadSession: aSessionId mcpServers: mcpServers [
	"Load an existing session"

	| request response session |
	request := ACPLoadSessionRequest new
		           sessionId: aSessionId;
		           mcpServers: mcpServers;
		           yourself.

	response := connection sendRequestAndWait: request.

	session := ACPSession new
		           sessionId: aSessionId;
		           connection: connection;
		           yourself.

	"Restore history if available"
	(connection transport isKindOf: ACPMockTransport) ifTrue: [
		connection transport sessionStore
			at: aSessionId
			ifPresent: [ :sessData |
				sessData at: 'history' ifPresent: [ :h |
					h do: [ :entry | session history add: entry ] ] ] ].

	sessions at: aSessionId put: session.
	^ session
]

{ #category : 'sessions' }
ACPClient >> newSession: aCwd [
	"Create a new session with the given working directory"

	| request response session cwdString |
	cwdString := aCwd isString
		             ifTrue: [ aCwd ]
		             ifFalse: [ aCwd pathString ].

	request := ACPNewSessionRequest new
		           cwd: cwdString;
		           mcpServers: #();
		           yourself.

	response := connection sendRequestAndWait: request.

	session := ACPSession new
		           sessionId: response sessionId;
		           cwd: aCwd;
		           connection: connection;
		           yourself.

	sessions at: session sessionId put: session.
	^ session
]

{ #category : 'file operations' }
ACPClient >> readFile: aFileReference inSession: aSession [
	"Read a file in the context of a session"

	| request response path |
	self validateSession: aSession.
	path := aFileReference isString
		        ifTrue: [ aFileReference ]
		        ifFalse: [ aFileReference pathString ].

	request := ACPReadFileRequest new
		           sessionId: aSession sessionId;
		           path: path;
		           yourself.

	response := connection sendRequestAndWait: request.
	^ response result at: 'content'
]

{ #category : 'file operations' }
ACPClient >> readFile: aFileReference inSession: aSession startLine: startLine lineLimit: lineLimit [
	"Read a file with line range in the context of a session"

	| request response path |
	self validateSession: aSession.
	path := aFileReference isString
		        ifTrue: [ aFileReference ]
		        ifFalse: [ aFileReference pathString ].

	request := ACPReadFileRequest new
		           sessionId: aSession sessionId;
		           path: path;
		           startLine: startLine;
		           lineLimit: lineLimit;
		           yourself.

	response := connection sendRequestAndWait: request.
	^ response result at: 'content'
]

{ #category : 'accessing' }
ACPClient >> sessions [

	^ sessions
]

{ #category : 'private' }
ACPClient >> validateSession: aSession [
	"Validate that a session belongs to this client"

	(sessions includesKey: aSession sessionId) ifFalse: [
		ACPError new
			code: -32003;
			message: 'Invalid session: ' , aSession sessionId asString;
			signal ]
]

{ #category : 'file operations' }
ACPClient >> writeFile: aFileReference content: aString inSession: aSession [
	"Write content to a file in the context of a session"

	| request path |
	self validateSession: aSession.
	path := aFileReference isString
		        ifTrue: [ aFileReference ]
		        ifFalse: [ aFileReference pathString ].

	request := ACPWriteFileRequest new
		           sessionId: aSession sessionId;
		           path: path;
		           content: aString;
		           yourself.

	connection sendRequestAndWait: request
]
