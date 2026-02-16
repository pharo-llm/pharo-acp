Class {
	#name : 'ACPClientTest',
	#superclass : 'TestCase',
	#instVars : [
		'client',
		'mockAgent'
	],
	#category : 'LLM-Pharo-ACP-Tests-Client',
	#package : 'LLM-Pharo-ACP-Tests',
	#tag : 'Client'
}

{ #category : 'running' }
ACPClientTest >> setUp [
	"Set up test fixtures"

	super setUp.
	mockAgent := ACPMockAgent new.
	client := ACPClient new
]

{ #category : 'running' }
ACPClientTest >> tearDown [
	"Clean up after tests"

	client ifNotNil: [ client disconnect ].
	super tearDown
]

{ #category : 'tests - initialization' }
ACPClientTest >> testCapabilityNegotiation [
	"Test that capabilities are negotiated during initialization"

	client initialize: mockAgent transport.

	self assert: client capabilities notNil.
	self assert: client agentCapabilities notNil
]

{ #category : 'tests - capabilities' }
ACPClientTest >> testCheckFileSystemCapabilities [
	"Test checking file system capabilities"

	client initialize: mockAgent transport.

	self assert: client capabilities fs readTextFile.
	self assert: client capabilities fs writeTextFile
]

{ #category : 'tests - capabilities' }
ACPClientTest >> testCheckTerminalCapabilities [
	"Test checking terminal capabilities"

	client initialize: mockAgent transport.

	self assert: client capabilities terminal create.
	self assert: client capabilities terminal output.
	self assert: client capabilities terminal waitForExit
]

{ #category : 'tests - lifecycle' }
ACPClientTest >> testClientDisconnect [
	"Test disconnecting client"

	client initialize: mockAgent transport.
	client disconnect.

	self deny: client isInitialized.
	self assert: client sessions isEmpty
]

{ #category : 'tests - initialization' }
ACPClientTest >> testClientInfo [
	"Test client information is sent during initialization"

	| info |
	client initialize: mockAgent transport.
	info := mockAgent receivedClientInfo.

	self assert: info notNil.
	self assert: info name notNil.
	self assert: info version notNil
]

{ #category : 'tests - initialization' }
ACPClientTest >> testClientInitialization [
	"Test initializing client connection"

	| response |
	response := client initialize: mockAgent transport.

	self assert: response notNil.
	self assert: response class equals: ACPInitializeResponse.
	self assert: response protocolVersion equals: 1.
	self assert: client isInitialized
]

{ #category : 'tests - sessions' }
ACPClientTest >> testCreateSession [
	"Test creating a new session"

	| session |
	client initialize: mockAgent transport.
	session := client newSession: FileSystem workingDirectory.

	self assert: session notNil.
	self assert: session sessionId notNil.
	self assert: session cwd equals: FileSystem workingDirectory.
	self assert: (client sessions includesKey: session sessionId)
]

{ #category : 'tests - error handling' }
ACPClientTest >> testFileNotFoundError [
	"Test handling file not found error"

	| session |
	client initialize: mockAgent transport.
	session := client newSession: FileSystem workingDirectory.

	self should: [
		client
			readFile: '/nonexistent/file.txt' asFileReference
			inSession: session ] raise: ACPError
]

{ #category : 'tests - sessions' }
ACPClientTest >> testLoadSession [
	"Test loading an existing session"

	| session loadedSession |
	client initialize: mockAgent transport.

	"First create a session"
	session := client newSession: FileSystem workingDirectory.

	"Then load it"
	loadedSession := client
		                 loadSession: session sessionId
		                 mcpServers: #(  ).

	self assert: loadedSession sessionId equals: session sessionId
]

{ #category : 'tests - sessions' }
ACPClientTest >> testMultipleSessions [
	"Test creating multiple independent sessions"

	| session1 session2 |
	client initialize: mockAgent transport.

	session1 := client newSession: FileSystem workingDirectory.
	session2 := client newSession: FileSystem workingDirectory / 'test'.

	self assert: session1 sessionId ~= session2 sessionId.
	self assert: client sessions size equals: 2
]

{ #category : 'tests - file operations' }
ACPClientTest >> testReadFile [
	"Test reading a file through the client"

	| session content testFile |
	client initialize: mockAgent transport.
	session := client newSession: FileSystem workingDirectory.

	"Create test file"
	testFile := FileSystem workingDirectory / 'test_read.txt'.
	testFile writeStreamDo: [ :stream |
		stream nextPutAll: 'Test content' ].

	[
	content := client readFile: testFile inSession: session.

	self assert: content equals: 'Test content' ]
		ensure: [ testFile ensureDelete ]
]

{ #category : 'tests - file operations' }
ACPClientTest >> testReadFileWithRange [
	"Test reading partial file with line range"

	| session content testFile |
	client initialize: mockAgent transport.
	session := client newSession: FileSystem workingDirectory.

	testFile := FileSystem workingDirectory / 'test_range.txt'.
	testFile writeStreamDo: [ :stream |
			1 to: 100 do: [ :i |
					stream
						nextPutAll: 'Line ' , i asString;
						cr ] ].

	[
		content := client
			           readFile: testFile
			           inSession: session
			           startLine: 10
			           lineLimit: 5.

		self assert: (content includesSubstring: 'Line 10').
		self deny: (content includesSubstring: 'Line 9').
	self deny: (content includesSubstring: 'Line 15') ] ensure: [ testFile ensureDelete ]
]

{ #category : 'tests - error handling' }
ACPClientTest >> testSessionNotFoundError [
	"Test handling invalid session ID"

	client initialize: mockAgent transport.

	self should: [
		client
			readFile: 'test.txt' asFileReference
			inSession: (ACPSession new sessionId: 'invalid') ] raise: ACPError
]

{ #category : 'tests - file operations' }
ACPClientTest >> testWriteFile [
	"Test writing a file through the client"

	| session testFile |
	client initialize: mockAgent transport.
	session := client newSession: FileSystem workingDirectory.

	testFile := FileSystem workingDirectory / 'test_write.txt'.

	[
	client
		writeFile: testFile
		content: 'Hello from test'
		inSession: session.

	self assert: testFile exists.
	self assert: testFile contents equals: 'Hello from test' ]
		ensure: [ testFile ensureDelete ]
]
