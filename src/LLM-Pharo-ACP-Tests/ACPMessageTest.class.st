Class {
	#name : 'ACPMessageTest',
	#superclass : 'TestCase',
	#category : 'LLM-Pharo-ACP-Tests-Messages',
	#package : 'LLM-Pharo-ACP-Tests',
	#tag : 'Messages'
}

{ #category : 'tests - terminal' }
ACPMessageTest >> testCreateTerminalRequestSerialization [
	"Test serializing create terminal request"

	| request json |
	request := ACPCreateTerminalRequest new
		           sessionId: 'sess_123';
		           command: 'npm';
		           args: #( 'test' );
		           cwd: '/path/to/project';
		           env: (Dictionary new
				            at: 'NODE_ENV' put: 'test';
				            yourself);
		           yourself.

	json := request asJSON.

	self assert: (json at: 'method') equals: 'terminal/create'.
	self assert: (json at: 'params' ifPresent: [ :p | p at: 'command' ]) equals: 'npm'.
	self assert: (json at: 'params' ifPresent: [ :p | (p at: 'args') first ]) equals: 'test'
]

{ #category : 'tests - error' }
ACPMessageTest >> testErrorSerialization [
	"Test serializing error messages"

	| error json |
	error := ACPError new
		         code: -32001;
		         message: 'Authentication failed';
		         data: (Dictionary new
				          at: 'reason' put: 'Invalid token';
				          yourself);
		         yourself.

	json := error asJSON.

	self assert: (json at: 'jsonrpc') equals: '2.0'.
	self assert: (json at: 'error' ifPresent: [ :e | e at: 'code' ]) equals: -32001.
	self assert: (json at: 'error' ifPresent: [ :e | e at: 'message' ]) equals: 'Authentication failed'
]

{ #category : 'tests - serialization' }
ACPMessageTest >> testInitializeRequestSerialization [
	"Test serializing an initialize request to JSON"

	| request json |
	request := ACPInitializeRequest new
		           protocolVersion: 1;
		           clientInfo: (ACPClientInfo new
				            name: 'Test Client';
				            version: '1.0.0';
				            yourself);
		           yourself.

	json := request asJSON.

	self assert: (json at: 'jsonrpc') equals: '2.0'.
	self assert: (json at: 'method') equals: 'initialize'.
	self assert: (json at: 'params' ifPresent: [ :p | p at: 'protocolVersion' ]) equals: 1.
	self assert: (json at: 'params' ifPresent: [ :p | (p at: 'clientInfo') at: 'name' ]) equals: 'Test Client'
]

{ #category : 'tests - serialization' }
ACPMessageTest >> testInitializeResponseDeserialization [
	"Test deserializing an initialize response from JSON"

	| jsonString message |
	jsonString := '{
		"jsonrpc": "2.0",
		"id": 1,
		"result": {
			"protocolVersion": 1,
			"agentInfo": {
				"name": "Test Agent",
				"version": "1.0.0"
			},
			"agentCapabilities": {
				"loadSession": true
			},
			"authenticationMethods": []
		}
	}'.

	message := ACPMessage fromJSON: jsonString.

	self assert: message class equals: ACPInitializeResponse.
	self assert: message protocolVersion equals: 1.
	self assert: message agentInfo name equals: 'Test Agent'.
	self assert: message agentCapabilities loadSession equals: true
]

{ #category : 'tests - serialization' }
ACPMessageTest >> testNewSessionRequestSerialization [
	"Test serializing a new session request"

	| request json |
	request := ACPNewSessionRequest new
		           cwd: '/home/user/project';
		           mcpServers: #(  );
		           yourself.

	json := request asJSON.

	self assert: (json at: 'method') equals: 'session/new'.
	self assert: (json at: 'params' ifPresent: [ :p | p at: 'cwd' ]) equals: '/home/user/project'
]

{ #category : 'tests - notification' }
ACPMessageTest >> testNotificationSerialization [
	"Test that notifications don't include ID"

	| notification json |
	notification := ACPCancelRequest new sessionId: 'sess_123'.

	json := notification asJSON.

	self assert: (json at: 'jsonrpc') equals: '2.0'.
	self assert: (json at: 'method') equals: 'session/cancel'.
	self deny: (json includesKey: 'id')
]

{ #category : 'tests - serialization' }
ACPMessageTest >> testPromptRequestWithContent [
	"Test serializing a prompt request with content blocks"

	| request content json |
	content := {
		           (ACPTextContent new text: 'Hello').
		           (ACPImageContent new
			            mimeType: 'image/png';
			            data: 'base64data';
			            yourself) }.

	request := ACPPromptRequest new
		           sessionId: 'sess_123';
		           prompt: content;
		           yourself.

	json := request asJSON.

	self assert: (json at: 'method') equals: 'session/prompt'.
	self assert: (json at: 'params' ifPresent: [ :p | p at: 'sessionId' ]) equals: 'sess_123'.
	self assert: (json at: 'params' ifPresent: [ :p | (p at: 'prompt') size ]) equals: 2
]

{ #category : 'tests - validation' }
ACPMessageTest >> testProtocolVersionValidation [
	"Test that protocol version must be valid"

	self should: [
		ACPInitializeRequest new
			protocolVersion: 0;
			yourself ] raise: ACPValidationError.

	self should: [
		ACPInitializeRequest new
			protocolVersion: 999;
			yourself ] raise: ACPValidationError
]

{ #category : 'tests - file operations' }
ACPMessageTest >> testReadFileRequestSerialization [
	"Test serializing read file request"

	| request json |
	request := ACPReadFileRequest new
		           sessionId: 'sess_123';
		           path: '/path/to/file.txt';
		           startLine: 10;
		           lineLimit: 50;
		           yourself.

	json := request asJSON.

	self assert: (json at: 'method') equals: 'fs/read_text_file'.
	self assert: (json at: 'params' ifPresent: [ :p | p at: 'path' ]) equals: '/path/to/file.txt'.
	self assert: (json at: 'params' ifPresent: [ :p | p at: 'startLine' ]) equals: 10
]

{ #category : 'tests - roundtrip' }
ACPMessageTest >> testSerializationRoundtrip [
	"Test that messages can be serialized and deserialized"

	| original json deserialized |
	original := ACPInitializeRequest new
		            protocolVersion: 1;
		            clientInfo: (ACPClientInfo new name: 'Test');
		            yourself.

	json := original asJSON asJSONString.
	deserialized := ACPMessage fromJSON: json.

	self assert: deserialized class equals: original class.
	self assert: deserialized protocolVersion equals: original protocolVersion
]

{ #category : 'tests - validation' }
ACPMessageTest >> testSessionIdValidation [
	"Test that session ID must be valid"

	self should: [
		ACPPromptRequest new
			sessionId: nil;
			prompt: #(  );
			yourself ] raise: ACPValidationError.

	self should: [
		ACPPromptRequest new
			sessionId: '';
			prompt: #(  );
			yourself ] raise: ACPValidationError
]

{ #category : 'tests - file operations' }
ACPMessageTest >> testWriteFileRequestSerialization [
	"Test serializing write file request"

	| request json |
	request := ACPWriteFileRequest new
		           sessionId: 'sess_123';
		           path: '/path/to/file.txt';
		           content: 'Hello World';
		           yourself.

	json := request asJSON.

	self assert: (json at: 'method') equals: 'fs/write_text_file'.
	self assert: (json at: 'params' ifPresent: [ :p | p at: 'content' ]) equals: 'Hello World'
]
