Class {
	#name : 'ACPConnectionTest',
	#superclass : 'TestCase',
	#instVars : [
		'connection',
		'transport'
	],
	#category : 'LLM-Pharo-ACP-Tests-Core',
	#package : 'LLM-Pharo-ACP-Tests',
	#tag : 'Core'
}

{ #category : 'running' }
ACPConnectionTest >> setUp [
	"Set up test fixtures"

	super setUp.
	transport := ACPMockTransport new.
	connection := ACPConnection new
		             transport: transport;
		             yourself
]

{ #category : 'running' }
ACPConnectionTest >> tearDown [
	"Clean up after tests"

	connection ifNotNil: [ connection close ].
	super tearDown
]

{ #category : 'tests - basic' }
ACPConnectionTest >> testConnectionCreation [
	"Test that connection can be created"

	self assert: connection notNil.
	self assert: connection transport equals: transport
]

{ #category : 'tests - basic' }
ACPConnectionTest >> testMessageIdGeneration [
	"Test that message IDs are generated sequentially"

	| id1 id2 id3 |
	id1 := connection nextMessageId.
	id2 := connection nextMessageId.
	id3 := connection nextMessageId.

	self assert: id1 equals: 1.
	self assert: id2 equals: 2.
	self assert: id3 equals: 3
]

{ #category : 'tests - sending' }
ACPConnectionTest >> testSendRequest [
	"Test sending a request message"

	| request messageId |
	request := ACPInitializeRequest new
		           protocolVersion: 1;
		           yourself.

	messageId := connection sendRequest: request.

	self assert: messageId notNil.
	self assert: request id equals: messageId.
	self assert: transport sentMessages size equals: 1.
	self assert: transport sentMessages first equals: request
]

{ #category : 'tests - sending' }
ACPConnectionTest >> testSendNotification [
	"Test sending a notification message"

	| notification |
	notification := ACPCancelRequest new
		                sessionId: 'sess_123';
		                yourself.

	connection sendNotification: notification.

	self assert: notification id isNil.
	self assert: transport sentMessages size equals: 1.
	self assert: transport sentMessages first equals: notification
]

{ #category : 'tests - sending' }
ACPConnectionTest >> testSendMultipleRequests [
	"Test sending multiple requests maintains separate IDs"

	| request1 request2 id1 id2 |
	request1 := ACPInitializeRequest new protocolVersion: 1.
	request2 := ACPPromptRequest new
		            sessionId: 'sess_123';
		            prompt: #(  ).

	id1 := connection sendRequest: request1.
	id2 := connection sendRequest: request2.

	self assert: id1 ~= id2.
	self assert: transport sentMessages size equals: 2
]

{ #category : 'tests - receiving' }
ACPConnectionTest >> testReceiveResponse [
	"Test receiving a response message"

	| request response receivedResponse |
	request := ACPInitializeRequest new protocolVersion: 1.
	connection sendRequest: request.

	response := ACPInitializeResponse new
		            protocolVersion: 1;
		            id: request id;
		            yourself.

	receivedResponse := connection handleIncoming: response.

	self assert: receivedResponse equals: response
]

{ #category : 'tests - receiving' }
ACPConnectionTest >> testReceiveNotification [
	"Test receiving a notification message"

	| notification received |
	received := nil.
	connection onNotification: 'session/update' do: [ :notif |
		received := notif ].

	notification := ACPSessionUpdate new
		                sessionId: 'sess_123';
		                update: Dictionary new;
		                yourself.

	connection handleIncoming: notification.

	self assert: received equals: notification
]

{ #category : 'tests - error handling' }
ACPConnectionTest >> testReceiveError [
	"Test receiving an error response"

	| request error |
	request := ACPInitializeRequest new protocolVersion: 1.
	connection sendRequest: request.

	error := ACPError new
		         code: -32001;
		         message: 'Authentication failed';
		         id: request id;
		         yourself.

	self should: [ connection handleIncoming: error ] raise: ACPError
]

{ #category : 'tests - error handling' }
ACPConnectionTest >> testInvalidMessageHandling [
	"Test handling of invalid messages"

	| invalidMessage |
	invalidMessage := Dictionary new
		                  at: 'invalid' put: 'data';
		                  yourself.

	self
		should: [ connection handleIncoming: invalidMessage ]
		raise: ACPProtocolError
]

{ #category : 'tests - lifecycle' }
ACPConnectionTest >> testConnectionClose [
	"Test closing the connection"

	connection close.

	self assert: connection isClosed.
	self assert: transport isClosed
]

{ #category : 'tests - lifecycle' }
ACPConnectionTest >> testConnectionTimeout [
	"Test connection timeout handling"

	| request |
	connection timeout: 1 second.
	request := ACPInitializeRequest new protocolVersion: 1.

	self
		should: [
			connection sendRequest: request.
			2 seconds wait ]
		raise: ACPTimeoutError
		description: 'Should timeout after 1 second'
]
