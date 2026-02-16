Class {
	#name : 'ACPConnection',
	#superclass : 'Object',
	#instVars : [
		'transport',
		'messageIdCounter',
		'pendingRequests',
		'notificationHandlers',
		'requestHandlers',
		'closed',
		'timeout',
		'timeoutProcess'
	],
	#category : 'LLM-Pharo-ACP-Core',
	#package : 'LLM-Pharo-ACP',
	#tag : 'Core'
}

{ #category : 'private' }
ACPConnection >> cancelTimeoutFor: aMessageId [
	"Cancel timeout for a request that received a response"

	timeoutProcess ifNotNil: [
		timeoutProcess terminate.
		timeoutProcess := nil ]
]

{ #category : 'lifecycle' }
ACPConnection >> close [

	closed := true.
	timeoutProcess ifNotNil: [
		timeoutProcess terminate.
		timeoutProcess := nil ].
	transport close
]

{ #category : 'private' }
ACPConnection >> dispatchIncomingRequest: aRequest [
	"Dispatch an incoming request from the agent to registered handlers"

	| method handlers |
	method := aRequest method.
	handlers := requestHandlers at: method ifAbsent: [ #() ].
	handlers do: [ :handler | handler value: aRequest ]
]

{ #category : 'private' }
ACPConnection >> dispatchNotification: aNotification [
	"Dispatch a notification to registered handlers"

	| method handlers |
	method := aNotification method.
	handlers := notificationHandlers at: method ifAbsent: [ #() ].
	handlers do: [ :handler | handler value: aNotification ]
]

{ #category : 'receiving' }
ACPConnection >> handleIncoming: aMessage [
	"Handle an incoming message. Returns the response for responses, dispatches notifications."

	"Handle ACPError"
	(aMessage isKindOf: ACPError) ifTrue: [
		pendingRequests removeKey: aMessage id ifAbsent: [ ].
		aMessage signal ].

	"Handle ACPMessage subclass"
	(aMessage isKindOf: ACPMessage) ifTrue: [
		"Check if it's a notification"
		aMessage isNotification ifTrue: [
			self dispatchNotification: aMessage.
			^ aMessage ].

		"Check if it's a response to a pending request"
		aMessage id ifNotNil: [ :respId |
			pendingRequests removeKey: respId ifAbsent: [ ].
			self cancelTimeoutFor: respId.
			^ aMessage ].

		^ aMessage ].

	"Invalid message"
	ACPProtocolError signal: 'Invalid message format'
]

{ #category : 'initialization' }
ACPConnection >> initialize [

	super initialize.
	messageIdCounter := 0.
	pendingRequests := Dictionary new.
	notificationHandlers := Dictionary new.
	requestHandlers := Dictionary new.
	closed := false
]

{ #category : 'testing' }
ACPConnection >> isClosed [

	^ closed ifNil: [ false ]
]

{ #category : 'message ids' }
ACPConnection >> nextMessageId [
	"Generate the next sequential message ID"

	messageIdCounter := messageIdCounter + 1.
	^ messageIdCounter
]

{ #category : 'registration' }
ACPConnection >> onNotification: aMethodString do: aBlock [
	"Register a handler for notifications with the given method"

	| handlers |
	handlers := notificationHandlers
		            at: aMethodString
		            ifAbsentPut: [ OrderedCollection new ].
	handlers add: aBlock
]

{ #category : 'registration' }
ACPConnection >> onRequest: aMethodString do: aBlock [
	"Register a handler for requests with the given method"

	| handlers |
	handlers := requestHandlers
		            at: aMethodString
		            ifAbsentPut: [ OrderedCollection new ].
	handlers add: aBlock
]

{ #category : 'private' }
ACPConnection >> processResponseQueue [
	"Process all queued responses and notifications, returning the first non-notification response"

	| response |
	[ transport hasResponse ] whileTrue: [
		| incoming |
		incoming := transport nextResponse.
		(incoming isKindOf: ACPMessage) ifTrue: [
			incoming isNotification
				ifTrue: [ self dispatchNotification: incoming ]
				ifFalse: [
					"Check if it's an incoming request from the agent"
					(incoming method notNil and: [ incoming id notNil ])
						ifTrue: [ self dispatchIncomingRequest: incoming ]
						ifFalse: [
							(incoming isKindOf: ACPError)
								ifTrue: [ incoming signal ]
								ifFalse: [ response ifNil: [ response := incoming ] ] ] ] ].
		(incoming isKindOf: ACPError) ifTrue: [ incoming signal ] ].
	^ response
]

{ #category : 'sending' }
ACPConnection >> sendNotification: aNotification [
	"Send a notification message (no ID assigned)"
	
	transport send: aNotification.
	(transport respondsTo: #hasResponse) ifTrue: [
		transport hasResponse ifTrue: [ self processResponseQueue ] ]
]

{ #category : 'sending' }
ACPConnection >> sendRequest: aRequest [
	"Send a request message, assigning it an ID.
	 If the transport auto-responded, returns the response directly.
	 Otherwise returns the message ID."

	| messageId |
	messageId := self nextMessageId.
	aRequest id: messageId.
	pendingRequests at: messageId put: aRequest.
	transport send: aRequest.
	timeout ifNotNil: [ self startTimeoutFor: messageId on: Processor activeProcess  ]. 
	(transport respondsTo: #hasResponse) ifTrue: [
			transport hasResponse ifTrue: [
					| response |
					response := self processResponseQueue.
					response ifNotNil: [
							pendingRequests removeKey: messageId ifAbsent: [ ].
							^ response ] ] ].

	^ messageId
]

{ #category : 'sending' }
ACPConnection >> sendRequestAndWait: aRequest [
	"Send a request and return the response synchronously.
	 Processes all queued notifications and responses."

	| result response |
	result := self sendRequest: aRequest.

	"If sendRequest already returned a response object, use it"
	(result isKindOf: ACPMessage) ifTrue: [ ^ result ].

	"Otherwise check the queue"
	response := self processResponseQueue.
	response ifNotNil: [
		pendingRequests removeKey: result ifAbsent: [ ].
		^ response ].

	"If still no response, ask transport to generate one"
	(transport respondsTo: #generateResponseFor:) ifTrue: [
		response := transport generateResponseFor: aRequest.
		response ifNotNil: [
			response id: result.
			pendingRequests removeKey: result ifAbsent: [ ] ].
		^ response ].

	^ nil
]

{ #category : 'private' }
ACPConnection >> startTimeoutFor: aMessageId [
	"Start a timeout process for a pending request"

	timeoutProcess := [
	                   timeout wait.
	                   (pendingRequests includesKey: aMessageId) ifTrue: [
		                   pendingRequests removeKey: aMessageId.
		                   ACPTimeoutError signal:
			                   'Request timed out: ' , aMessageId asString ] ]
		                   forkAt: Processor userBackgroundPriority
]

{ #category : 'sending' }
ACPConnection >> startTimeoutFor: aMessageId on: aProcess [

timeoutProcess := [
	timeout wait.
		(pendingRequests includesKey: aMessageId) ifTrue: [
			pendingRequests removeKey: aMessageId.
			aProcess
				signalException: (ACPTimeoutError new
					messageText: 'Request timed out: ' , aMessageId asString;
					yourself) ] ]
		forkAt: Processor userBackgroundPriority
]

{ #category : 'accessing' }
ACPConnection >> timeout: aDuration [

	timeout := aDuration
]

{ #category : 'accessing' }
ACPConnection >> transport [

	^ transport
]

{ #category : 'accessing' }
ACPConnection >> transport: anACPTransport [

	transport := anACPTransport
]
