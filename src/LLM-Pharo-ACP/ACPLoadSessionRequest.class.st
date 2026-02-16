Class {
	#name : 'ACPLoadSessionRequest',
	#superclass : 'ACPMessage',
	#instVars : [
		'sessionId',
		'mcpServers'
	],
	#category : 'LLM-Pharo-ACP-Messages',
	#package : 'LLM-Pharo-ACP',
	#tag : 'Messages'
}

{ #category : 'serialization' }
ACPLoadSessionRequest >> asJSON [

	| json params |
	json := Dictionary new.
	json at: 'jsonrpc' put: '2.0'.
	id ifNotNil: [ json at: 'id' put: id ].
	json at: 'method' put: self method.
	params := Dictionary new.
	params at: 'sessionId' put: sessionId.
	params at: 'mcpServers' put: self mcpServers.
	json at: 'params' put: params.
	^ json
]

{ #category : 'testing' }
ACPLoadSessionRequest >> isRequest [

	^ true
]

{ #category : 'accessing' }
ACPLoadSessionRequest >> mcpServers [

	^ mcpServers ifNil: [ #() ]
]

{ #category : 'accessing' }
ACPLoadSessionRequest >> mcpServers: anArray [

	mcpServers := anArray
]

{ #category : 'accessing' }
ACPLoadSessionRequest >> method [

	^ 'session/load'
]

{ #category : 'accessing' }
ACPLoadSessionRequest >> sessionId [

	^ sessionId
]

{ #category : 'accessing' }
ACPLoadSessionRequest >> sessionId: aString [

	sessionId := aString
]
