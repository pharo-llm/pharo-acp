Class {
	#name : 'ACPTerminalReleaseRequest',
	#superclass : 'ACPMessage',
	#instVars : [
		'sessionId',
		'terminalId'
	],
	#category : 'LLM-Pharo-ACP-Messages',
	#package : 'LLM-Pharo-ACP',
	#tag : 'Messages'
}

{ #category : 'serialization' }
ACPTerminalReleaseRequest >> asJSON [

	| json params |
	json := Dictionary new.
	json at: 'jsonrpc' put: '2.0'.
	id ifNotNil: [ json at: 'id' put: id ].
	json at: 'method' put: self method.
	params := Dictionary new.
	params at: 'sessionId' put: sessionId.
	params at: 'terminalId' put: terminalId.
	json at: 'params' put: params.
	^ json
]

{ #category : 'testing' }
ACPTerminalReleaseRequest >> isRequest [

	^ true
]

{ #category : 'accessing' }
ACPTerminalReleaseRequest >> method [

	^ 'terminal/release'
]

{ #category : 'accessing' }
ACPTerminalReleaseRequest >> sessionId [

	^ sessionId
]

{ #category : 'accessing' }
ACPTerminalReleaseRequest >> sessionId: aString [

	sessionId := aString
]

{ #category : 'accessing' }
ACPTerminalReleaseRequest >> terminalId [

	^ terminalId
]

{ #category : 'accessing' }
ACPTerminalReleaseRequest >> terminalId: aString [

	terminalId := aString
]
