Class {
	#name : 'ACPNewSessionResponse',
	#superclass : 'ACPMessage',
	#instVars : [
		'sessionId'
	],
	#category : 'LLM-Pharo-ACP-Messages',
	#package : 'LLM-Pharo-ACP',
	#tag : 'Messages'
}

{ #category : 'accessing' }
ACPNewSessionResponse >> sessionId [

	^ sessionId
]

{ #category : 'accessing' }
ACPNewSessionResponse >> sessionId: aString [

	sessionId := aString
]

{ #category : 'testing' }
ACPNewSessionResponse >> isResponse [

	^ true
]

{ #category : 'serialization' }
ACPNewSessionResponse >> asJSON [

	| json result |
	json := Dictionary new.
	json at: 'jsonrpc' put: '2.0'.
	id ifNotNil: [ json at: 'id' put: id ].
	result := Dictionary new.
	result at: 'sessionId' put: sessionId.
	json at: 'result' put: result.
	^ json
]
