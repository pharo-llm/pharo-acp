Class {
	#name : 'ACPInitializeRequest',
	#superclass : 'ACPMessage',
	#instVars : [
		'protocolVersion',
		'clientInfo'
	],
	#category : 'LLM-Pharo-ACP-Messages',
	#package : 'LLM-Pharo-ACP',
	#tag : 'Messages'
}

{ #category : 'serialization' }
ACPInitializeRequest >> asJSON [

	| json params |
	json := Dictionary new.
	json at: 'jsonrpc' put: '2.0'.
	id ifNotNil: [ json at: 'id' put: id ].
	json at: 'method' put: self method.
	params := Dictionary new.
	params at: 'protocolVersion' put: protocolVersion.
	clientInfo ifNotNil: [ params at: 'clientInfo' put: clientInfo asJSON ].
	json at: 'params' put: params.
	^ json
]

{ #category : 'accessing' }
ACPInitializeRequest >> clientInfo [

	^ clientInfo
]

{ #category : 'accessing' }
ACPInitializeRequest >> clientInfo: anACPClientInfo [

	clientInfo := anACPClientInfo
]

{ #category : 'testing' }
ACPInitializeRequest >> isRequest [

	^ true
]

{ #category : 'accessing' }
ACPInitializeRequest >> method [

	^ 'initialize'
]

{ #category : 'accessing' }
ACPInitializeRequest >> protocolVersion [

	^ protocolVersion
]

{ #category : 'accessing' }
ACPInitializeRequest >> protocolVersion: anInteger [

	(anInteger isNil or: [ anInteger < 1 or: [ anInteger > 100 ] ])
		ifTrue: [
		ACPValidationError signal: 'Invalid protocol version: ' , anInteger asString ].
	protocolVersion := anInteger
]
