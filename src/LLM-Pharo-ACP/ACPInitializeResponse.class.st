Class {
	#name : 'ACPInitializeResponse',
	#superclass : 'ACPMessage',
	#instVars : [
		'protocolVersion',
		'agentInfo',
		'agentCapabilities',
		'authenticationMethods'
	],
	#category : 'LLM-Pharo-ACP-Messages',
	#package : 'LLM-Pharo-ACP',
	#tag : 'Messages'
}

{ #category : 'accessing' }
ACPInitializeResponse >> agentCapabilities [

	^ agentCapabilities
]

{ #category : 'accessing' }
ACPInitializeResponse >> agentCapabilities: anACPAgentCapabilities [

	agentCapabilities := anACPAgentCapabilities
]

{ #category : 'accessing' }
ACPInitializeResponse >> agentInfo [

	^ agentInfo
]

{ #category : 'accessing' }
ACPInitializeResponse >> agentInfo: anACPAgentInfo [

	agentInfo := anACPAgentInfo
]

{ #category : 'serialization' }
ACPInitializeResponse >> asJSON [

	| json result |
	json := Dictionary new.
	json at: 'jsonrpc' put: '2.0'.
	id ifNotNil: [ json at: 'id' put: id ].
	result := Dictionary new.
	result at: 'protocolVersion' put: protocolVersion.
	agentInfo ifNotNil: [ result at: 'agentInfo' put: agentInfo asJSON ].
	agentCapabilities ifNotNil: [
		result at: 'agentCapabilities' put: agentCapabilities asJSON ].
	result at: 'authenticationMethods' put: self authenticationMethods.
	json at: 'result' put: result.
	^ json
]

{ #category : 'accessing' }
ACPInitializeResponse >> authenticationMethods [

	^ authenticationMethods ifNil: [ #() ]
]

{ #category : 'accessing' }
ACPInitializeResponse >> authenticationMethods: anArray [

	authenticationMethods := anArray
]

{ #category : 'testing' }
ACPInitializeResponse >> isResponse [

	^ true
]

{ #category : 'accessing' }
ACPInitializeResponse >> protocolVersion [

	^ protocolVersion
]

{ #category : 'accessing' }
ACPInitializeResponse >> protocolVersion: anInteger [

	protocolVersion := anInteger
]
