Class {
	#name : 'ACPMCPCapabilities',
	#superclass : 'Object',
	#instVars : [
		'supportedTransports'
	],
	#category : 'LLM-Pharo-ACP-Capabilities',
	#package : 'LLM-Pharo-ACP',
	#tag : 'Capabilities'
}

{ #category : 'accessing' }
ACPMCPCapabilities >> supportedTransports [

	^ supportedTransports
]

{ #category : 'accessing' }
ACPMCPCapabilities >> supportedTransports: anArray [

	(anArray isNil or: [ anArray isEmpty ]) ifTrue: [
		ACPValidationError signal: 'At least one transport must be supported' ].
	supportedTransports := anArray
]

{ #category : 'testing' }
ACPMCPCapabilities >> supports: aTransportString [

	^ supportedTransports includes: aTransportString
]

{ #category : 'serialization' }
ACPMCPCapabilities >> asJSON [

	| json |
	json := Dictionary new.
	json at: 'supportedTransports' put: supportedTransports asArray.
	^ json
]
