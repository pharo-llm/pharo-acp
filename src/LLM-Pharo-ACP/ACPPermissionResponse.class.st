Class {
	#name : 'ACPPermissionResponse',
	#superclass : 'ACPMessage',
	#instVars : [
		'selectedOptionId'
	],
	#category : 'LLM-Pharo-ACP-Messages',
	#package : 'LLM-Pharo-ACP',
	#tag : 'Messages'
}

{ #category : 'accessing' }
ACPPermissionResponse >> selectedOptionId [

	^ selectedOptionId
]

{ #category : 'accessing' }
ACPPermissionResponse >> selectedOptionId: aString [

	selectedOptionId := aString
]

{ #category : 'testing' }
ACPPermissionResponse >> isResponse [

	^ true
]

{ #category : 'serialization' }
ACPPermissionResponse >> asJSON [

	| json result |
	json := Dictionary new.
	json at: 'jsonrpc' put: '2.0'.
	id ifNotNil: [ json at: 'id' put: id ].
	result := Dictionary new.
	result at: 'selectedOptionId' put: selectedOptionId.
	json at: 'result' put: result.
	^ json
]
