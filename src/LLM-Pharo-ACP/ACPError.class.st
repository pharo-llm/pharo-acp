Class {
	#name : 'ACPError',
	#superclass : 'Error',
	#instVars : [
		'code',
		'errorMessage',
		'data',
		'id'
	],
	#category : 'LLM-Pharo-ACP-Errors',
	#package : 'LLM-Pharo-ACP',
	#tag : 'Errors'
}

{ #category : 'serialization' }
ACPError >> asJSON [

	| json errorDict |
	json := Dictionary new.
	json at: 'jsonrpc' put: '2.0'.
	id ifNotNil: [ json at: 'id' put: id ].
	errorDict := Dictionary new.
	errorDict at: 'code' put: code.
	errorDict at: 'message' put: errorMessage.
	data ifNotNil: [ errorDict at: 'data' put: data ].
	json at: 'error' put: errorDict.
	^ json
]

{ #category : 'accessing' }
ACPError >> code [

	^ code
]

{ #category : 'accessing' }
ACPError >> code: anInteger [

	code := anInteger
]

{ #category : 'accessing' }
ACPError >> data [

	^ data
]

{ #category : 'accessing' }
ACPError >> data: aDictionary [

	data := aDictionary
]

{ #category : 'accessing' }
ACPError >> id [

	^ id
]

{ #category : 'accessing' }
ACPError >> id: anObject [

	id := anObject
]

{ #category : 'testing' }
ACPError >> isError [

	^ true
]

{ #category : 'accessing' }
ACPError >> message [

	^ errorMessage ifNil: [ super messageText ]
]

{ #category : 'accessing' }
ACPError >> message: aString [

	errorMessage := aString.
	self messageText: aString
]
