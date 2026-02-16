Class {
	#name : 'ACPGenericRequest',
	#superclass : 'ACPMessage',
	#instVars : [
		'methodName',
		'params'
	],
	#category : 'LLM-Pharo-ACP-Messages',
	#package : 'LLM-Pharo-ACP',
	#tag : 'Messages'
}

{ #category : 'accessing' }
ACPGenericRequest >> method [

	^ methodName
]

{ #category : 'accessing' }
ACPGenericRequest >> method: aString [

	methodName := aString
]

{ #category : 'accessing' }
ACPGenericRequest >> params [

	^ params
]

{ #category : 'accessing' }
ACPGenericRequest >> params: aDictionary [

	params := aDictionary
]

{ #category : 'testing' }
ACPGenericRequest >> isRequest [

	^ true
]

{ #category : 'serialization' }
ACPGenericRequest >> asJSON [

	| json |
	json := Dictionary new.
	json at: 'jsonrpc' put: '2.0'.
	id ifNotNil: [ json at: 'id' put: id ].
	json at: 'method' put: methodName.
	params ifNotNil: [ json at: 'params' put: params ].
	^ json
]
