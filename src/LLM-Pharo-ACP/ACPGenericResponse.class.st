Class {
	#name : 'ACPGenericResponse',
	#superclass : 'ACPMessage',
	#instVars : [
		'result'
	],
	#category : 'LLM-Pharo-ACP-Messages',
	#package : 'LLM-Pharo-ACP',
	#tag : 'Messages'
}

{ #category : 'serialization' }
ACPGenericResponse >> asJSON [

	| json |
	json := Dictionary new.
	json at: 'jsonrpc' put: '2.0'.
	id ifNotNil: [ json at: 'id' put: id ].
	json at: 'result' put: result.
	^ json
]

{ #category : 'accessing' }
ACPGenericResponse >> doesNotUnderstand: aMessage [
	"Delegate unknown messages to the result dictionary"

	(result isDictionary and: [ aMessage arguments isEmpty ]) ifTrue: [
		| key |
		key := aMessage selector asString.
		result at: key ifPresent: [ :value | ^ value ] ].
	^ super doesNotUnderstand: aMessage
]

{ #category : 'testing' }
ACPGenericResponse >> isResponse [

	^ true
]

{ #category : 'accessing' }
ACPGenericResponse >> result [

	^ result
]

{ #category : 'accessing' }
ACPGenericResponse >> result: aDictionary [

	result := aDictionary
]
