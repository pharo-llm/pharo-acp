Class {
	#name : 'ACPClientInfo',
	#superclass : 'Object',
	#instVars : [
		'name',
		'version'
	],
	#category : 'LLM-Pharo-ACP-Core',
	#package : 'LLM-Pharo-ACP',
	#tag : 'Core'
}

{ #category : 'accessing' }
ACPClientInfo >> name [

	^ name
]

{ #category : 'accessing' }
ACPClientInfo >> name: aString [

	name := aString
]

{ #category : 'accessing' }
ACPClientInfo >> version [

	^ version
]

{ #category : 'accessing' }
ACPClientInfo >> version: aString [

	version := aString
]

{ #category : 'serialization' }
ACPClientInfo >> asJSON [

	| json |
	json := Dictionary new.
	json at: 'name' put: name.
	version ifNotNil: [ json at: 'version' put: version ].
	^ json
]
