Class {
	#name : 'ACPLocation',
	#superclass : 'Object',
	#instVars : [
		'path',
		'line'
	],
	#category : 'LLM-Pharo-ACP-ToolCalls',
	#package : 'LLM-Pharo-ACP',
	#tag : 'ToolCalls'
}

{ #category : 'accessing' }
ACPLocation >> path [

	^ path
]

{ #category : 'accessing' }
ACPLocation >> path: aString [

	path := aString
]

{ #category : 'accessing' }
ACPLocation >> line [

	^ line
]

{ #category : 'accessing' }
ACPLocation >> line: anInteger [

	line := anInteger
]

{ #category : 'serialization' }
ACPLocation >> asJSON [

	| json |
	json := Dictionary new.
	json at: 'path' put: path.
	line ifNotNil: [ json at: 'line' put: line ].
	^ json
]
