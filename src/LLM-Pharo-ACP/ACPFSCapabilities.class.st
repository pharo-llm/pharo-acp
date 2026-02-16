Class {
	#name : 'ACPFSCapabilities',
	#superclass : 'Object',
	#instVars : [
		'readTextFile',
		'writeTextFile'
	],
	#category : 'LLM-Pharo-ACP-Capabilities',
	#package : 'LLM-Pharo-ACP',
	#tag : 'Capabilities'
}

{ #category : 'serialization' }
ACPFSCapabilities >> asJSON [

	| json |
	json := Dictionary new.
	json at: 'readTextFile' put: self readTextFile.
	json at: 'writeTextFile' put: self writeTextFile.
	^ json
]

{ #category : 'accessing' }
ACPFSCapabilities >> readTextFile [

	^ readTextFile ifNil: [ false ]
]

{ #category : 'accessing' }
ACPFSCapabilities >> readTextFile: aBoolean [

	readTextFile := aBoolean
]

{ #category : 'accessing' }
ACPFSCapabilities >> writeTextFile [

	^ writeTextFile ifNil: [ false ]
]

{ #category : 'accessing' }
ACPFSCapabilities >> writeTextFile: aBoolean [

	writeTextFile := aBoolean
]
