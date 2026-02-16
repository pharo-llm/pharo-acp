Class {
	#name : 'ACPSessionMode',
	#superclass : 'Object',
	#instVars : [
		'modeId',
		'title',
		'description'
	],
	#category : 'LLM-Pharo-ACP-Capabilities',
	#package : 'LLM-Pharo-ACP',
	#tag : 'Capabilities'
}

{ #category : 'accessing' }
ACPSessionMode >> modeId [

	^ modeId
]

{ #category : 'accessing' }
ACPSessionMode >> modeId: aString [

	modeId := aString
]

{ #category : 'accessing' }
ACPSessionMode >> title [

	^ title
]

{ #category : 'accessing' }
ACPSessionMode >> title: aString [

	title := aString
]

{ #category : 'accessing' }
ACPSessionMode >> description [

	^ description
]

{ #category : 'accessing' }
ACPSessionMode >> description: aString [

	description := aString
]

{ #category : 'serialization' }
ACPSessionMode >> asJSON [

	| json |
	json := Dictionary new.
	json at: 'modeId' put: modeId.
	json at: 'title' put: title.
	description ifNotNil: [ json at: 'description' put: description ].
	^ json
]
