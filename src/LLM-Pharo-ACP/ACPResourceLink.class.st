Class {
	#name : 'ACPResourceLink',
	#superclass : 'ACPContentBlock',
	#instVars : [
		'uri',
		'name',
		'mimeType',
		'size',
		'description'
	],
	#category : 'LLM-Pharo-ACP-Content',
	#package : 'LLM-Pharo-ACP',
	#tag : 'Content'
}

{ #category : 'serialization' }
ACPResourceLink >> asJSON [

	| json |
	json := Dictionary new.
	json at: 'type' put: 'resource_link'.
	json at: 'uri' put: uri.
	name ifNotNil: [ json at: 'name' put: name ].
	mimeType ifNotNil: [ json at: 'mimeType' put: mimeType ].
	size ifNotNil: [ json at: 'size' put: size ].
	description ifNotNil: [ json at: 'description' put: description ].
	^ json
]

{ #category : 'accessing' }
ACPResourceLink >> description [

	^ description
]

{ #category : 'accessing' }
ACPResourceLink >> description: aString [

	description := aString
]

{ #category : 'accessing' }
ACPResourceLink >> mimeType [

	^ mimeType
]

{ #category : 'accessing' }
ACPResourceLink >> mimeType: aString [

	mimeType := aString
]

{ #category : 'accessing' }
ACPResourceLink >> name [

	^ name
]

{ #category : 'accessing' }
ACPResourceLink >> name: aString [

	name := aString
]

{ #category : 'accessing' }
ACPResourceLink >> size [

	^ size
]

{ #category : 'accessing' }
ACPResourceLink >> size: anInteger [

	size := anInteger
]

{ #category : 'accessing' }
ACPResourceLink >> type [

	^ 'resource_link'
]

{ #category : 'accessing' }
ACPResourceLink >> uri [

	^ uri
]

{ #category : 'accessing' }
ACPResourceLink >> uri: aString [

	aString ifNil: [ ACPValidationError signal: 'Resource link URI cannot be nil' ].
	uri := aString
]
