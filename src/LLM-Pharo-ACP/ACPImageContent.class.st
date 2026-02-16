Class {
	#name : 'ACPImageContent',
	#superclass : 'ACPContentBlock',
	#instVars : [
		'mimeType',
		'data',
		'uri'
	],
	#category : 'LLM-Pharo-ACP-Content',
	#package : 'LLM-Pharo-ACP',
	#tag : 'Content'
}

{ #category : 'serialization' }
ACPImageContent >> asJSON [

	| json |
	json := Dictionary new.
	json at: 'type' put: 'image'.
	json at: 'mimeType' put: mimeType.
	json at: 'data' put: data.
	uri ifNotNil: [ json at: 'uri' put: uri ].
	^ json
]

{ #category : 'accessing' }
ACPImageContent >> data [

	^ data
]

{ #category : 'accessing' }
ACPImageContent >> data: aString [

	data := aString
]

{ #category : 'accessing' }
ACPImageContent >> mimeType [

	^ mimeType
]

{ #category : 'accessing' }
ACPImageContent >> mimeType: aString [

	aString ifNil: [ ACPValidationError signal: 'Image MIME type cannot be nil' ].
	mimeType := aString
]

{ #category : 'accessing' }
ACPImageContent >> type [

	^ 'image'
]

{ #category : 'accessing' }
ACPImageContent >> uri [

	^ uri
]

{ #category : 'accessing' }
ACPImageContent >> uri: aString [

	uri := aString
]
