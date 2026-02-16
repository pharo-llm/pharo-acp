Class {
	#name : 'ACPResource',
	#superclass : 'Object',
	#instVars : [
		'uri',
		'text',
		'blob',
		'mimeType'
	],
	#category : 'LLM-Pharo-ACP-Content',
	#package : 'LLM-Pharo-ACP',
	#tag : 'Content'
}

{ #category : 'accessing' }
ACPResource >> uri [

	^ uri
]

{ #category : 'accessing' }
ACPResource >> uri: aString [

	uri := aString
]

{ #category : 'accessing' }
ACPResource >> text [

	^ text
]

{ #category : 'accessing' }
ACPResource >> text: aString [

	text := aString
]

{ #category : 'accessing' }
ACPResource >> blob [

	^ blob
]

{ #category : 'accessing' }
ACPResource >> blob: aString [

	blob := aString
]

{ #category : 'accessing' }
ACPResource >> mimeType [

	^ mimeType
]

{ #category : 'accessing' }
ACPResource >> mimeType: aString [

	mimeType := aString
]

{ #category : 'serialization' }
ACPResource >> asJSON [

	| json |
	json := Dictionary new.
	json at: 'uri' put: uri.
	text ifNotNil: [ json at: 'text' put: text ].
	blob ifNotNil: [ json at: 'blob' put: blob ].
	mimeType ifNotNil: [ json at: 'mimeType' put: mimeType ].
	^ json
]
