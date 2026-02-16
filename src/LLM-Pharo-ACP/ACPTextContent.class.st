Class {
	#name : 'ACPTextContent',
	#superclass : 'ACPContentBlock',
	#instVars : [
		'text',
		'annotations'
	],
	#category : 'LLM-Pharo-ACP-Content',
	#package : 'LLM-Pharo-ACP',
	#tag : 'Content'
}

{ #category : 'accessing' }
ACPTextContent >> annotations [

	^ annotations
]

{ #category : 'accessing' }
ACPTextContent >> annotations: aDictionary [

	annotations := aDictionary
]

{ #category : 'serialization' }
ACPTextContent >> asJSON [

	| json |
	json := Dictionary new.
	json at: 'type' put: 'text'.
	json at: 'text' put: text.
	annotations ifNotNil: [ json at: 'annotations' put: annotations ].
	^ json
]

{ #category : 'accessing' }
ACPTextContent >> text [

	^ text
]

{ #category : 'accessing' }
ACPTextContent >> text: aString [

	aString ifNil: [ ACPValidationError signal: 'Text content cannot be nil' ].
	text := aString
]

{ #category : 'accessing' }
ACPTextContent >> type [

	^ 'text'
]
