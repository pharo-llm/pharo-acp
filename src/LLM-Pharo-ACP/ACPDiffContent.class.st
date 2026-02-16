Class {
	#name : 'ACPDiffContent',
	#superclass : 'ACPContentBlock',
	#instVars : [
		'path',
		'oldText',
		'newText',
		'mimeType'
	],
	#category : 'LLM-Pharo-ACP-Content',
	#package : 'LLM-Pharo-ACP',
	#tag : 'Content'
}

{ #category : 'accessing' }
ACPDiffContent >> path [

	^ path
]

{ #category : 'accessing' }
ACPDiffContent >> path: aString [

	path := aString
]

{ #category : 'accessing' }
ACPDiffContent >> oldText [

	^ oldText
]

{ #category : 'accessing' }
ACPDiffContent >> oldText: aString [

	oldText := aString
]

{ #category : 'accessing' }
ACPDiffContent >> newText [

	^ newText
]

{ #category : 'accessing' }
ACPDiffContent >> newText: aString [

	newText := aString
]

{ #category : 'accessing' }
ACPDiffContent >> mimeType [

	^ mimeType
]

{ #category : 'accessing' }
ACPDiffContent >> mimeType: aString [

	mimeType := aString
]

{ #category : 'accessing' }
ACPDiffContent >> type [

	^ 'diff'
]

{ #category : 'serialization' }
ACPDiffContent >> asJSON [

	| json |
	json := Dictionary new.
	json at: 'type' put: 'diff'.
	json at: 'path' put: path.
	json at: 'oldText' put: oldText.
	json at: 'newText' put: newText.
	mimeType ifNotNil: [ json at: 'mimeType' put: mimeType ].
	^ json
]
