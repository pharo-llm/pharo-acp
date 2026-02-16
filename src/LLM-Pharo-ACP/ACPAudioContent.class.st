Class {
	#name : 'ACPAudioContent',
	#superclass : 'ACPContentBlock',
	#instVars : [
		'mimeType',
		'data'
	],
	#category : 'LLM-Pharo-ACP-Content',
	#package : 'LLM-Pharo-ACP',
	#tag : 'Content'
}

{ #category : 'serialization' }
ACPAudioContent >> asJSON [

	| json |
	json := Dictionary new.
	json at: 'type' put: 'audio'.
	json at: 'mimeType' put: mimeType.
	json at: 'data' put: data.
	^ json
]

{ #category : 'accessing' }
ACPAudioContent >> data [

	^ data
]

{ #category : 'accessing' }
ACPAudioContent >> data: aString [

	data := aString
]

{ #category : 'accessing' }
ACPAudioContent >> mimeType [

	^ mimeType
]

{ #category : 'accessing' }
ACPAudioContent >> mimeType: aString [

	mimeType := aString
]

{ #category : 'accessing' }
ACPAudioContent >> type [

	^ 'audio'
]
