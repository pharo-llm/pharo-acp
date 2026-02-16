Class {
	#name : 'ACPPromptCapabilities',
	#superclass : 'Object',
	#instVars : [
		'image',
		'audio',
		'embeddedContext'
	],
	#category : 'LLM-Pharo-ACP-Capabilities',
	#package : 'LLM-Pharo-ACP',
	#tag : 'Capabilities'
}

{ #category : 'accessing' }
ACPPromptCapabilities >> image [

	^ image ifNil: [ false ]
]

{ #category : 'accessing' }
ACPPromptCapabilities >> image: aBoolean [

	image := aBoolean
]

{ #category : 'accessing' }
ACPPromptCapabilities >> audio [

	^ audio ifNil: [ false ]
]

{ #category : 'accessing' }
ACPPromptCapabilities >> audio: aBoolean [

	audio := aBoolean
]

{ #category : 'accessing' }
ACPPromptCapabilities >> embeddedContext [

	^ embeddedContext ifNil: [ false ]
]

{ #category : 'accessing' }
ACPPromptCapabilities >> embeddedContext: aBoolean [

	embeddedContext := aBoolean
]

{ #category : 'testing' }
ACPPromptCapabilities >> supportsImage [

	^ self image
]

{ #category : 'testing' }
ACPPromptCapabilities >> supportsAudio [

	^ self audio
]

{ #category : 'testing' }
ACPPromptCapabilities >> supportsEmbeddedContext [

	^ self embeddedContext
]

{ #category : 'serialization' }
ACPPromptCapabilities >> asJSON [

	| json |
	json := Dictionary new.
	json at: 'image' put: self image.
	json at: 'audio' put: self audio.
	json at: 'embeddedContext' put: self embeddedContext.
	^ json
]
