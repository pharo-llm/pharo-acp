Class {
	#name : 'ACPAgentCapabilities',
	#superclass : 'Object',
	#instVars : [
		'loadSession',
		'promptCapabilities',
		'mcpCapabilities',
		'sessionModes'
	],
	#category : 'LLM-Pharo-ACP-Capabilities',
	#package : 'LLM-Pharo-ACP',
	#tag : 'Capabilities'
}

{ #category : 'accessing' }
ACPAgentCapabilities >> loadSession [

	^ loadSession ifNil: [ false ]
]

{ #category : 'accessing' }
ACPAgentCapabilities >> loadSession: aBoolean [

	loadSession := aBoolean
]

{ #category : 'accessing' }
ACPAgentCapabilities >> promptCapabilities [

	^ promptCapabilities
]

{ #category : 'accessing' }
ACPAgentCapabilities >> promptCapabilities: anACPPromptCapabilities [

	promptCapabilities := anACPPromptCapabilities
]

{ #category : 'accessing' }
ACPAgentCapabilities >> mcpCapabilities [

	^ mcpCapabilities
]

{ #category : 'accessing' }
ACPAgentCapabilities >> mcpCapabilities: anACPMCPCapabilities [

	mcpCapabilities := anACPMCPCapabilities
]

{ #category : 'accessing' }
ACPAgentCapabilities >> sessionModes [

	^ sessionModes ifNil: [ #() ]
]

{ #category : 'accessing' }
ACPAgentCapabilities >> sessionModes: anArray [

	sessionModes := anArray
]

{ #category : 'testing' }
ACPAgentCapabilities >> canLoadSession [

	^ self loadSession
]

{ #category : 'testing' }
ACPAgentCapabilities >> supportsImagePrompts [

	^ promptCapabilities notNil and: [ promptCapabilities supportsImage ]
]

{ #category : 'testing' }
ACPAgentCapabilities >> supportsEmbeddedContext [

	^ promptCapabilities notNil and: [
		  promptCapabilities supportsEmbeddedContext ]
]

{ #category : 'testing' }
ACPAgentCapabilities >> supportsMode: aModeId [

	^ self sessionModes anySatisfy: [ :mode | mode modeId = aModeId ]
]

{ #category : 'serialization' }
ACPAgentCapabilities >> asJSON [

	| json |
	json := Dictionary new.
	json at: 'loadSession' put: self loadSession.
	promptCapabilities ifNotNil: [
		json at: 'promptCapabilities' put: promptCapabilities asJSON ].
	mcpCapabilities ifNotNil: [
		json at: 'mcpCapabilities' put: mcpCapabilities asJSON ].
	self sessionModes ifNotEmpty: [ :modes |
		json
			at: 'sessionModes'
			put: (modes collect: [ :m | m asJSON ]) ].
	^ json
]

{ #category : 'instance creation' }
ACPAgentCapabilities class >> fromDictionary: aDictionary [

	| caps |
	caps := self new.
	aDictionary at: 'loadSession' ifPresent: [ :v | caps loadSession: v ].
	^ caps
]
