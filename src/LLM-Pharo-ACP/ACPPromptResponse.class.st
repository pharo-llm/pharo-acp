Class {
	#name : 'ACPPromptResponse',
	#superclass : 'ACPMessage',
	#instVars : [
		'stopReason',
		'content'
	],
	#category : 'LLM-Pharo-ACP-Messages',
	#package : 'LLM-Pharo-ACP',
	#tag : 'Messages'
}

{ #category : 'serialization' }
ACPPromptResponse >> asJSON [

	| json result |
	json := Dictionary new.
	json at: 'jsonrpc' put: '2.0'.
	id ifNotNil: [ json at: 'id' put: id ].
	result := Dictionary new.
	result at: 'stopReason' put: stopReason.
	content ifNotNil: [
		result at: 'content' put: (content collect: [ :c | c asJSON ]) ].
	json at: 'result' put: result.
	^ json
]

{ #category : 'accessing' }
ACPPromptResponse >> content [

	^ content
]

{ #category : 'accessing' }
ACPPromptResponse >> content: anArray [

	content := anArray
]

{ #category : 'testing' }
ACPPromptResponse >> isResponse [

	^ true
]

{ #category : 'accessing' }
ACPPromptResponse >> stopReason [

	^ stopReason
]

{ #category : 'accessing' }
ACPPromptResponse >> stopReason: aString [

	stopReason := aString
]
