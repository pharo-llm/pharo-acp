Class {
	#name : 'ACPPromptRequest',
	#superclass : 'ACPMessage',
	#instVars : [
		'sessionId',
		'prompt'
	],
	#category : 'LLM-Pharo-ACP-Messages',
	#package : 'LLM-Pharo-ACP',
	#tag : 'Messages'
}

{ #category : 'serialization' }
ACPPromptRequest >> asJSON [

	| json params |
	json := Dictionary new.
	json at: 'jsonrpc' put: '2.0'.
	id ifNotNil: [ json at: 'id' put: id ].
	json at: 'method' put: self method.
	params := Dictionary new.
	params at: 'sessionId' put: sessionId.
	params
		at: 'prompt'
		put: (prompt collect: [ :block |
				 (block isKindOf: ACPContentBlock)
					 ifTrue: [ block asJSON ]
					 ifFalse: [ block ] ]).
	json at: 'params' put: params.
	^ json
]

{ #category : 'testing' }
ACPPromptRequest >> isRequest [

	^ true
]

{ #category : 'accessing' }
ACPPromptRequest >> method [

	^ 'session/prompt'
]

{ #category : 'accessing' }
ACPPromptRequest >> prompt [

	^ prompt
]

{ #category : 'accessing' }
ACPPromptRequest >> prompt: anArray [

	prompt := anArray
]

{ #category : 'accessing' }
ACPPromptRequest >> sessionId [

	^ sessionId
]

{ #category : 'accessing' }
ACPPromptRequest >> sessionId: aString [

	(aString isNil or: [ aString isEmpty ]) ifTrue: [
		ACPValidationError signal: 'Session ID must be a non-empty string' ].
	sessionId := aString
]
