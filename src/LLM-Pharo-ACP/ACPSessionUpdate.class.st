Class {
	#name : 'ACPSessionUpdate',
	#superclass : 'ACPMessage',
	#instVars : [
		'sessionId',
		'update'
	],
	#category : 'LLM-Pharo-ACP-Messages',
	#package : 'LLM-Pharo-ACP',
	#tag : 'Messages'
}

{ #category : 'accessing' }
ACPSessionUpdate >> method [

	^ 'session/update'
]

{ #category : 'accessing' }
ACPSessionUpdate >> sessionId [

	^ sessionId
]

{ #category : 'accessing' }
ACPSessionUpdate >> sessionId: aString [

	sessionId := aString
]

{ #category : 'accessing' }
ACPSessionUpdate >> update [

	^ update
]

{ #category : 'accessing' }
ACPSessionUpdate >> update: aDictionary [

	update := aDictionary
]

{ #category : 'accessing' }
ACPSessionUpdate >> type [
	"Return the update type from the update dictionary"

	update ifNil: [ ^ nil ].
	update isDictionary ifTrue: [ ^ update at: 'type' ifAbsent: [ nil ] ].
	^ nil
]

{ #category : 'accessing' }
ACPSessionUpdate >> kind [
	"Return the kind from the update dictionary"

	update ifNil: [ ^ nil ].
	update isDictionary ifTrue: [
		^ (update at: 'kind' ifAbsent: [ nil ]) ifNotNil: [ :k |
			  k asSymbol ] ].
	^ nil
]

{ #category : 'accessing' }
ACPSessionUpdate >> modeId [
	"Return the mode ID from the update dictionary"

	update ifNil: [ ^ nil ].
	update isDictionary ifTrue: [
		^ update at: 'modeId' ifAbsent: [ nil ] ].
	^ nil
]

{ #category : 'testing' }
ACPSessionUpdate >> isNotification [

	^ true
]

{ #category : 'serialization' }
ACPSessionUpdate >> asJSON [

	| json params |
	json := Dictionary new.
	json at: 'jsonrpc' put: '2.0'.
	json at: 'method' put: self method.
	params := Dictionary new.
	params at: 'sessionId' put: sessionId.
	params at: 'update' put: update.
	json at: 'params' put: params.
	^ json
]
