Class {
	#name : 'ACPSetModeRequest',
	#superclass : 'ACPMessage',
	#instVars : [
		'sessionId',
		'modeId'
	],
	#category : 'LLM-Pharo-ACP-Messages',
	#package : 'LLM-Pharo-ACP',
	#tag : 'Messages'
}

{ #category : 'serialization' }
ACPSetModeRequest >> asJSON [

	| json params |
	json := Dictionary new.
	json at: 'jsonrpc' put: '2.0'.
	json at: 'method' put: self method.
	params := Dictionary new.
	params at: 'sessionId' put: sessionId.
	params at: 'modeId' put: modeId.
	json at: 'params' put: params.
	^ json
]

{ #category : 'testing' }
ACPSetModeRequest >> isNotification [

	^ true
]

{ #category : 'accessing' }
ACPSetModeRequest >> method [

	^ 'session/set_mode'
]

{ #category : 'accessing' }
ACPSetModeRequest >> modeId [

	^ modeId
]

{ #category : 'accessing' }
ACPSetModeRequest >> modeId: aString [

	modeId := aString
]

{ #category : 'accessing' }
ACPSetModeRequest >> sessionId [

	^ sessionId
]

{ #category : 'accessing' }
ACPSetModeRequest >> sessionId: aString [

	sessionId := aString
]
