Class {
	#name : 'ACPCancelRequest',
	#superclass : 'ACPMessage',
	#instVars : [
		'sessionId'
	],
	#category : 'LLM-Pharo-ACP-Messages',
	#package : 'LLM-Pharo-ACP',
	#tag : 'Messages'
}

{ #category : 'serialization' }
ACPCancelRequest >> asJSON [

	| json params |
	json := Dictionary new.
	json at: 'jsonrpc' put: '2.0'.
	json at: 'method' put: self method.
	params := Dictionary new.
	params at: 'sessionId' put: sessionId.
	json at: 'params' put: params.
	^ json
]

{ #category : 'testing' }
ACPCancelRequest >> isNotification [

	^ true
]

{ #category : 'accessing' }
ACPCancelRequest >> method [

	^ 'session/cancel'
]

{ #category : 'accessing' }
ACPCancelRequest >> sessionId [

	^ sessionId
]

{ #category : 'accessing' }
ACPCancelRequest >> sessionId: aString [

	sessionId := aString
]
