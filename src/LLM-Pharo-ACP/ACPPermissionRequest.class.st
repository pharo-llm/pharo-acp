Class {
	#name : 'ACPPermissionRequest',
	#superclass : 'ACPMessage',
	#instVars : [
		'sessionId',
		'title',
		'description',
		'options'
	],
	#category : 'LLM-Pharo-ACP-Messages',
	#package : 'LLM-Pharo-ACP',
	#tag : 'Messages'
}

{ #category : 'accessing' }
ACPPermissionRequest >> method [

	^ 'session/request_permission'
]

{ #category : 'accessing' }
ACPPermissionRequest >> sessionId [

	^ sessionId
]

{ #category : 'accessing' }
ACPPermissionRequest >> sessionId: aString [

	sessionId := aString
]

{ #category : 'accessing' }
ACPPermissionRequest >> title [

	^ title
]

{ #category : 'accessing' }
ACPPermissionRequest >> title: aString [

	title := aString
]

{ #category : 'accessing' }
ACPPermissionRequest >> description [

	^ description
]

{ #category : 'accessing' }
ACPPermissionRequest >> description: aString [

	description := aString
]

{ #category : 'accessing' }
ACPPermissionRequest >> options [

	^ options
]

{ #category : 'accessing' }
ACPPermissionRequest >> options: anArray [

	options := anArray
]

{ #category : 'testing' }
ACPPermissionRequest >> isRequest [

	^ true
]

{ #category : 'serialization' }
ACPPermissionRequest >> asJSON [

	| json params |
	json := Dictionary new.
	json at: 'jsonrpc' put: '2.0'.
	id ifNotNil: [ json at: 'id' put: id ].
	json at: 'method' put: self method.
	params := Dictionary new.
	params at: 'sessionId' put: sessionId.
	params at: 'title' put: title.
	params at: 'description' put: description.
	options ifNotNil: [
		params at: 'options' put: (options collect: [ :o | o asJSON ]) ].
	json at: 'params' put: params.
	^ json
]
