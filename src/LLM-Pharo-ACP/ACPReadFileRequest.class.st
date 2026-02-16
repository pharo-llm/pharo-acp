Class {
	#name : 'ACPReadFileRequest',
	#superclass : 'ACPMessage',
	#instVars : [
		'sessionId',
		'path',
		'startLine',
		'lineLimit'
	],
	#category : 'LLM-Pharo-ACP-Messages',
	#package : 'LLM-Pharo-ACP',
	#tag : 'Messages'
}

{ #category : 'accessing' }
ACPReadFileRequest >> method [

	^ 'fs/read_text_file'
]

{ #category : 'accessing' }
ACPReadFileRequest >> sessionId [

	^ sessionId
]

{ #category : 'accessing' }
ACPReadFileRequest >> sessionId: aString [

	sessionId := aString
]

{ #category : 'accessing' }
ACPReadFileRequest >> path [

	^ path
]

{ #category : 'accessing' }
ACPReadFileRequest >> path: aString [

	path := aString
]

{ #category : 'accessing' }
ACPReadFileRequest >> startLine [

	^ startLine
]

{ #category : 'accessing' }
ACPReadFileRequest >> startLine: anInteger [

	startLine := anInteger
]

{ #category : 'accessing' }
ACPReadFileRequest >> lineLimit [

	^ lineLimit
]

{ #category : 'accessing' }
ACPReadFileRequest >> lineLimit: anInteger [

	lineLimit := anInteger
]

{ #category : 'testing' }
ACPReadFileRequest >> isRequest [

	^ true
]

{ #category : 'serialization' }
ACPReadFileRequest >> asJSON [

	| json params |
	json := Dictionary new.
	json at: 'jsonrpc' put: '2.0'.
	id ifNotNil: [ json at: 'id' put: id ].
	json at: 'method' put: self method.
	params := Dictionary new.
	params at: 'sessionId' put: sessionId.
	params at: 'path' put: path.
	startLine ifNotNil: [ params at: 'startLine' put: startLine ].
	lineLimit ifNotNil: [ params at: 'lineLimit' put: lineLimit ].
	json at: 'params' put: params.
	^ json
]
