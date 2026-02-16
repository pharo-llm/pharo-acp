Class {
	#name : 'ACPWriteFileRequest',
	#superclass : 'ACPMessage',
	#instVars : [
		'sessionId',
		'path',
		'content'
	],
	#category : 'LLM-Pharo-ACP-Messages',
	#package : 'LLM-Pharo-ACP',
	#tag : 'Messages'
}

{ #category : 'accessing' }
ACPWriteFileRequest >> method [

	^ 'fs/write_text_file'
]

{ #category : 'accessing' }
ACPWriteFileRequest >> sessionId [

	^ sessionId
]

{ #category : 'accessing' }
ACPWriteFileRequest >> sessionId: aString [

	sessionId := aString
]

{ #category : 'accessing' }
ACPWriteFileRequest >> path [

	^ path
]

{ #category : 'accessing' }
ACPWriteFileRequest >> path: aString [

	path := aString
]

{ #category : 'accessing' }
ACPWriteFileRequest >> content [

	^ content
]

{ #category : 'accessing' }
ACPWriteFileRequest >> content: aString [

	content := aString
]

{ #category : 'testing' }
ACPWriteFileRequest >> isRequest [

	^ true
]

{ #category : 'serialization' }
ACPWriteFileRequest >> asJSON [

	| json params |
	json := Dictionary new.
	json at: 'jsonrpc' put: '2.0'.
	id ifNotNil: [ json at: 'id' put: id ].
	json at: 'method' put: self method.
	params := Dictionary new.
	params at: 'sessionId' put: sessionId.
	params at: 'path' put: path.
	params at: 'content' put: content.
	json at: 'params' put: params.
	^ json
]
