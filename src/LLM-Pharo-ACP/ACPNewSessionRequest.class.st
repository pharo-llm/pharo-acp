Class {
	#name : 'ACPNewSessionRequest',
	#superclass : 'ACPMessage',
	#instVars : [
		'cwd',
		'mcpServers'
	],
	#category : 'LLM-Pharo-ACP-Messages',
	#package : 'LLM-Pharo-ACP',
	#tag : 'Messages'
}

{ #category : 'accessing' }
ACPNewSessionRequest >> method [

	^ 'session/new'
]

{ #category : 'accessing' }
ACPNewSessionRequest >> cwd [

	^ cwd
]

{ #category : 'accessing' }
ACPNewSessionRequest >> cwd: aString [

	cwd := aString
]

{ #category : 'accessing' }
ACPNewSessionRequest >> mcpServers [

	^ mcpServers ifNil: [ #() ]
]

{ #category : 'accessing' }
ACPNewSessionRequest >> mcpServers: anArray [

	mcpServers := anArray
]

{ #category : 'testing' }
ACPNewSessionRequest >> isRequest [

	^ true
]

{ #category : 'serialization' }
ACPNewSessionRequest >> asJSON [

	| json params |
	json := Dictionary new.
	json at: 'jsonrpc' put: '2.0'.
	id ifNotNil: [ json at: 'id' put: id ].
	json at: 'method' put: self method.
	params := Dictionary new.
	params at: 'cwd' put: cwd.
	params at: 'mcpServers' put: self mcpServers.
	json at: 'params' put: params.
	^ json
]
