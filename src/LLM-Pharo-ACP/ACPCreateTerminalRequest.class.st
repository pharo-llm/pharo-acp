Class {
	#name : 'ACPCreateTerminalRequest',
	#superclass : 'ACPMessage',
	#instVars : [
		'sessionId',
		'command',
		'args',
		'cwd',
		'env'
	],
	#category : 'LLM-Pharo-ACP-Messages',
	#package : 'LLM-Pharo-ACP',
	#tag : 'Messages'
}

{ #category : 'accessing' }
ACPCreateTerminalRequest >> args [

	^ args ifNil: [ #() ]
]

{ #category : 'accessing' }
ACPCreateTerminalRequest >> args: anArray [

	args := anArray
]

{ #category : 'serialization' }
ACPCreateTerminalRequest >> asJSON [

	| json params |
	json := Dictionary new.
	json at: 'jsonrpc' put: '2.0'.
	id ifNotNil: [ json at: 'id' put: id ].
	json at: 'method' put: self method.
	params := Dictionary new.
	params at: 'sessionId' put: sessionId.
	params at: 'command' put: command.
	params at: 'args' put: self args.
	cwd ifNotNil: [ params at: 'cwd' put: cwd ].
	env ifNotNil: [ params at: 'env' put: env ].
	json at: 'params' put: params.
	^ json
]

{ #category : 'accessing' }
ACPCreateTerminalRequest >> command [

	^ command
]

{ #category : 'accessing' }
ACPCreateTerminalRequest >> command: aString [

	command := aString
]

{ #category : 'accessing' }
ACPCreateTerminalRequest >> cwd [

	^ cwd
]

{ #category : 'accessing' }
ACPCreateTerminalRequest >> cwd: aString [

	cwd := aString
]

{ #category : 'accessing' }
ACPCreateTerminalRequest >> env [

	^ env
]

{ #category : 'accessing' }
ACPCreateTerminalRequest >> env: aDictionary [

	env := aDictionary
]

{ #category : 'testing' }
ACPCreateTerminalRequest >> isRequest [

	^ true
]

{ #category : 'accessing' }
ACPCreateTerminalRequest >> method [

	^ 'terminal/create'
]

{ #category : 'accessing' }
ACPCreateTerminalRequest >> sessionId [

	^ sessionId
]

{ #category : 'accessing' }
ACPCreateTerminalRequest >> sessionId: aString [

	sessionId := aString
]
