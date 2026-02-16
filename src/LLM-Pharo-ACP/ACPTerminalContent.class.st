Class {
	#name : 'ACPTerminalContent',
	#superclass : 'ACPContentBlock',
	#instVars : [
		'terminalId',
		'command',
		'exitCode'
	],
	#category : 'LLM-Pharo-ACP-Content',
	#package : 'LLM-Pharo-ACP',
	#tag : 'Content'
}

{ #category : 'serialization' }
ACPTerminalContent >> asJSON [

	| json |
	json := Dictionary new.
	json at: 'type' put: 'terminal'.
	json at: 'terminalId' put: terminalId.
	json at: 'command' put: command.
	json at: 'exitCode' put: exitCode.
	^ json
]

{ #category : 'accessing' }
ACPTerminalContent >> command [

	^ command
]

{ #category : 'accessing' }
ACPTerminalContent >> command: aString [

	command := aString
]

{ #category : 'accessing' }
ACPTerminalContent >> exitCode [

	^ exitCode
]

{ #category : 'accessing' }
ACPTerminalContent >> exitCode: anInteger [

	exitCode := anInteger
]

{ #category : 'accessing' }
ACPTerminalContent >> terminalId [

	^ terminalId
]

{ #category : 'accessing' }
ACPTerminalContent >> terminalId: aString [

	terminalId := aString
]

{ #category : 'accessing' }
ACPTerminalContent >> type [

	^ 'terminal'
]
