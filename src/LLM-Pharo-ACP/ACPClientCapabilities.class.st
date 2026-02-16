Class {
	#name : 'ACPClientCapabilities',
	#superclass : 'Object',
	#instVars : [
		'fs',
		'terminal'
	],
	#category : 'LLM-Pharo-ACP-Capabilities',
	#package : 'LLM-Pharo-ACP',
	#tag : 'Capabilities'
}

{ #category : 'accessing' }
ACPClientCapabilities >> fs [

	^ fs
]

{ #category : 'accessing' }
ACPClientCapabilities >> fs: anACPFSCapabilities [

	fs := anACPFSCapabilities
]

{ #category : 'accessing' }
ACPClientCapabilities >> terminal [

	^ terminal
]

{ #category : 'accessing' }
ACPClientCapabilities >> terminal: anACPTerminalCapabilities [

	terminal := anACPTerminalCapabilities
]

{ #category : 'testing' }
ACPClientCapabilities >> canReadFiles [

	^ fs notNil and: [ fs readTextFile ]
]

{ #category : 'testing' }
ACPClientCapabilities >> canWriteFiles [

	^ fs notNil and: [ fs writeTextFile ]
]

{ #category : 'testing' }
ACPClientCapabilities >> canCreateTerminal [

	^ terminal notNil and: [ terminal create ]
]

{ #category : 'serialization' }
ACPClientCapabilities >> asJSON [

	| json |
	json := Dictionary new.
	fs ifNotNil: [ json at: 'fs' put: fs asJSON ].
	terminal ifNotNil: [ json at: 'terminal' put: terminal asJSON ].
	^ json
]
