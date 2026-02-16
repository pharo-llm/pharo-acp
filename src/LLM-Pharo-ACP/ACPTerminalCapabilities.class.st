Class {
	#name : 'ACPTerminalCapabilities',
	#superclass : 'Object',
	#instVars : [
		'create',
		'output',
		'waitForExit',
		'kill',
		'release'
	],
	#category : 'LLM-Pharo-ACP-Capabilities',
	#package : 'LLM-Pharo-ACP',
	#tag : 'Capabilities'
}

{ #category : 'accessing' }
ACPTerminalCapabilities >> create [

	^ create ifNil: [ false ]
]

{ #category : 'accessing' }
ACPTerminalCapabilities >> create: aBoolean [

	create := aBoolean
]

{ #category : 'accessing' }
ACPTerminalCapabilities >> output [

	^ output ifNil: [ false ]
]

{ #category : 'accessing' }
ACPTerminalCapabilities >> output: aBoolean [

	output := aBoolean
]

{ #category : 'accessing' }
ACPTerminalCapabilities >> waitForExit [

	^ waitForExit ifNil: [ false ]
]

{ #category : 'accessing' }
ACPTerminalCapabilities >> waitForExit: aBoolean [

	waitForExit := aBoolean
]

{ #category : 'accessing' }
ACPTerminalCapabilities >> kill [

	^ kill ifNil: [ false ]
]

{ #category : 'accessing' }
ACPTerminalCapabilities >> kill: aBoolean [

	kill := aBoolean
]

{ #category : 'accessing' }
ACPTerminalCapabilities >> release [

	^ release ifNil: [ false ]
]

{ #category : 'accessing' }
ACPTerminalCapabilities >> release: aBoolean [

	release := aBoolean
]

{ #category : 'serialization' }
ACPTerminalCapabilities >> asJSON [

	| json |
	json := Dictionary new.
	json at: 'create' put: self create.
	json at: 'output' put: self output.
	json at: 'waitForExit' put: self waitForExit.
	json at: 'kill' put: self kill.
	json at: 'release' put: self release.
	^ json
]
