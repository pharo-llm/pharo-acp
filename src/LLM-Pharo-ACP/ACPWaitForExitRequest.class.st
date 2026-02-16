Class {
	#name : 'ACPWaitForExitRequest',
	#superclass : 'ACPMessage',
	#instVars : [
		'sessionId',
		'terminalId',
		'timeout'
	],
	#category : 'LLM-Pharo-ACP-Messages',
	#package : 'LLM-Pharo-ACP',
	#tag : 'Messages'
}

{ #category : 'accessing' }
ACPWaitForExitRequest >> method [

	^ 'terminal/wait_for_exit'
]

{ #category : 'accessing' }
ACPWaitForExitRequest >> sessionId [

	^ sessionId
]

{ #category : 'accessing' }
ACPWaitForExitRequest >> sessionId: aString [

	sessionId := aString
]

{ #category : 'accessing' }
ACPWaitForExitRequest >> terminalId [

	^ terminalId
]

{ #category : 'accessing' }
ACPWaitForExitRequest >> terminalId: aString [

	terminalId := aString
]

{ #category : 'accessing' }
ACPWaitForExitRequest >> timeout [

	^ timeout
]

{ #category : 'accessing' }
ACPWaitForExitRequest >> timeout: aDuration [

	timeout := aDuration
]

{ #category : 'testing' }
ACPWaitForExitRequest >> isRequest [

	^ true
]

{ #category : 'serialization' }
ACPWaitForExitRequest >> asJSON [

	| json params |
	json := Dictionary new.
	json at: 'jsonrpc' put: '2.0'.
	id ifNotNil: [ json at: 'id' put: id ].
	json at: 'method' put: self method.
	params := Dictionary new.
	params at: 'sessionId' put: sessionId.
	params at: 'terminalId' put: terminalId.
	timeout ifNotNil: [ params at: 'timeout' put: timeout asMilliSeconds ].
	json at: 'params' put: params.
	^ json
]
