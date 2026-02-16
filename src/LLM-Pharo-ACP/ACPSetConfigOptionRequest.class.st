Class {
	#name : 'ACPSetConfigOptionRequest',
	#superclass : 'ACPMessage',
	#instVars : [
		'sessionId',
		'option',
		'value'
	],
	#category : 'LLM-Pharo-ACP-Messages',
	#package : 'LLM-Pharo-ACP',
	#tag : 'Messages'
}

{ #category : 'serialization' }
ACPSetConfigOptionRequest >> asJSON [

	| json params |
	json := Dictionary new.
	json at: 'jsonrpc' put: '2.0'.
	json at: 'method' put: self method.
	params := Dictionary new.
	params at: 'sessionId' put: sessionId.
	params at: 'option' put: option.
	params at: 'value' put: value.
	json at: 'params' put: params.
	^ json
]

{ #category : 'testing' }
ACPSetConfigOptionRequest >> isNotification [

	^ true
]

{ #category : 'accessing' }
ACPSetConfigOptionRequest >> method [

	^ 'session/set_config_option'
]

{ #category : 'accessing' }
ACPSetConfigOptionRequest >> option [

	^ option
]

{ #category : 'accessing' }
ACPSetConfigOptionRequest >> option: aString [

	option := aString
]

{ #category : 'accessing' }
ACPSetConfigOptionRequest >> sessionId [

	^ sessionId
]

{ #category : 'accessing' }
ACPSetConfigOptionRequest >> sessionId: aString [

	sessionId := aString
]

{ #category : 'accessing' }
ACPSetConfigOptionRequest >> value [

	^ value
]

{ #category : 'accessing' }
ACPSetConfigOptionRequest >> value: anObject [

	value := anObject
]
