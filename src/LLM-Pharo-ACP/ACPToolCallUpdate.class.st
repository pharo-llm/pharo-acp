Class {
	#name : 'ACPToolCallUpdate',
	#superclass : 'Object',
	#instVars : [
		'toolCallId',
		'status',
		'content'
	],
	#category : 'LLM-Pharo-ACP-ToolCalls',
	#package : 'LLM-Pharo-ACP',
	#tag : 'ToolCalls'
}

{ #category : 'accessing' }
ACPToolCallUpdate >> toolCallId [

	^ toolCallId
]

{ #category : 'accessing' }
ACPToolCallUpdate >> toolCallId: aString [

	toolCallId := aString
]

{ #category : 'accessing' }
ACPToolCallUpdate >> status [

	^ status
]

{ #category : 'accessing' }
ACPToolCallUpdate >> status: aSymbol [

	status := aSymbol
]

{ #category : 'accessing' }
ACPToolCallUpdate >> content [

	^ content
]

{ #category : 'accessing' }
ACPToolCallUpdate >> content: anArray [

	content := anArray
]

{ #category : 'accessing' }
ACPToolCallUpdate >> type [

	^ 'tool_call_update'
]

{ #category : 'serialization' }
ACPToolCallUpdate >> asJSON [

	| json |
	json := Dictionary new.
	json at: 'type' put: 'tool_call_update'.
	json at: 'toolCallId' put: toolCallId.
	json at: 'status' put: status asString.
	content ifNotNil: [
		json at: 'content' put: (content collect: [ :c | c asJSON ]) ].
	^ json
]
