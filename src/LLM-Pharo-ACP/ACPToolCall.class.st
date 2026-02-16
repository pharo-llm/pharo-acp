Class {
	#name : 'ACPToolCall',
	#superclass : 'Object',
	#instVars : [
		'toolCallId',
		'title',
		'kind',
		'status',
		'location',
		'content'
	],
	#classVars : [
		'ValidKinds',
		'ValidStatuses'
	],
	#category : 'LLM-Pharo-ACP-ToolCalls',
	#package : 'LLM-Pharo-ACP',
	#tag : 'ToolCalls'
}

{ #category : 'class initialization' }
ACPToolCall class >> initialize [

	ValidKinds := #( #read #edit #delete #move #search #execute #think #fetch #other ).
	ValidStatuses := #( #pending #in_progress #completed )
]

{ #category : 'accessing' }
ACPToolCall class >> validKinds [

	^ ValidKinds ifNil: [
		  ValidKinds := #( #read #edit #delete #move #search #execute #think #fetch #other ) ]
]

{ #category : 'accessing' }
ACPToolCall class >> validStatuses [

	^ ValidStatuses ifNil: [
		  ValidStatuses := #( #pending #in_progress #completed ) ]
]

{ #category : 'accessing' }
ACPToolCall >> toolCallId [

	^ toolCallId
]

{ #category : 'accessing' }
ACPToolCall >> toolCallId: aString [

	toolCallId := aString
]

{ #category : 'accessing' }
ACPToolCall >> title [

	^ title
]

{ #category : 'accessing' }
ACPToolCall >> title: aString [

	title := aString
]

{ #category : 'accessing' }
ACPToolCall >> kind [

	^ kind
]

{ #category : 'accessing' }
ACPToolCall >> kind: aSymbol [

	(self class validKinds includes: aSymbol) ifFalse: [
		ACPValidationError signal: 'Invalid tool call kind: ' , aSymbol asString ].
	kind := aSymbol
]

{ #category : 'accessing' }
ACPToolCall >> status [

	^ status
]

{ #category : 'accessing' }
ACPToolCall >> status: aSymbol [

	(self class validStatuses includes: aSymbol) ifFalse: [
		ACPValidationError signal: 'Invalid tool call status: ' , aSymbol asString ].
	status := aSymbol
]

{ #category : 'accessing' }
ACPToolCall >> location [

	^ location
]

{ #category : 'accessing' }
ACPToolCall >> location: anACPLocation [

	location := anACPLocation
]

{ #category : 'accessing' }
ACPToolCall >> content [

	^ content
]

{ #category : 'accessing' }
ACPToolCall >> content: anArray [

	content := anArray
]

{ #category : 'accessing' }
ACPToolCall >> type [

	^ 'tool_call'
]

{ #category : 'validation' }
ACPToolCall >> yourself [

	toolCallId ifNil: [ ACPValidationError signal: 'Tool call ID is required' ].
	title ifNil: [ ACPValidationError signal: 'Tool call title is required' ].
	kind ifNil: [ ACPValidationError signal: 'Tool call kind is required' ].
	^ self
]

{ #category : 'serialization' }
ACPToolCall >> asJSON [

	| json |
	json := Dictionary new.
	json at: 'type' put: 'tool_call'.
	json at: 'toolCallId' put: toolCallId.
	json at: 'title' put: title.
	json at: 'kind' put: kind asString.
	json at: 'status' put: status asString.
	location ifNotNil: [ json at: 'location' put: location asJSON ].
	content ifNotNil: [
		json at: 'content' put: (content collect: [ :c | c asJSON ]) ].
	^ json
]
