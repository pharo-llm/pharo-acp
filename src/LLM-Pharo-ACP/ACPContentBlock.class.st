Class {
	#name : 'ACPContentBlock',
	#superclass : 'Object',
	#category : 'LLM-Pharo-ACP-Content',
	#package : 'LLM-Pharo-ACP',
	#tag : 'Content'
}

{ #category : 'instance creation' }
ACPContentBlock class >> fromJSON: aString [
	"Deserialize a content block from a JSON string"

	| dict |
	dict := STONJSON fromString: aString.
	^ self fromDictionary: dict
]

{ #category : 'instance creation' }
ACPContentBlock class >> fromDictionary: aDictionary [
	"Create appropriate content block subclass from a dictionary"

	| type |
	type := aDictionary at: 'type'.
	type = 'text' ifTrue: [
		^ ACPTextContent new
			  text: (aDictionary at: 'text');
			  yourself ].
	type = 'image' ifTrue: [
		^ ACPImageContent new
			  mimeType: (aDictionary at: 'mimeType');
			  data: (aDictionary at: 'data');
			  yourself ].
	type = 'audio' ifTrue: [
		^ ACPAudioContent new
			  mimeType: (aDictionary at: 'mimeType');
			  data: (aDictionary at: 'data');
			  yourself ].
	type = 'resource' ifTrue: [
		| resDict resource |
		resDict := aDictionary at: 'resource'.
		resource := ACPResource new
			            uri: (resDict at: 'uri');
			            yourself.
		resDict at: 'text' ifPresent: [ :t | resource text: t ].
		resDict at: 'blob' ifPresent: [ :b | resource blob: b ].
		resDict at: 'mimeType' ifPresent: [ :m | resource mimeType: m ].
		^ ACPResourceContent new resource: resource ].
	type = 'resource_link' ifTrue: [
		| link |
		link := ACPResourceLink new
			        uri: (aDictionary at: 'uri');
			        yourself.
		aDictionary at: 'name' ifPresent: [ :n | link name: n ].
		aDictionary at: 'mimeType' ifPresent: [ :m | link mimeType: m ].
		aDictionary at: 'size' ifPresent: [ :s | link size: s ].
		aDictionary at: 'description' ifPresent: [ :d | link description: d ].
		^ link ].
	type = 'diff' ifTrue: [
		^ ACPDiffContent new
			  path: (aDictionary at: 'path');
			  oldText: (aDictionary at: 'oldText');
			  newText: (aDictionary at: 'newText');
			  yourself ].
	type = 'terminal' ifTrue: [
		^ ACPTerminalContent new
			  terminalId: (aDictionary at: 'terminalId');
			  command: (aDictionary at: 'command');
			  exitCode: (aDictionary at: 'exitCode');
			  yourself ].
	ACPProtocolError signal: 'Unknown content block type: ' , type
]

{ #category : 'instance creation' }
ACPContentBlock class >> arrayFromJSON: aString [
	"Deserialize an array of content blocks from a JSON string"

	| array |
	array := STONJSON fromString: aString.
	^ array collect: [ :dict | self fromDictionary: dict ]
]

{ #category : 'accessing' }
ACPContentBlock >> type [
	"Subclasses must override"

	^ self subclassResponsibility
]

{ #category : 'serialization' }
ACPContentBlock >> asJSON [
	"Subclasses must override"

	^ self subclassResponsibility
]
