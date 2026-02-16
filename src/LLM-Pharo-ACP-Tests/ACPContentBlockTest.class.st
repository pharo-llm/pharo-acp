Class {
	#name : 'ACPContentBlockTest',
	#superclass : 'TestCase',
	#category : 'LLM-Pharo-ACP-Tests-Content',
	#package : 'LLM-Pharo-ACP-Tests',
	#tag : 'Content'
}

{ #category : 'tests - audio' }
ACPContentBlockTest >> testAudioContentCreation [
	"Test creating audio content block"

	| content |
	content := ACPAudioContent new
		           mimeType: 'audio/wav';
		           data: 'base64audiodata';
		           yourself.

	self assert: content type equals: 'audio'.
	self assert: content mimeType equals: 'audio/wav'
]

{ #category : 'tests - deserialization' }
ACPContentBlockTest >> testContentBlockDeserialization [
	"Test deserializing content blocks from JSON"

	| jsonString content |
	jsonString := '{
		"type": "text",
		"text": "Hello World"
	}'.

	content := ACPContentBlock fromJSON: jsonString.

	self assert: content class equals: ACPTextContent.
	self assert: content text equals: 'Hello World'
]

{ #category : 'tests - validation' }
ACPContentBlockTest >> testContentBlockValidation [
	"Test content block validation"

	self should: [
		ACPTextContent new
			text: nil;
			yourself ] raise: ACPValidationError.

	self should: [
		ACPImageContent new
			mimeType: nil;
			data: 'base64';
			yourself ] raise: ACPValidationError.

	self should: [
		ACPResourceLink new
			uri: nil;
			yourself ] raise: ACPValidationError
]

{ #category : 'tests - diff' }
ACPContentBlockTest >> testDiffContentCreation [
	"Test creating diff content block"

	| diff json |
	diff := ACPDiffContent new
		        path: '/path/to/file.js';
		        oldText: 'const x = 1;';
		        newText: 'const x = 2;';
		        mimeType: 'text/javascript';
		        yourself.

	json := diff asJSON.

	self assert: (json at: 'type') equals: 'diff'.
	self assert: (json at: 'path') equals: '/path/to/file.js'.
	self assert: (json at: 'oldText') equals: 'const x = 1;'.
	self assert: (json at: 'newText') equals: 'const x = 2;'
]

{ #category : 'tests - image' }
ACPContentBlockTest >> testImageContentCreation [
	"Test creating image content block"

	| content |
	content := ACPImageContent new
		           mimeType: 'image/png';
		           data: 'base64encodeddata';
		           yourself.

	self assert: content type equals: 'image'.
	self assert: content mimeType equals: 'image/png'.
	self assert: content data equals: 'base64encodeddata'
]

{ #category : 'tests - image' }
ACPContentBlockTest >> testImageContentWithUri [
	"Test image content with URI"

	| content json |
	content := ACPImageContent new
		           mimeType: 'image/jpeg';
		           data: 'base64data';
		           uri: 'file:///path/to/image.jpg';
		           yourself.

	json := content asJSON.

	self assert: (json at: 'uri') equals: 'file:///path/to/image.jpg'
]

{ #category : 'tests - deserialization' }
ACPContentBlockTest >> testMultipleContentBlockDeserialization [
	"Test deserializing array of content blocks"

	| jsonString blocks |
	jsonString := '[
		{"type": "text", "text": "First"},
		{"type": "text", "text": "Second"}
	]'.

	blocks := ACPContentBlock arrayFromJSON: jsonString.

	self assert: blocks size equals: 2.
	self assert: blocks first text equals: 'First'.
	self assert: blocks second text equals: 'Second'
]

{ #category : 'tests - resource' }
ACPContentBlockTest >> testResourceContentWithBlob [
	"Test embedded resource with binary blob"

	| resource content json |
	resource := ACPResource new
		            uri: 'file:///image.png';
		            blob: 'base64binarydata';
		            mimeType: 'image/png';
		            yourself.

	content := ACPResourceContent new resource: resource.

	json := content asJSON.

	self assert: (json at: 'resource' ifPresent: [ :r | r at: 'blob' ]) equals: 'base64binarydata'.
	self deny: (json at: 'resource' ifPresent: [ :r | r includesKey: 'text' ])
]

{ #category : 'tests - resource' }
ACPContentBlockTest >> testResourceContentWithText [
	"Test embedded resource with text content"

	| resource content json |
	resource := ACPResource new
		            uri: 'file:///test.js';
		            text: 'const x = 1;';
		            mimeType: 'text/javascript';
		            yourself.

	content := ACPResourceContent new resource: resource.

	json := content asJSON.

	self assert: (json at: 'type') equals: 'resource'.
	self assert: (json at: 'resource' ifPresent: [ :r | r at: 'uri' ]) equals: 'file:///test.js'.
	self assert: (json at: 'resource' ifPresent: [ :r | r at: 'text' ]) equals: 'const x = 1;'
]

{ #category : 'tests - resource link' }
ACPContentBlockTest >> testResourceLinkCreation [
	"Test creating resource link"

	| link json |
	link := ACPResourceLink new
		        uri: 'file:///document.pdf';
		        name: 'Documentation';
		        mimeType: 'application/pdf';
		        size: 1024000;
		        description: 'API documentation';
		        yourself.

	json := link asJSON.

	self assert: (json at: 'type') equals: 'resource_link'.
	self assert: (json at: 'uri') equals: 'file:///document.pdf'.
	self assert: (json at: 'name') equals: 'Documentation'.
	self assert: (json at: 'size') equals: 1024000
]

{ #category : 'tests - terminal' }
ACPContentBlockTest >> testTerminalContentCreation [
	"Test creating terminal content block"

	| terminal json |
	terminal := ACPTerminalContent new
		            terminalId: 'term_123';
		            command: 'npm test';
		            exitCode: 0;
		            yourself.

	json := terminal asJSON.

	self assert: (json at: 'type') equals: 'terminal'.
	self assert: (json at: 'terminalId') equals: 'term_123'.
	self assert: (json at: 'command') equals: 'npm test'.
	self assert: (json at: 'exitCode') equals: 0
]

{ #category : 'tests - text' }
ACPContentBlockTest >> testTextContentCreation [
	"Test creating text content block"

	| content |
	content := ACPTextContent new text: 'Hello World'.

	self assert: content type equals: 'text'.
	self assert: content text equals: 'Hello World'
]

{ #category : 'tests - text' }
ACPContentBlockTest >> testTextContentSerialization [
	"Test serializing text content to JSON"

	| content json |
	content := ACPTextContent new text: 'Test message'.

	json := content asJSON.

	self assert: (json at: 'type') equals: 'text'.
	self assert: (json at: 'text') equals: 'Test message'
]

{ #category : 'tests - text' }
ACPContentBlockTest >> testTextContentWithAnnotations [
	"Test text content with annotations"

	| content json |
	content := ACPTextContent new
		           text: 'Important';
		           annotations: (Dictionary new
				            at: 'priority' put: 'high';
				            yourself);
		           yourself.

	json := content asJSON.

	self assert: (json at: 'annotations' ifPresent: [ :a | a at: 'priority' ]) equals: 'high'
]
