Class {
	#name : 'ACPTransport',
	#superclass : 'Object',
	#instVars : [
		'closed'
	],
	#category : 'LLM-Pharo-ACP-Transport',
	#package : 'LLM-Pharo-ACP',
	#tag : 'Transport'
}

{ #category : 'initialization' }
ACPTransport >> initialize [

	super initialize.
	closed := false
]

{ #category : 'operations' }
ACPTransport >> send: aMessage [
	"Send a message through the transport"

	self subclassResponsibility
]

{ #category : 'operations' }
ACPTransport >> close [

	closed := true
]

{ #category : 'testing' }
ACPTransport >> isClosed [

	^ closed ifNil: [ false ]
]
