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

{ #category : 'operations' }
ACPTransport >> close [

	closed := true
]

{ #category : 'initialization' }
ACPTransport >> initialize [

	super initialize.
	closed := false
]

{ #category : 'testing' }
ACPTransport >> isClosed [

	^ closed ifNil: [ false ]
]

{ #category : 'operations' }
ACPTransport >> send: aMessage [
	"Send a message through the transport"

	self subclassResponsibility
]
