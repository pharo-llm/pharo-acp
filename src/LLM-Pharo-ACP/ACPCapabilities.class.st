Class {
	#name : 'ACPCapabilities',
	#superclass : 'Object',
	#instVars : [
		'clientCapabilities',
		'agentCapabilities'
	],
	#category : 'LLM-Pharo-ACP-Capabilities',
	#package : 'LLM-Pharo-ACP',
	#tag : 'Capabilities'
}

{ #category : 'negotiation' }
ACPCapabilities class >> negotiate: clientCaps with: agentCaps [
	"Negotiate capabilities between client and agent"

	^ self new
		  clientCapabilities: clientCaps;
		  agentCapabilities: agentCaps;
		  yourself
]

{ #category : 'accessing' }
ACPCapabilities >> agentCapabilities [

	^ agentCapabilities
]

{ #category : 'accessing' }
ACPCapabilities >> agentCapabilities: anACPAgentCapabilities [

	agentCapabilities := anACPAgentCapabilities
]

{ #category : 'testing' }
ACPCapabilities >> canCreateTerminal [

	^ clientCapabilities notNil and: [
		  clientCapabilities canCreateTerminal ]
]

{ #category : 'testing' }
ACPCapabilities >> canReadFiles [

	^ clientCapabilities notNil and: [ clientCapabilities canReadFiles ]
]

{ #category : 'testing' }
ACPCapabilities >> canWriteFiles [

	^ clientCapabilities notNil and: [ clientCapabilities canWriteFiles ]
]

{ #category : 'accessing' }
ACPCapabilities >> clientCapabilities [

	^ clientCapabilities
]

{ #category : 'accessing' }
ACPCapabilities >> clientCapabilities: anACPClientCapabilities [

	clientCapabilities := anACPClientCapabilities
]

{ #category : 'accessing' }
ACPCapabilities >> fs [

	^ clientCapabilities ifNotNil: [ clientCapabilities fs ]
]

{ #category : 'testing' }
ACPCapabilities >> supportsEmbeddedContext [

	^ agentCapabilities notNil and: [
		  agentCapabilities supportsEmbeddedContext ]
]

{ #category : 'accessing' }
ACPCapabilities >> terminal [

	^ clientCapabilities ifNotNil: [ clientCapabilities terminal ]
]
