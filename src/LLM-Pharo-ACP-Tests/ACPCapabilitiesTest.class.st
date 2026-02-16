Class {
	#name : 'ACPCapabilitiesTest',
	#superclass : 'TestCase',
	#category : 'LLM-Pharo-ACP-Tests-Core',
	#package : 'LLM-Pharo-ACP-Tests',
	#tag : 'Core'
}

{ #category : 'tests - agent capabilities' }
ACPCapabilitiesTest >> testAgentCapabilitiesCreation [
	"Test creating agent capabilities"

	| capabilities |
	capabilities := ACPAgentCapabilities new
		                loadSession: true;
		                promptCapabilities: (ACPPromptCapabilities new
				                 image: true;
				                 audio: false;
				                 embeddedContext: true;
				                 yourself);
		                yourself.

	self assert: capabilities loadSession.
	self assert: capabilities promptCapabilities image.
	self deny: capabilities promptCapabilities audio
]

{ #category : 'tests - capability checking' }
ACPCapabilitiesTest >> testCapabilityChecking [
	"Test helper methods for checking capabilities"

	| clientCaps agentCaps |
	clientCaps := ACPClientCapabilities new
		              fs: (ACPFSCapabilities new
				               readTextFile: true;
				               writeTextFile: false;
				               yourself);
		              yourself.

	agentCaps := ACPAgentCapabilities new
		             loadSession: true;
		             promptCapabilities: (ACPPromptCapabilities new
				              image: false;
				              audio: false;
				              embeddedContext: true;
				              yourself);
		             yourself.

	"Test client capability checks"
	self assert: (clientCaps canReadFiles).
	self deny: (clientCaps canWriteFiles).

	"Test agent capability checks"
	self assert: (agentCaps canLoadSession).
	self deny: (agentCaps supportsImagePrompts).
	self assert: (agentCaps supportsEmbeddedContext)
]

{ #category : 'tests - negotiation' }
ACPCapabilitiesTest >> testCapabilityNegotiation [
	"Test capability negotiation between client and agent"

	| clientCaps agentCaps negotiated |
	"Client supports file operations but not terminal"
	clientCaps := ACPClientCapabilities new
		              fs: (ACPFSCapabilities new
				               readTextFile: true;
				               writeTextFile: true;
				               yourself);
		              yourself.

	"Agent wants embedded context and file operations"
	agentCaps := ACPAgentCapabilities new
		             promptCapabilities: (ACPPromptCapabilities new
				              embeddedContext: true;
				              yourself);
		             yourself.

	"Negotiate common capabilities"
	negotiated := ACPCapabilities negotiate: clientCaps with: agentCaps.

	"Both support file operations"
	self assert: negotiated canReadFiles.
	self assert: negotiated canWriteFiles.

	"Agent supports embedded context"
	self assert: negotiated supportsEmbeddedContext.

	"Client doesn't support terminal"
	self deny: negotiated canCreateTerminal
]

{ #category : 'tests - client capabilities' }
ACPCapabilitiesTest >> testClientCapabilitiesCreation [
	"Test creating client capabilities"

	| capabilities |
	capabilities := ACPClientCapabilities new
		                fs: (ACPFSCapabilities new
				                 readTextFile: true;
				                 writeTextFile: true;
				                 yourself);
		                terminal: (ACPTerminalCapabilities new
				                 create: true;
				                 output: true;
				                 waitForExit: true;
				                 kill: true;
				                 release: true;
				                 yourself);
		                yourself.

	self assert: capabilities fs readTextFile.
	self assert: capabilities fs writeTextFile.
	self assert: capabilities terminal create
]

{ #category : 'tests - client capabilities' }
ACPCapabilitiesTest >> testClientCapabilitiesSerialization [
	"Test serializing client capabilities to JSON"

	| capabilities json |
	capabilities := ACPClientCapabilities new
		                fs: (ACPFSCapabilities new
				                 readTextFile: true;
				                 writeTextFile: false;
				                 yourself);
		                yourself.

	json := capabilities asJSON.

	self assert: (json at: 'fs' ifPresent: [ :fs | fs at: 'readTextFile' ]) equals: true.
	self assert: (json at: 'fs' ifPresent: [ :fs | fs at: 'writeTextFile' ]) equals: false
]

{ #category : 'tests - serialization' }
ACPCapabilitiesTest >> testCompleteCapabilitiesSerialization [
	"Test serializing complete capabilities structure"

	| capabilities json |
	capabilities := ACPAgentCapabilities new
		                loadSession: true;
		                promptCapabilities: (ACPPromptCapabilities new
				                 image: true;
				                 audio: false;
				                 embeddedContext: true;
				                 yourself);
		                mcpCapabilities: (ACPMCPCapabilities new
				                 supportedTransports: #( 'stdio' 'http' );
				                 yourself);
		                sessionModes: {
						                (ACPSessionMode new
							                 modeId: 'code';
							                 title: 'Code Mode';
							                 description: 'Full mode';
							                 yourself) };
		                yourself.

	json := capabilities asJSON.

	self assert: (json at: 'loadSession') equals: true.
	self
		assert: (json at: 'promptCapabilities' ifPresent: [ :pc | pc at: 'image' ])
		equals: true.
self assert:
  ((json at: 'mcpCapabilities' ifPresent: [ :mc |
      mc at: 'supportedTransports' ])
        includes: 'stdio').

	self
		assert: (json at: 'sessionModes' ifPresent: [ :sm | sm size ])
		equals: 1
]

{ #category : 'tests - validation' }
ACPCapabilitiesTest >> testInvalidCapabilityConfiguration [
	"Test that invalid capability configurations are rejected"

	self should: [
		ACPMCPCapabilities new supportedTransports: #(  ) ] raise: ACPValidationError
		description: 'At least stdio transport must be supported'
]

{ #category : 'tests - agent capabilities' }
ACPCapabilitiesTest >> testMCPCapabilities [
	"Test MCP transport capabilities"

	| capabilities |
	capabilities := ACPMCPCapabilities new
		                supportedTransports: #( 'stdio' 'http' 'websocket' );
		                yourself.

	self assert: (capabilities supports: 'stdio').
	self assert: (capabilities supports: 'http').
	self assert: (capabilities supports: 'websocket').
	self deny: (capabilities supports: 'sse')
]

{ #category : 'tests - agent capabilities' }
ACPCapabilitiesTest >> testPromptCapabilities [
	"Test prompt capabilities configuration"

	| capabilities |
	capabilities := ACPPromptCapabilities new
		                image: true;
		                audio: true;
		                embeddedContext: true;
		                yourself.

	self assert: capabilities supportsImage.
	self assert: capabilities supportsAudio.
	self assert: capabilities supportsEmbeddedContext
]

{ #category : 'tests - session modes' }
ACPCapabilitiesTest >> testSessionModesCapability [
	"Test session modes in agent capabilities"

	| capabilities modes |
	modes := {
		         (ACPSessionMode new
			          modeId: 'code';
			          title: 'Code Mode';
			          description: 'Full implementation mode';
			          yourself).
		         (ACPSessionMode new
			          modeId: 'architect';
			          title: 'Architect Mode';
			          description: 'Planning mode';
			          yourself) }.

	capabilities := ACPAgentCapabilities new
		                sessionModes: modes;
		                yourself.

	self assert: capabilities sessionModes size equals: 2.
	self assert: (capabilities supportsMode: 'code').
	self assert: (capabilities supportsMode: 'architect').
	self deny: (capabilities supportsMode: 'invalid')
]
