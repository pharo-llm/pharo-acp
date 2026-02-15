Class {
	#name : 'BaselineOfLLMPharoACP',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfLLMPharoACP',
	#package : 'BaselineOfLLMPharoACP'
}

{ #category : 'baselines' }
BaselineOfLLMPharoACP >> baseline: spec [

	<baseline>
	spec for: #common do: [
		spec
			package: 'LLM-Pharo-ACP';
			package: 'LLM-Pharo-ACP-Tests'
			with: [ spec requires: #( 'LLM-Pharo-ACP' ) ].
		spec
			group: 'Core' with: #( 'LLM-Pharo-ACP' );
			group: 'Tests' with: #( 'LLM-Pharo-ACP-Tests' );
			group: 'default' with: #( 'Core' 'Tests' ) ]
]
