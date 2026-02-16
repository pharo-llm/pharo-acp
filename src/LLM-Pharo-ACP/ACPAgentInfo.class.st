Class {
	#name : 'ACPAgentInfo',
	#superclass : 'Object',
	#instVars : [
		'name',
		'version'
	],
	#category : 'LLM-Pharo-ACP-Core',
	#package : 'LLM-Pharo-ACP',
	#tag : 'Core'
}

{ #category : 'accessing' }
ACPAgentInfo >> name [

	^ name
]

{ #category : 'accessing' }
ACPAgentInfo >> name: aString [

	name := aString
]

{ #category : 'accessing' }
ACPAgentInfo >> version [

	^ version
]

{ #category : 'accessing' }
ACPAgentInfo >> version: aString [

	version := aString
]

{ #category : 'serialization' }
ACPAgentInfo >> asJSON [

	| json |
	json := Dictionary new.
	json at: 'name' put: name.
	version ifNotNil: [ json at: 'version' put: version ].
	^ json
]

{ #category : 'instance creation' }
ACPAgentInfo class >> fromDictionary: aDictionary [

	^ self new
		  name: (aDictionary at: 'name');
		  version: (aDictionary at: 'version' ifAbsent: [ nil ]);
		  yourself
]
