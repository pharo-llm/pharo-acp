Class {
	#name : 'ACPResourceContent',
	#superclass : 'ACPContentBlock',
	#instVars : [
		'resource'
	],
	#category : 'LLM-Pharo-ACP-Content',
	#package : 'LLM-Pharo-ACP',
	#tag : 'Content'
}

{ #category : 'serialization' }
ACPResourceContent >> asJSON [

	| json |
	json := Dictionary new.
	json at: 'type' put: 'resource'.
	json at: 'resource' put: resource asJSON.
	^ json
]

{ #category : 'accessing' }
ACPResourceContent >> resource [

	^ resource
]

{ #category : 'accessing' }
ACPResourceContent >> resource: anACPResource [

	resource := anACPResource
]

{ #category : 'accessing' }
ACPResourceContent >> type [

	^ 'resource'
]
