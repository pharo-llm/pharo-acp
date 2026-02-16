Class {
	#name : 'ACPPermissionOption',
	#superclass : 'Object',
	#instVars : [
		'optionId',
		'kind',
		'label'
	],
	#category : 'LLM-Pharo-ACP-Messages',
	#package : 'LLM-Pharo-ACP',
	#tag : 'Messages'
}

{ #category : 'serialization' }
ACPPermissionOption >> asJSON [

	| json |
	json := Dictionary new.
	json at: 'optionId' put: optionId.
	json at: 'kind' put: kind.
	json at: 'label' put: label.
	^ json
]

{ #category : 'accessing' }
ACPPermissionOption >> kind [

	^ kind
]

{ #category : 'accessing' }
ACPPermissionOption >> kind: aString [

	kind := aString
]

{ #category : 'accessing' }
ACPPermissionOption >> label [

	^ label
]

{ #category : 'accessing' }
ACPPermissionOption >> label: aString [

	label := aString
]

{ #category : 'accessing' }
ACPPermissionOption >> optionId [

	^ optionId
]

{ #category : 'accessing' }
ACPPermissionOption >> optionId: aString [

	optionId := aString
]
