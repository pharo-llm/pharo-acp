Extension { #name : 'Dictionary' }

{ #category : '*LLM-Pharo-ACP' }
Dictionary >> asJSONString [
	"Serialize this dictionary to a JSON string"

	^ STONJSON toString: self
]
