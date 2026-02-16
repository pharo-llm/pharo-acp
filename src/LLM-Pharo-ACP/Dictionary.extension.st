Extension { #name : 'LLM-Pharo-ACP' }

{ #category : '*LLM-Pharo-ACP' }
Dictionary >> asJSONString [
	"Serialize this dictionary to a JSON string"

	^ STONJSON toString: self
]
