Class {
	#name : 'ACPTestSuite',
	#superclass : 'TestCase',
	#category : 'LLM-Pharo-ACP-Tests',
	#package : 'LLM-Pharo-ACP-Tests'
}

{ #category : 'accessing' }
ACPTestSuite class >> allTestClasses [
	"Return all ACP test classes"

	^ {
		  ACPConnectionTest.
		  ACPSessionTest.
		  ACPMessageTest.
		  ACPContentBlockTest.
		  ACPToolCallTest.
		  ACPClientTest.
		  ACPCapabilitiesTest.
		  ACPIntegrationTest }
]

{ #category : 'running' }
ACPTestSuite class >> run [
	"Run all ACP tests"

	<script>
	^ self allTestClasses collect: [ :testClass | testClass suite run ]
]

{ #category : 'running' }
ACPTestSuite class >> runCore [
	"Run core tests only"

	<script>
	^ {
		  ACPConnectionTest.
		  ACPSessionTest.
		  ACPMessageTest } collect: [ :testClass | testClass suite run ]
]

{ #category : 'running' }
ACPTestSuite class >> runIntegration [
	"Run integration tests only"

	<script>
	^ ACPIntegrationTest suite run
]

{ #category : 'coverage' }
ACPTestSuite class >> runWithCoverage [
	"Run all tests with coverage analysis"

	<script>
	| packages |
	packages := { 'LLM-Pharo-ACP' }.

	^ TestRunner new
		  coverage: packages;
		  run: self allTestClasses
]

{ #category : 'accessing' }
ACPTestSuite class >> testCount [
	"Return total number of tests"

	^ self allTestClasses sum: [ :testClass | testClass suite tests size ]
]
