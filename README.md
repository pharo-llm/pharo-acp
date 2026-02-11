# Pharo-ACP

[![Pharo 13 & 14](https://img.shields.io/badge/Pharo-13%20%7C%2014-2c98f0.svg)](https://github.com/pharo-llm/pharo-acp)
[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://github.com/pharo-llm/pharo-acp/blob/master/LICENSE)
[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](https://github.com/pharo-llm/pharo-acp/pulls)
[![Status: Active](https://img.shields.io/badge/status-active-success.svg)](https://github.com/pharo-llm/pharo-acp)
[![CI](https://github.com/omarabedelkader/ChatPharo/actions/workflows/CI.yml/badge.svg)](https://github.com/pharo-llm/pharo-acp/actions/workflows/CI.yml)

Pharo-ACP implements the Agent Client Protocol (ACP) within Pharo so that Pharo tools or agents can communicate using the standardized ACP format (allowing them to interoperate with any editor or client that supports ACP)

Official ACP site: [https://agentclientprotocol.com/](https://agentclientprotocol.com/)

To install stable version of `Pharo-ACP` in your image you can use:

```smalltalk
Metacello new
  githubUser: 'pharo-llm' project: 'pharo-acp' commitish: 'X.X.X' path: 'src';
  baseline: 'LLMPharoACP';
  load
```


For development version install it with this:

```smalltalk
Metacello new
  githubUser: 'pharo-llm' project: 'pharo-acp' commitish: 'main' path: 'src';
  baseline: 'LLMPharoACP';
  load.
```
