```
curl 'https://sourcegraph.com/.api/graphql' \
  --data-raw $'{"query":"query Hover($repository: String\u0021, $commit: String\u0021, $path: String\u0021, $line: Int\u0021, $character: Int\u0021) {\\n  repository(name: $repository) {\\n    commit(rev: $commit) {\\n      blob(path: $path) {\\n        lsif {\\n          hover(line: $line, character: $character) {\\n            markdown {\\n              text\\n            }\\n            range {\\n              start {\\n                line\\n                character\\n              }\\n              end {\\n                line\\n                character\\n              }\\n            }\\n          }\\n        }\\n      }\\n    }\\n  }\\n}","variables":{"line":10,"character":30,"commit":"HEAD","path":"lib/codeintel/semantic/hash.go","repository":"github.com/sourcegraph/sourcegraph"},"operationName":"Hover"}' \
  --compressed
```
