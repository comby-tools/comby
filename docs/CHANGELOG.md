## 0.9.0

- Adds interactive mode with the `-review` option. Edit can be set with `-editor`, and default accept behavior can be toggled with `-default-no` (as in [codemod](https://github.com/facebook/codemod)).
- `-match-only` returns matches on single lines, prefixed by the matched files, like `grep`. Newlines are converted to `\n`.
- Allow rewrite templates to contain `[hole\n]`, `[ hole]`, `:[hole.]` syntax which substitute for variable `hole`.

## 0.8.0

Changelog starts
