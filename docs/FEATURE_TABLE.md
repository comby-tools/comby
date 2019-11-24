# Feature Table

This document tabulates the current feature stability and development status.

## Legend

| Status            | Icon                    | Description                                                                                                                                                              |
|-------------------|-------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Stable            | :white_check_mark:      | There are no planned changes to this feature in the forseeable future.                                                                                                   |
| Beta              | :b:                     | The feature is available and in beta for quality control testing, after which it becomes stable                                                                         |
| Experimental      | :alembic:               | This feature is available but how it works may change in the near future. If you rely on this feature, you may need to update the project that uses it in the future.  |
| Under development | :building_construction: | This feature is planned or being actively developed and is not currently available.                                                                                |
| Requested         | :bulb:                  | This feature has been requested but is not being actively developed.                                                                                                     |

## Features

| Name                                        | Status                  | Description                                                                                           |
|---------------------------------------------|-------------------------|-------------------------------------------------------------------------------------------------------|
| Basic template syntax                       | :white_check_mark:      | See [basic syntax](https://comby.dev/#match-syntax)                                                   |
| Matching for balanced alphanumeric keywords | :white_check_mark:      |                                                                                                       |
| Interactive review mode                     | :white_check_mark:      | Use `-review` for interactive review like in [codemod](https://github.com/facebook/codemod)           |
| Sub-matching rule expressions               | :alembic:               | See [sub-matching syntax](https://comby.dev/#experimental-language-features-sub-matching)             |
| Nested rewrite rule expressions             | :alembic:               | See [rewrite expression syntax](https://comby.dev/#experimental-language-features-rewrite-expression) |
| Optional holes                              | :alembic:               | Optional hole syntax, to be documented                                                                |
| Matching for arbitrary balanced tags        | :building_construction: | See [roadmap](https://github.com/comby-tools/comby/blob/master/docs/ROADMAP.md)                       |
| Indentation-sensitive matching              | :building_construction: | See [#65](https://github.com/comby-tools/comby/issues/65) and [roadmap](https://github.com/comby-tools/comby/blob/master/docs/ROADMAP.md)               |
| Interactive mode with git-patch             | :bulb:                  | See [#134](https://github.com/comby-tools/comby/issues/134)                                                |
| Editor extensions                           | :bulb:                  | See [#103](https://github.com/comby-tools/comby/issues/103)                                                |
