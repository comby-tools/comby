# Patdiff test directory layout

patdiff tests are separated into two directories, `src` and
`feedback`.

The goal is to be clear when a test changed because we improved the
diff vs. when a test changed because we broke something.

- `test/src` contains tests that document expected behavior.
- `test/feedback` contains tests that document unexpected behavior,
  which we would like to change (and then move to test/src).
- `test/bin` contains some common code, such as unified test setup
  scripts.
