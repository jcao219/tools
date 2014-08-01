Assertions
==========

Simple `assert` statements in OCaml.

These are the preferred tools for writing unit tests.
The average test should look something like the following:

```
TEST_UNIT "test_name" =
  let result = ... in
  assert_xxx result
```

where the ellipses are replaced with a call to students' code and the `xxx` is replaced with an actual assertion.
See `assertions.mli` for a rundown of available commands.

The file `serializer.ml` attempts to turn arbitrary OCaml values into strings at runtime.

