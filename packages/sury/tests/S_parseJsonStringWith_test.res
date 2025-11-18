open Ava

S.enableJsonString()

test("Successfully parses", t => {
  let schema = S.bool

  t->Assert.deepEqual("true"->S.parseJsonStringOrThrow(schema), true)
})

test("Successfully parses unknown", t => {
  let schema = S.unknown

  t->Assert.deepEqual("true"->S.parseJsonStringOrThrow(schema), true->Obj.magic)
})

test("Fails to parse JSON", t => {
  let schema = S.bool

  switch "123,"->S.parseJsonStringOrThrow(schema) {
  | _ => t->Assert.fail("Must return Error")
  | exception S.Exn({reason, path}) => {
      t->Assert.deepEqual(path, S.Path.empty)
      switch reason {
      // Different errors for different Node.js versions
      | "Unexpected token , in JSON at position 3"
      | "Unexpected non-whitespace character after JSON at position 3"
      | "Unexpected non-whitespace character after JSON at position 3 (line 1 column 4)" => ()
      | _ => t->Assert.fail("Invalid reason")
      }
    }
  }
})

test("Fails to parse", t => {
  let schema = S.bool

  t->U.assertThrowsMessage(
    () => "123"->S.parseJsonStringOrThrow(schema),
    `Expected boolean, received 123`,
  )
})
