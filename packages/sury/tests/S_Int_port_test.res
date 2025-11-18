open Ava

test("Successfully parses valid data", t => {
  let schema = S.int->S.port

  t->Assert.deepEqual(8080->S.parseOrThrow(schema), 8080)
})

test("Fails to parse invalid data", t => {
  let schema = S.int->S.port

  t->U.assertThrowsMessage(() => 65536->S.parseOrThrow(schema), `Expected port, received 65536`)
})

test("Successfully serializes valid value", t => {
  let schema = S.int->S.port

  t->Assert.deepEqual(8080->S.reverseConvertOrThrow(schema), %raw(`8080`))
})

test("Fails to serialize invalid value", t => {
  let schema = S.int->S.port

  t->U.assertThrowsMessage(
    () => -80->S.reverseConvertOrThrow(schema),
    `Expected port, received -80`,
  )
})

test("Returns custom error message", t => {
  let schema = S.int->S.port(~message="Custom")

  t->U.assertThrowsMessage(() => 400000->S.parseOrThrow(schema), `Custom`)
})

test("Reflects refinement on schema", t => {
  let schema = S.int->S.port

  t->Assert.deepEqual((schema->S.untag).format, Some(Port))
  switch schema {
  | Number({format}) => t->Assert.deepEqual(format, Port)
  | _ => t->Assert.fail("Expected Number with format Port")
  }
})
