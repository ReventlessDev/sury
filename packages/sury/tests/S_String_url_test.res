open Ava

test("Successfully parses valid data", t => {
  let schema = S.string->S.url

  t->Assert.deepEqual("http://dzakh.dev"->S.parseOrThrow(schema), "http://dzakh.dev")
})

test("Fails to parse invalid data", t => {
  let schema = S.string->S.url

  t->U.assertThrowsMessage(() => "cifjhdsfhsd"->S.parseOrThrow(schema), `Invalid url`)
})

test("Successfully serializes valid value", t => {
  let schema = S.string->S.url

  t->Assert.deepEqual(
    "http://dzakh.dev"->S.reverseConvertOrThrow(schema),
    %raw(`"http://dzakh.dev"`),
  )
})

test("Fails to serialize invalid value", t => {
  let schema = S.string->S.url

  t->U.assertThrowsMessage(() => "cifjhdsfhsd"->S.reverseConvertOrThrow(schema), `Invalid url`)
})

test("Returns custom error message", t => {
  let schema = S.string->S.url(~message="Custom")

  t->U.assertThrowsMessage(() => "abc"->S.parseOrThrow(schema), `Custom`)
})
