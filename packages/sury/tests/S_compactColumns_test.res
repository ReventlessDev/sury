open Ava

test("Successfully parses and reverse converts a simple object with compactColumns", t => {
  let schema =
    S.compactColumns(S.unknown)->S.to(
      S.schema(s =>
        {
          "foo": s.matches(S.string),
          "bar": s.matches(S.int),
        }
      ),
    )

  t->Assert.deepEqual(
    %raw(`[["a", "b"], [0, 1]]`)->S.parseOrThrow(schema),
    [{"foo": "a", "bar": 0}, {"foo": "b", "bar": 1}],
  )

  t->Assert.deepEqual(
    [{"foo": "a", "bar": 0}, {"foo": "b", "bar": 1}]->S.reverseConvertOrThrow(schema),
    %raw(`[["a", "b"], [0, 1]]`),
  )
})

test("Transforms nullable fields", t => {
  let schema =
    S.compactColumns(S.unknown)->S.to(
      S.schema(s =>
        {
          "foo": s.matches(S.string),
          "bar": s.matches(S.nullAsOption(S.int)),
        }
      ),
    )

  t->Assert.deepEqual(
    %raw(`[["a", "b"], [0, null]]`)->S.parseOrThrow(schema),
    [{"foo": "a", "bar": Some(0)}, {"foo": "b", "bar": None}],
  )

  t->Assert.deepEqual(
    [{"foo": "a", "bar": Some(0)}, {"foo": "b", "bar": None}]->S.reverseConvertOrThrow(schema),
    %raw(`[["a", "b"], [0, null]]`),
  )
})

test("Case with missing item at the end", t => {
  let schema =
    S.compactColumns(S.unknown)->S.to(
      S.schema(s =>
        {
          "foo": s.matches(S.option(S.string)),
          "bar": s.matches(S.bool),
        }
      ),
    )

  t->Assert.deepEqual(
    %raw(`[["a", "b"], [true, true, false]]`)->S.parseOrThrow(schema),
    [{"foo": Some("a"), "bar": true}, {"foo": Some("b"), "bar": true}, {"foo": None, "bar": false}],
  )

  t->Assert.deepEqual(
    [
      {"foo": Some("a"), "bar": true},
      {"foo": Some("b"), "bar": true},
      {"foo": None, "bar": false},
    ]->S.reverseConvertOrThrow(schema),
    %raw(`[["a", "b", undefined], [true, true, false]]`),
  )
})

test("Handles empty objects", t => {
  t->Assert.throws(
    () => {
      S.compactColumns(S.unknown)->S.to(S.object(_ => ()))->S.parseOrThrow(%raw(`[]`))
    },
    ~expectations={
      message: "[Sury] Invalid empty object for S.compactColumns schema.",
    },
  )
})

test("Handles non-object schemas", t => {
  t->Assert.throws(
    () => {
      S.compactColumns(S.unknown)->S.to(S.tuple2(S.string, S.int))->S.parseOrThrow(%raw(`[]`))
    },
    ~expectations={
      message: "[Sury] Conversion from \"unknown\" to \"unknown\" is not supported",
    },
  )
})

test("Schema has compactColumns field storing the input schema", t => {
  let inputSchema = S.unknown
  let schema = S.compactColumns(inputSchema)
  t->Assert.deepEqual((schema->S.toUntagged).compactColumns, Some(inputSchema))
})
