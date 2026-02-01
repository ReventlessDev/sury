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
    %raw(`[{"foo": "a", "bar": 0}, {"foo": "b", "bar": 1}]`),
  )

  t->Assert.deepEqual(
    %raw(`[{"foo": "a", "bar": 0}, {"foo": "b", "bar": 1}]`)->S.reverseConvertOrThrow(schema),
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
    %raw(`[{"foo": "a", "bar": 0}, {"foo": "b", "bar": undefined}]`),
  )

  t->Assert.deepEqual(
    %raw(`[{"foo": "a", "bar": 0}, {"foo": "b", "bar": undefined}]`)->S.reverseConvertOrThrow(schema),
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
    %raw(`[{"foo": "a", "bar": true}, {"foo": "b", "bar": true}, {"foo": undefined, "bar": false}]`),
  )

  t->Assert.deepEqual(
    %raw(`[{"foo": "a", "bar": true}, {"foo": "b", "bar": true}, {"foo": undefined, "bar": false}]`)->S.reverseConvertOrThrow(schema),
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

test("Schema has format field set to compactColumns", t => {
  let schema = S.compactColumns(S.unknown)
  // Use Obj.magic to cast schema to untagged representation for testing internal field
  t->Assert.deepEqual((schema->Obj.magic)["format"], "compactColumns")
})
