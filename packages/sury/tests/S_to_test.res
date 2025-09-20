open Ava

S.enableJson()

test("Coerce from string to string", t => {
  let schema = S.string->S.to(S.string)
  t->Assert.is(schema, S.string)
})

test("Coerce from string to bool", t => {
  let schema = S.string->S.to(S.bool)

  t->Assert.deepEqual("false"->S.parseOrThrow(schema), false)
  t->Assert.deepEqual("true"->S.parseOrThrow(schema), true)
  t->U.assertThrows(
    () => "tru"->S.parseOrThrow(schema),
    {
      code: InvalidType({
        expected: S.bool->S.castToUnknown,
        received: %raw(`"tru"`),
      }),
      path: S.Path.empty,
      operation: Parse,
    },
  )
  t->Assert.deepEqual(false->S.reverseConvertOrThrow(schema), %raw(`"false"`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[1](i)}let v0;(v0=i==="true")||i==="false"||e[0](i);return v0}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Convert,
    `i=>{let v0;(v0=i==="true")||i==="false"||e[0](i);return v0}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{return ""+i}`)
})

test("Coerce from bool to string", t => {
  let schema = S.bool->S.to(S.string)

  t->Assert.deepEqual(false->S.parseOrThrow(schema), "false")
  t->Assert.deepEqual(true->S.parseOrThrow(schema), "true")
  t->U.assertThrows(
    () => "tru"->S.reverseConvertOrThrow(schema),
    {
      code: InvalidType({
        expected: S.bool->S.castToUnknown,
        received: %raw(`"tru"`),
      }),
      path: S.Path.empty,
      operation: ReverseConvert,
    },
  )
  t->Assert.deepEqual("false"->S.reverseConvertOrThrow(schema), %raw(`false`))

  t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{if(typeof i!=="boolean"){e[0](i)}return ""+i}`)
  t->U.assertCompiledCode(~schema, ~op=#Convert, `i=>{return \"\"+i}`)
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{let v0;(v0=i===\"true\")||i===\"false\"||e[0](i);return v0}`,
  )
})

test("Coerce from string to bool literal", t => {
  let schema = S.string->S.to(S.literal(false))

  t->Assert.deepEqual("false"->S.parseOrThrow(schema), false)
  t->U.assertThrows(
    () => "true"->S.parseOrThrow(schema),
    {
      code: InvalidType({
        expected: S.literal(false)->S.castToUnknown,
        received: %raw(`"true"`),
      }),
      path: S.Path.empty,
      operation: Parse,
    },
  )
  t->Assert.deepEqual(false->S.reverseConvertOrThrow(schema), %raw(`"false"`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[1](i)}if(i!=="false"){e[0](i)}return false}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{if(i!==false){e[0](i)}return "false"}`)
})

test("Coerce from string to null literal", t => {
  let schema = S.string->S.to(S.literal(%raw(`null`)))

  t->Assert.deepEqual("null"->S.parseOrThrow(schema), %raw(`null`))
  t->U.assertThrows(
    () => "true"->S.parseOrThrow(schema),
    {
      code: InvalidType({
        expected: S.literal(%raw(`null`))->S.castToUnknown,
        received: %raw(`"true"`),
      }),
      path: S.Path.empty,
      operation: Parse,
    },
  )
  t->Assert.deepEqual(%raw(`null`)->S.reverseConvertOrThrow(schema), %raw(`"null"`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[1](i)}if(i!=="null"){e[0](i)}return null}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{if(i!==null){e[0](i)}return "null"}`)
})

test("Coerce from string to undefined literal", t => {
  let schema = S.string->S.to(S.literal(%raw(`undefined`)))

  t->Assert.deepEqual("undefined"->S.parseOrThrow(schema), %raw(`undefined`))
  t->U.assertThrows(
    () => "true"->S.parseOrThrow(schema),
    {
      code: InvalidType({
        expected: S.literal(%raw(`undefined`))->S.castToUnknown,
        received: %raw(`"true"`),
      }),
      path: S.Path.empty,
      operation: Parse,
    },
  )
  t->Assert.deepEqual(%raw(`undefined`)->S.reverseConvertOrThrow(schema), %raw(`"undefined"`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[1](i)}if(i!=="undefined"){e[0](i)}return void 0}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{if(i!==void 0){e[0](i)}return "undefined"}`,
  )
})

test("Coerce from string to NaN literal", t => {
  let schema = S.string->S.to(S.literal(%raw(`NaN`)))

  t->Assert.deepEqual("NaN"->S.parseOrThrow(schema), %raw(`NaN`))
  t->U.assertThrows(
    () => "true"->S.parseOrThrow(schema),
    {
      code: InvalidType({
        expected: S.literal(%raw(`NaN`))->S.castToUnknown,
        received: %raw(`"true"`),
      }),
      path: S.Path.empty,
      operation: Parse,
    },
  )
  t->Assert.deepEqual(%raw(`NaN`)->S.reverseConvertOrThrow(schema), %raw(`"NaN"`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[1](i)}if(i!=="NaN"){e[0](i)}return NaN}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{if(!Number.isNaN(i)){e[0](i)}return "NaN"}`,
  )
})

test("Coerce from string to string literal", t => {
  let quotedString = `"'\``
  let schema = S.string->S.to(S.literal(quotedString))

  t->Assert.deepEqual(quotedString->S.parseOrThrow(schema), quotedString)
  t->U.assertThrows(
    () => "bar"->S.parseOrThrow(schema),
    {
      code: InvalidType({
        expected: S.literal(quotedString)->S.castToUnknown,
        received: %raw(`"bar"`),
      }),
      path: S.Path.empty,
      operation: Parse,
    },
  )
  t->Assert.deepEqual(quotedString->S.reverseConvertOrThrow(schema), %raw(`quotedString`))
  t->U.assertThrows(
    () => "bar"->S.reverseConvertOrThrow(schema),
    {
      code: InvalidType({
        expected: S.literal(quotedString)->S.castToUnknown,
        received: %raw(`"bar"`),
      }),
      path: S.Path.empty,
      operation: ReverseConvert,
    },
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[1](i)}if(i!=="\\"\'\`"){e[0](i)}return i}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{if(i!=="\\"\'\`"){e[0](i)}return i}`)
})

test("Coerce from object shaped as string to float", t => {
  let schema = S.object(s => s.field("foo", S.string))->S.to(S.float)

  t->Assert.deepEqual({"foo": "123"}->S.parseOrThrow(schema), 123.)
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="object"||!i){e[2](i)}let v0=i["foo"];if(typeof v0!=="string"){e[0](v0)}let v1=+v0;Number.isNaN(v1)&&e[1](v0);return v1}`,
  )

  t->Assert.deepEqual(123.->S.reverseConvertOrThrow(schema), %raw(`{"foo": "123"}`))
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{return {"foo":""+i,}}`)
})

test("Coerce to literal can be used as tag and automatically embeded on reverse operation", t => {
  let schema = S.object(s => {
    let _ = s.field("tag", S.string->S.to(S.literal(true)))
  })

  t->Assert.deepEqual(()->S.reverseConvertOrThrow(schema), %raw(`{"tag": "true"}`))
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{if(i!==void 0){e[0](i)}return {"tag":"true",}}`,
  )

  t->Assert.deepEqual({"tag": "true"}->S.parseOrThrow(schema), ())
  t->U.assertThrows(
    () => {"tag": "false"}->S.parseOrThrow(schema),
    {
      code: InvalidType({
        expected: S.literal(true)->S.castToUnknown,
        received: %raw(`"false"`),
      }),
      path: S.Path.fromLocation("tag"),
      operation: Parse,
    },
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    // FIXME: Test that it'll work with S.refine on S.string
    `i=>{if(typeof i!=="object"||!i){e[1](i)}let v0=i["tag"];if(v0!=="true"){e[0](v0)}return void 0}`,
  )
})

test("Coerce from string to float", t => {
  let schema = S.string->S.to(S.float)

  t->Assert.deepEqual("10"->S.parseOrThrow(schema), 10.)
  t->Assert.deepEqual("10.2"->S.parseOrThrow(schema), 10.2)
  t->U.assertThrows(
    () => "tru"->S.parseOrThrow(schema),
    {
      code: InvalidType({
        expected: S.float->S.castToUnknown,
        received: %raw(`"tru"`),
      }),
      path: S.Path.empty,
      operation: Parse,
    },
  )
  t->Assert.deepEqual(10.->S.reverseConvertOrThrow(schema), %raw(`"10"`))
  t->Assert.deepEqual(10.2->S.reverseConvertOrThrow(schema), %raw(`"10.2"`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[1](i)}let v0=+i;Number.isNaN(v0)&&e[0](i);return v0}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Convert,
    `i=>{let v0=+i;Number.isNaN(v0)&&e[0](i);return v0}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{return ""+i}`)
})

test("Coerce from string to int32", t => {
  let schema = S.string->S.to(S.int)

  t->Assert.deepEqual("10"->S.parseOrThrow(schema), 10)
  t->U.assertThrows(
    () => "2147483648"->S.parseOrThrow(schema),
    {
      code: InvalidType({
        expected: S.int->S.castToUnknown,
        received: %raw(`"2147483648"`),
      }),
      path: S.Path.empty,
      operation: Parse,
    },
  )
  t->U.assertThrows(
    () => "10.2"->S.parseOrThrow(schema),
    {
      code: InvalidType({
        expected: S.int->S.castToUnknown,
        received: %raw(`"10.2"`),
      }),
      path: S.Path.empty,
      operation: Parse,
    },
  )
  t->Assert.deepEqual(10->S.reverseConvertOrThrow(schema), %raw(`"10"`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[1](i)}let v0=+i;(v0>2147483647||v0<-2147483648||v0%1!==0)&&e[0](i);return v0}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Convert,
    `i=>{let v0=+i;(v0>2147483647||v0<-2147483648||v0%1!==0)&&e[0](i);return v0}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{return ""+i}`)
})

test("Coerce from string to port", t => {
  let schema = S.string->S.to(S.int->S.port)

  t->Assert.deepEqual("10"->S.parseOrThrow(schema), 10)
  t->U.assertThrowsMessage(
    () => "2147483648"->S.parseOrThrow(schema),
    `Failed parsing: Expected port, received 2147483648`,
  )
  t->U.assertThrowsMessage(
    () => "10.2"->S.parseOrThrow(schema),
    `Failed parsing: Expected port, received 10.2`,
  )
  t->Assert.deepEqual(10->S.reverseConvertOrThrow(schema), %raw(`"10"`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[2](i)}let v0=+i;Number.isNaN(v0)&&e[0](i);v0>0&&v0<65536&&v0%1===0||e[1](v0);return v0}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Convert,
    `i=>{let v0=+i;Number.isNaN(v0)&&e[0](i);v0>0&&v0<65536&&v0%1===0||e[1](v0);return v0}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{i>0&&i<65536&&i%1===0||e[0](i);return ""+i}`,
  )
})

test("Coerce from true to bool", t => {
  let schema = S.literal(true)->S.to(S.bool)

  t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{if(i!==true){e[0](i)}return i}`)
  t->U.assertCompiledCode(~schema, ~op=#Convert, `i=>{if(i!==true){e[0](i)}return i}`)
})

test("Coerce from string to bigint literal", t => {
  let schema = S.string->S.to(S.literal(10n))

  t->Assert.deepEqual("10"->S.parseOrThrow(schema), 10n)
  t->U.assertThrows(
    () => "11"->S.parseOrThrow(schema),
    {
      code: InvalidType({
        expected: S.literal(10n)->S.castToUnknown,
        received: %raw(`"11"`),
      }),
      path: S.Path.empty,
      operation: Parse,
    },
  )
  t->Assert.deepEqual(10n->S.reverseConvertOrThrow(schema), %raw(`"10"`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[1](i)}if(i!=="10"){e[0](i)}return 10n}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#Convert, `i=>{if(i!=="10"){e[0](i)}return 10n}`)
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{if(i!==10n){e[0](i)}return "10"}`)
})

test("Coerce from string to bigint", t => {
  let schema = S.string->S.to(S.bigint)

  t->Assert.deepEqual("10"->S.parseOrThrow(schema), 10n)
  t->U.assertThrows(
    () => "10.2"->S.parseOrThrow(schema),
    {
      code: InvalidType({
        expected: S.bigint->S.castToUnknown,
        received: %raw(`"10.2"`),
      }),
      path: S.Path.empty,
      operation: Parse,
    },
  )
  t->Assert.deepEqual(10n->S.reverseConvertOrThrow(schema), %raw(`"10"`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[1](i)}let v0;try{v0=BigInt(i)}catch(_){e[0](i)}return v0}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Convert,
    `i=>{let v0;try{v0=BigInt(i)}catch(_){e[0](i)}return v0}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{return ""+i}`)
})

test("Coerce string after a transform", t => {
  let schema = S.string->S.transform(_ => {parser: v => v, serializer: v => v})->S.to(S.bool)

  t->U.assertThrowsMessage(
    () => "true"->S.parseOrThrow(schema),
    `Failed parsing: Expected boolean, received "true"`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[2](i)}let v0=e[0](i);if(typeof v0!=="boolean"){e[1](v0)}return v0}`,
  )

  t->U.assertThrowsMessage(
    () => true->S.parseOrThrow(S.reverse(schema)),
    `Failed parsing: Expected string, received true`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseParse,
    `i=>{if(typeof i!=="boolean"){e[2](i)}let v0=e[0](i);if(typeof v0!=="string"){e[1](v0)}return v0}`,
  )
})

@unboxed
type numberOrBoolean = Number(float) | Boolean(bool)

// FIXME: Test nested union
// FIXME: Test transformed union
test("Coerce string to unboxed union (each item separately)", t => {
  let schema =
    S.string->S.to(
      S.union([
        S.schema(s => Number(s.matches(S.float))),
        S.schema(s => Boolean(s.matches(S.bool))),
      ]),
    )

  t->Assert.deepEqual("10"->S.parseOrThrow(schema), Number(10.))
  t->Assert.deepEqual("true"->S.parseOrThrow(schema), Boolean(true))

  t->Assert.throws(
    () => {
      "t"->S.parseOrThrow(schema)
    },
    ~expectations={
      message: `Failed parsing: Expected number | boolean, received "t"
- Expected number, received "t"
- Expected boolean, received "t"`,
    },
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[3](i)}try{let v0=+i;Number.isNaN(v0)&&e[0](i);i=v0}catch(e0){try{let v1;(v1=i==="true")||i==="false"||e[1](i);i=v1}catch(e1){e[2](i,e0,e1)}}return i}`,
  )

  t->Assert.deepEqual(Number(10.)->S.reverseConvertOrThrow(schema), %raw(`"10"`))
  t->Assert.deepEqual(Boolean(true)->S.reverseConvertOrThrow(schema), %raw(`"true"`))

  // // TODO: Can be improved
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{if(typeof i==="number"&&!Number.isNaN(i)){i=""+i}else if(typeof i==="boolean"){i=""+i}return i}`,
  )
})

// test("Coerce string to JSON schema", t => {
//   let schema = S.string->S.to(
//     S.recursive(self => {
//       S.union([
//         S.schema(_ => Json.Null),
//         S.schema(s => Json.Number(s.matches(S.float))),
//         S.schema(s => Json.Boolean(s.matches(S.bool))),
//         S.schema(s => Json.String(s.matches(S.string))),
//         S.schema(s => Json.Object(s.matches(S.dict(self)))),
//         S.schema(s => Json.Array(s.matches(S.array(self)))),
//       ])
//     }),
//   )

//   t->U.assertCompiledCode(
//     ~schema,
//     ~op=#ReverseConvert,
//     ``,
//   )
// })

test("Keeps description of the schema we are coercing to (not working)", t => {
  // Fix it later if it's needed
  let schema = S.string->S.to(S.string->S.meta({description: "To descr"}))
  t->Assert.is((schema->S.untag).description, None)

  // let schema = S.string->S.description("From descr")->S.to(S.string->S.description("To descr"))
  // t->Assert.is((schema->S.untag).description, Some("To descr"))

  // There's no specific reason for it. Just wasn't needed for cases S.to initially designed
  let schema = S.string->S.meta({description: "From descr"})->S.to(S.string)
  t->Assert.is((schema->S.untag).description, Some("From descr"))
})

test("Coerce from unit to null literal", t => {
  let schema = S.unit->S.to(S.literal(%raw(`null`)))

  t->Assert.deepEqual(()->S.parseOrThrow(schema), %raw(`null`))
  t->U.assertThrows(
    () => %raw(`null`)->S.parseOrThrow(schema),
    {
      code: InvalidType({
        expected: S.unit->S.castToUnknown,
        received: %raw(`null`),
      }),
      path: S.Path.empty,
      operation: Parse,
    },
  )
  t->Assert.deepEqual(%raw(`null`)->S.reverseConvertOrThrow(schema), %raw(`undefined`))

  t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{if(i!==void 0){e[0](i)}return null}`)
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{if(i!==null){e[0](i)}return void 0}`)
})

test("Coerce from string to optional bool", t => {
  let schema = S.string->S.to(S.option(S.bool))

  t->Assert.deepEqual("undefined"->S.parseOrThrow(schema), None)
  t->Assert.deepEqual("true"->S.parseOrThrow(schema), Some(true))
  t->U.assertThrows(
    () => %raw(`null`)->S.parseOrThrow(schema),
    {
      code: InvalidType({
        expected: schema->S.castToUnknown,
        received: %raw(`null`),
      }),
      path: S.Path.empty,
      operation: Parse,
    },
  )

  t->Assert.deepEqual(Some(true)->S.reverseConvertOrThrow(schema), %raw(`"true"`))
  t->Assert.deepEqual(None->S.reverseConvertOrThrow(schema), %raw(`"undefined"`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[3](i)}try{let v0;(v0=i==="true")||i==="false"||e[0](i);i=v0}catch(e0){try{if(i!=="undefined"){e[1](i)}i=void 0}catch(e1){e[2](i,e0,e1)}}return i}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{if(typeof i==="boolean"){i=""+i}else if(i===void 0){i="undefined"}return i}`,
  )
})

test("Coerce from object to string", t => {
  let schema = S.schema(s =>
    {
      "foo": s.matches(S.string),
    }
  )->S.to(S.string)

  t->U.assertThrowsMessage(() => {
    %raw(`{"foo": "bar"}`)->S.parseOrThrow(schema)
  }, `Failed parsing: Unsupported transformation from { foo: string; } to string`)
  t->U.assertThrowsMessage(() => {
    %raw(`{"foo": "bar"}`)->S.reverseConvertOrThrow(schema)
  }, `Failed converting: Unsupported transformation from string to { foo: string; }`)
})

test("Coerce from string to JSON and then to bigint", t => {
  let schema = S.string->S.to(S.json)->S.to(S.bigint)

  t->Assert.deepEqual("123"->S.parseOrThrow(schema), %raw(`123n`))
  t->Assert.deepEqual(123n->S.reverseConvertOrThrow(schema), %raw(`"123"`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[1](i)}let v0;try{v0=BigInt(i)}catch(_){e[0](i)}return v0}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{return ""+i}`)
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseParse,
    `i=>{if(typeof i!=="bigint"){e[0](i)}return ""+i}`,
  )
})

test("Coerce from JSON to bigint", t => {
  let schema = S.json->S.to(S.bigint)

  t->Assert.deepEqual("123"->S.parseOrThrow(schema), %raw(`123n`))
  t->U.assertThrowsMessage(() => {
    123->S.parseOrThrow(schema)
  }, "Failed parsing: Expected string, received 123")
  t->U.assertThrowsMessage(() => {
    true->S.parseOrThrow(schema)
  }, "Failed parsing: Expected string, received true")

  t->Assert.deepEqual(123n->S.reverseConvertOrThrow(schema), %raw(`"123"`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[1](i)}let v0;try{v0=BigInt(i)}catch(_){e[0](i)}return v0}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{return ""+i}`)
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseParse,
    `i=>{if(typeof i!=="bigint"){e[0](i)}return ""+i}`,
  )
})

test("Coerce from JSON to unit", t => {
  let schema = S.json->S.to(S.unit)

  t->Assert.deepEqual(%raw(`null`)->S.parseOrThrow(schema), ())
  t->U.assertThrowsMessage(() => {
    %raw(`undefined`)->S.parseOrThrow(schema)
  }, "Failed parsing: Expected null, received undefined")
  t->Assert.deepEqual(()->S.reverseConvertOrThrow(schema), %raw(`null`))

  t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{if(i!==null){e[0](i)}return void 0}`)
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{if(i!==void 0){e[0](i)}return null}`)
})

test("Coerce from JSON to NaN", t => {
  let schema = S.json->S.to(S.literal(%raw(`NaN`)))

  t->Assert.deepEqual(%raw(`null`)->S.parseOrThrow(schema), %raw(`NaN`))
  t->U.assertThrowsMessage(() => {
    %raw(`undefined`)->S.parseOrThrow(schema)
  }, "Failed parsing: Expected null, received undefined")
  t->Assert.deepEqual(%raw(`NaN`)->S.reverseConvertOrThrow(schema), %raw(`null`))

  t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{if(i!==null){e[0](i)}return NaN}`)
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{if(!Number.isNaN(i)){e[0](i)}return null}`,
  )
})

test("Coerce from JSON to optional bigint", t => {
  let schema = S.json->S.to(S.option(S.bigint))

  t->Assert.deepEqual(%raw(`null`)->S.parseOrThrow(schema), None)
  t->Assert.deepEqual(%raw(`"123"`)->S.parseOrThrow(schema), Some(123n))
  t->U.assertThrowsMessage(
    () => {
      %raw(`123`)->S.parseOrThrow(schema)
    },
    `Failed parsing: Expected bigint | undefined, received 123
- Expected string, received 123
- Expected null, received 123`,
  )
  t->Assert.deepEqual(None->S.reverseConvertOrThrow(schema), %raw(`null`))
  t->Assert.deepEqual(Some(123n)->S.reverseConvertOrThrow(schema), %raw(`"123"`))

  // TODO: Improve union logic to avoid try/catch
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{try{if(typeof i!=="string"){e[1](i)}let v0;try{v0=BigInt(i)}catch(_){e[0](i)}i=v0}catch(e0){try{if(i!==null){e[2](i)}i=void 0}catch(e1){e[3](i,e0,e1)}}return i}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{if(typeof i==="bigint"){i=""+i}else if(i===void 0){i=null}return i}`,
  )
})

test("Coerce from JSON to array of bigint", t => {
  let schema = S.json->S.to(S.array(S.bigint))

  t->Assert.deepEqual(%raw(`["123"]`)->S.parseOrThrow(schema), [123n])
  t->U.assertThrowsMessage(() => {
    %raw(`[123]`)->S.parseOrThrow(schema)
  }, `Failed parsing at ["0"]: Expected string, received 123`)
  t->Assert.deepEqual([123n]->S.reverseConvertOrThrow(schema), %raw(`["123"]`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(!Array.isArray(i)){e[2](i)}let v7=new Array(i.length);for(let v2=0;v2<i.length;++v2){let v6;try{let v5=i[v2];if(typeof v5!=="string"){e[1](v5)}let v4;try{v4=BigInt(v5)}catch(_){e[0](v5)}v6=v4}catch(v3){if(v3&&v3.s===s){v3.path=""+\'["\'+v2+\'"]\'+v3.path}throw v3}v7[v2]=v6}return v7}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{let v5=new Array(i.length);for(let v2=0;v2<i.length;++v2){let v4;try{v4=""+i[v2]}catch(v3){if(v3&&v3.s===s){v3.path=""+\'["\'+v2+\'"]\'+v3.path}throw v3}v5[v2]=v4}return v5}`,
  )
})

test("Coerce from JSON to tuple with bigint", t => {
  let schema = S.json->S.to(S.schema(s => (s.matches(S.string), s.matches(S.bigint))))

  t->Assert.deepEqual(%raw(`["foo", "123"]`)->S.parseOrThrow(schema), ("foo", 123n))
  t->U.assertThrowsMessage(() => {
    %raw(`["foo"]`)->S.parseOrThrow(schema)
  }, `Failed parsing: Expected [string, bigint], received ["foo"]`)
  t->Assert.deepEqual(("foo", 123n)->S.reverseConvertOrThrow(schema), %raw(`["foo", "123"]`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(!Array.isArray(i)){e[4](i)}if(i.length!==2){e[3](i)}let v2=i["0"],v4=i["1"];if(typeof v2!=="string"){e[0](v2)}if(typeof v4!=="string"){e[2](v4)}let v3;try{v3=BigInt(v4)}catch(_){e[1](v4)}return [v2,v3,]}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{return [i["0"],""+i["1"],]}`)
})

// test("Coerce from JSON to object with optional field", t => {
//   let schema = S.json->S.to(
//     S.schema(s =>
//       {
//         "id": s.matches(S.bigint),
//         "isDeleted": s.matches(S.option(S.string)),
//       }
//     ),
//   )

//   // t->Assert.deepEqual(
//   //   {
//   //     "id": "123",
//   //   }->S.parseOrThrow(schema),
//   //   {
//   //     "id": 123n,
//   //     "isDeleted": None,
//   //   },
//   // )
//   // t->U.assertThrowsMessage(() => {
//   //   123->S.parseOrThrow(schema)
//   // }, "Failed parsing: Expected string, received 123")
//   // t->U.assertThrowsMessage(() => {
//   //   true->S.parseOrThrow(schema)
//   // }, "Failed parsing: Expected string, received true")

//   // t->Assert.deepEqual(123n->S.reverseConvertOrThrow(schema), %raw(`"123"`))

//   t->U.assertCompiledCode(
//     ~schema,
//     ~op=#Parse,
//     `i=>{if(typeof i!=="string"){e[1](i)}let v0;try{v0=BigInt(i)}catch(_){e[0](i)}return v0}`,
//   )
//   // t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{return ""+i}`)
//   // t->U.assertCompiledCode(
//   //   ~schema,
//   //   ~op=#ReverseParse,
//   //   `i=>{if(typeof i!=="bigint"){e[0](i)}return ""+i}`,
//   // )
// })

test("Coerce from union to bigint", t => {
  let schema =
    S.union([S.string->S.castToUnknown, S.float->S.castToUnknown, S.bool->S.castToUnknown])->S.to(
      S.bigint,
    )

  t->Assert.deepEqual("123"->S.parseOrThrow(schema), %raw(`123n`))
  t->Assert.deepEqual(123->S.parseOrThrow(schema), %raw(`123n`))
  t->U.assertThrowsMessage(() => {
    true->S.parseOrThrow(schema)
  }, "Failed parsing: Unsupported transformation from boolean to bigint")
  t->U.assertThrowsMessage(() => {
    123n->S.parseOrThrow(schema)
  }, "Failed parsing: Expected string | number | boolean, received 123n")

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i==="string"){let v0;try{v0=BigInt(i)}catch(_){e[0](i)}i=v0}else if(typeof i==="number"&&!Number.isNaN(i)){i=BigInt(i)}else if(typeof i==="boolean"){throw e[1]}else{e[2](i)}return i}`,
  )

  t->Assert.deepEqual(123n->S.reverseConvertOrThrow(schema), %raw(`"123"`))

  // TODO: Can be improved
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{try{i=""+i}catch(e0){try{throw e[0]}catch(e1){try{throw e[1]}catch(e2){e[2](i,e0,e1,e2)}}}return i}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseParse,
    `i=>{if(typeof i!=="bigint"){e[3](i)}try{i=""+i}catch(e0){try{throw e[0]}catch(e1){try{throw e[1]}catch(e2){e[2](i,e0,e1,e2)}}}return i}`,
  )
})

test("Coerce from union to bigint with refinement on union", t => {
  let schema =
    S.union([S.string->S.castToUnknown, S.float->S.castToUnknown, S.bool->S.castToUnknown])
    ->S.refine(s =>
      v =>
        if typeof(v) === #bigint {
          s.fail("Unsupported bigint")
        }
    )
    ->S.to(S.bigint)

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i==="string"){e[0](i);let v0;try{v0=BigInt(i)}catch(_){e[1](i)}i=v0}else if(typeof i==="number"&&!Number.isNaN(i)){e[2](i);i=BigInt(i)}else if(typeof i==="boolean"){throw e[4]}else{e[5](i)}return i}`,
  )
})

test("Coerce from union to bigint with refinement on union (with an item transformed to)", t => {
  let schema =
    S.union([
      S.string->S.castToUnknown,
      S.float->S.to(S.string)->S.castToUnknown,
      S.bool->S.castToUnknown,
    ])
    ->S.refine(s =>
      v =>
        if typeof(v) === #bigint {
          s.fail("Unsupported bigint")
        }
    )
    ->S.to(S.bigint)

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i==="string"){e[0](i);let v0;try{v0=BigInt(i)}catch(_){e[1](i)}i=v0}else if(typeof i==="number"&&!Number.isNaN(i)){let v1=""+i;e[2](v1);let v2;try{v2=BigInt(v1)}catch(_){e[3](v1)}i=v2}else if(typeof i==="boolean"){throw e[5]}else{e[6](i)}return i}`,
    ~message="Should apply refinement after the item transformation",
  )
})

test("Coerce from union to bigint and then to string", t => {
  let schema =
    S.union([S.string->S.castToUnknown, S.float->S.castToUnknown, S.bool->S.castToUnknown])
    ->S.to(S.bigint)
    ->S.to(S.string)

  t->Assert.deepEqual("123"->S.parseOrThrow(schema), %raw(`"123"`))
  t->Assert.deepEqual(123->S.parseOrThrow(schema), %raw(`"123"`))
  t->U.assertThrowsMessage(() => {
    true->S.parseOrThrow(schema)
  }, "Failed parsing: Unsupported transformation from boolean to bigint")
  t->U.assertThrowsMessage(() => {
    123n->S.parseOrThrow(schema)
  }, "Failed parsing: Expected string | number | boolean, received 123n")

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i==="string"){let v0;try{v0=BigInt(i)}catch(_){e[0](i)}i=""+v0}else if(typeof i==="number"&&!Number.isNaN(i)){i=""+BigInt(i)}else if(typeof i==="boolean"){throw e[1]}else{e[2](i)}return i}`,
  )

  t->Assert.deepEqual("123"->S.reverseConvertOrThrow(schema), %raw(`"123"`))
  t->U.assertThrowsMessage(() => {
    "abc"->S.reverseConvertOrThrow(schema)
  }, `Failed parsing: Expected bigint, received "abc"`)

  // TODO: Can be improved
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{let v0;try{v0=BigInt(i)}catch(_){e[0](i)}try{v0=""+v0}catch(e0){try{throw e[1]}catch(e1){try{throw e[2]}catch(e2){e[3](v0,e0,e1,e2)}}}return v0}`,
  )
})

test("Coerce from union to wider union should keep the original value type", t => {
  let schema =
    S.union([S.string->S.castToUnknown, S.float->S.castToUnknown])->S.to(
      S.union([S.string->S.castToUnknown, S.float->S.castToUnknown, S.bool->S.castToUnknown]),
    )

  t->Assert.deepEqual("123"->S.parseOrThrow(schema), %raw(`"123"`))
  t->Assert.deepEqual(123->S.parseOrThrow(schema), %raw(`123`))
  t->U.assertThrowsMessage(() => {
    true->S.parseOrThrow(schema)
  }, "Failed parsing: Expected string | number, received true")

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(!(typeof i==="string"||typeof i==="number"&&!Number.isNaN(i))){e[0](i)}return i}`,
  )
})

test("Fails to transform union to union to string (no reason for this, just not supported)", t => {
  let schema =
    S.union([S.string->S.castToUnknown, S.float->S.castToUnknown])
    ->S.to(S.union([S.string->S.castToUnknown, S.float->S.castToUnknown, S.bool->S.castToUnknown]))
    ->S.to(S.string)

  t->U.assertThrowsMessage(() => {
    true->S.parseOrThrow(schema)
  }, "Failed parsing: Unsupported transformation from string | number to string")
})

test(
  "Coerce from union to wider union fails if the order of items is different (no reason for this, just not supported)",
  t => {
    let schema =
      S.union([S.string->S.castToUnknown, S.float->S.castToUnknown])->S.to(
        S.union([S.float->S.castToUnknown, S.string->S.castToUnknown, S.bool->S.castToUnknown]),
      )

    t->U.assertThrowsMessage(() => {
      true->S.parseOrThrow(schema)
    }, "Failed parsing: Unsupported transformation from string | number to number | string | boolean")
  },
)
