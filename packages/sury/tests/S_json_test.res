open Ava

S.enableJson()

test("Supports String", t => {
  let schema = S.json
  let data = JSON.Encode.string("Foo")

  t->Assert.deepEqual(data->S.parseOrThrow(schema), data)
  t->Assert.deepEqual(data->S.reverseConvertToJsonOrThrow(schema), data)
})

test("Supports Number", t => {
  let schema = S.json
  let data = JSON.Encode.float(123.)

  t->Assert.deepEqual(data->S.parseOrThrow(schema), data)
  t->Assert.deepEqual(data->S.reverseConvertToJsonOrThrow(schema), data)
})

test("Supports Bool", t => {
  let schema = S.json
  let data = JSON.Encode.bool(true)

  t->Assert.deepEqual(data->S.parseOrThrow(schema), data)
  t->Assert.deepEqual(data->S.reverseConvertToJsonOrThrow(schema), data)
})

test("Supports Null", t => {
  let schema = S.json
  let data = JSON.Encode.null

  t->Assert.deepEqual(data->S.parseOrThrow(schema), data)
  t->Assert.deepEqual(data->S.reverseConvertToJsonOrThrow(schema), data)
})

test("Supports Array", t => {
  let schema = S.json
  let data = JSON.Encode.array([JSON.Encode.string("foo"), JSON.Encode.null])

  t->Assert.deepEqual(data->S.parseOrThrow(schema), data)
  t->Assert.deepEqual(data->S.reverseConvertToJsonOrThrow(schema), data)
})

test("Supports Object", t => {
  let schema = S.json
  let data = JSON.Encode.object(
    [("bar", JSON.Encode.string("foo")), ("baz", JSON.Encode.null)]->Dict.fromArray,
  )

  t->Assert.deepEqual(data->S.parseOrThrow(schema), data)
  t->Assert.deepEqual(data->S.reverseConvertToJsonOrThrow(schema), data)
})

test("Fails to parse Object field", t => {
  let schema = S.json
  let data = JSON.Encode.object(
    [("bar", %raw(`undefined`)), ("baz", JSON.Encode.null)]->Dict.fromArray,
  )

  t->U.assertThrowsMessage(
    () => data->S.parseOrThrow(schema),
    `Failed at ["bar"]: Expected JSON, received undefined`,
  )
})

test("Fails to parse matrix field", t => {
  let schema = S.json
  let data = %raw(`[1,[undefined]]`)

  t->U.assertThrowsMessage(
    () => data->S.parseOrThrow(schema),
    `Failed at ["1"]["0"]: Expected JSON, received undefined`,
  )
})

test("Fails to parse NaN", t => {
  let schema = S.json
  t->U.assertThrowsMessage(() => %raw(`NaN`)->S.parseOrThrow(schema), `Expected JSON, received NaN`)
})

test("Fails to parse undefined", t => {
  let schema = S.json
  t->U.assertThrowsMessage(
    () => %raw(`undefined`)->S.parseOrThrow(schema),
    `Expected JSON, received undefined`,
  )
})

let jsonParseCode = `i=>{let v0;v0=e[0](i);return v0}
JSON: i=>{if(Array.isArray(i)){for(let v0=0;v0<i.length;++v0){try{let v1;v1=e[0]["unknown->JSON--0"](i[v0]);}catch(v2){v2.path='["'+v0+'"]'+v2.path;throw v2}}}else if(typeof i==="object"&&i&&!Array.isArray(i)){for(let v3 in i){try{let v4;v4=e[1]["unknown->JSON--0"](i[v3]);}catch(v5){v5.path='["'+v3+'"]'+v5.path;throw v5}}}else if(!(typeof i==="string"||typeof i==="boolean"||typeof i==="number"&&!Number.isNaN(i)||i===null)){e[2](i)}return i}`
test("Compiled parse code snapshot", t => {
  let schema = S.json

  t->U.assertCompiledCode(~schema, ~op=#Parse, jsonParseCode)
  t->U.assertCompiledCodeIsNoop(~schema, ~op=#Convert)
})

test("Compiled serialize code snapshot", t => {
  let schema = S.json
  t->U.assertCompiledCodeIsNoop(~schema=schema->S.reverse, ~op=#Convert)
  t->U.assertCompiledCode(~schema, ~op=#ReverseParse, jsonParseCode)
})

test("Reverse schema to S.json", t => {
  let schema = S.json
  t->U.assertEqualSchemas(schema->S.reverse, S.json->S.castToUnknown)
})

test("Succesfully uses reversed schema with validate=true for parsing back to initial value", t => {
  let schema = S.json
  t->U.assertReverseParsesBack(schema, %raw(`{"foo":"bar"}`))
})
