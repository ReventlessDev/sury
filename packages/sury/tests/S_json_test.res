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

// TODO: No need to recreate array or json values
let jsonParseCode = `i=>{let v0;v0=e[0](i);return v0}
JSON: i=>{if(Array.isArray(i)){let v3=new Array(i.length);for(let v0=0;v0<i.length;++v0){try{let v1;v1=e[0]["unknown->JSON--0"](i[v0]);v3[v0]=v1}catch(v2){v2.path='["'+v0+'"]'+v2.path;throw v2}}i=v3}else if(typeof i==="object"&&i&&!Array.isArray(i)){let v7={};for(let v4 in i){try{let v5;v5=e[1]["unknown->JSON--0"](i[v4]);v7[v4]=v5}catch(v6){v6.path='["'+v4+'"]'+v6.path;throw v6}}i=v7}else if(!(typeof i==="string"||typeof i==="boolean"||typeof i==="number"&&!Number.isNaN(i)||i===null)){e[2](i)}return i}`
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
