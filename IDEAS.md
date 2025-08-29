# Ideas draft

## Alpha.4

- Use built-in JSON String transformation for JSON String output in `S.compile`
- Fix https://github.com/DZakh/sury/issues/150
- Add `S.brand` for TS API
- Update Standard Schema error message to only include reason part
- Fix refinement on union schema which also uses `S.to`
- TS API: Removed `S.transform` in favor of `S.to`

## v11

### ideas

- Add `promise` type and `S.promise` (instead of async flag internally)

TODO:

- Keep current operationFn approach. Rename to makeOperation
- Use define property to be enumerable and simplify copy
- Add counter and set unique id to each schema
- Use the unique id to cache the operationFn (from/to) in the schema (partially solves garbage collection problem)
- Also cache reverse result
- makeParseOrThrow
- parseOrThrow(schema)(data) for ts api
- deprecate compile

```diff
const userSchema = S.schema({
  id: S.string,
  name: S.string
})

S.parseOrThrow(data, userSchema)
+ ts: S.parseOrThrow(userSchema)(data)

- S.parseJsonOrThrow(data, userSchema)
+ res: S.decodeOrThrow(data, S.json, userSchema)
+ ts:  S.decodeOrThrow(S.json, userSchema)(data)

- S.parseJsonStringOrThrow(data, userSchema)
+ res: S.decodeOrThrow(data, S.jsonString, userSchema)
+ ts:  S.decodeOrThrow(S.jsonString, userSchema)(data)

- S.reverseConvertOrThrow(user, userSchema)
+ res: S.encodeOrThrow(user, userSchema, S.unknown)
+ ts:  S.encodeOrThrow(userSchema)(user)

- S.reverseConvertToJsonOrThrow(user, userSchema)
+ res: S.encodeOrThrow(user, userSchema, S.json)
+ ts:  S.encodeOrThrow(userSchema, S.json)(user)

- S.reverseConvertToJsonStringOrThrow(user, userSchema)
+ res: S.encodeOrThrow(user, userSchema, S.jsonString)
+ ts:  S.encodeOrThrow(userSchema, S.jsonString)(user)

- S.reverseConvertToJsonStringOrThrow(user, userSchema, 2)
+ res: S.encodeOrThrow(user, userSchema, S.jsonStringWithSpace(2))
+ ts:  S.encodeOrThrow(userSchema, S.jsonStringWithSpace(2))(user)

- S.convertOrThrow(data, userSchema)
+ ts:  S.decodeOrThrow(userSchema)(data) (when single from Input to Output, when multiple from Output to Output)
+ res: S.decodeOrThrow(data, S.unknown, userSchema) (from Output to Output)

- S.convertToJsonOrThrow(data, userSchema)
+ res: S.decodeOrThrow(data, S.unknown, userSchema) + S.decodeOrThrow(data, userSchema, S.json)
// Because it was from input before

- S.convertToJsonStringOrThrow(data, userSchema)
+ res: S.decodeOrThrow(data, S.unknown, userSchema) + S.decodeFromOrThrow(data, userSchema, S.jsonString)
```

- rename `serializer` to reverse parser ?
- Make `foo->S.to(S.unknown)` stricter ??

- Add `S.to(from, target, parser, serializer)` instead of `S.transform`?
- Remove `s.fail` with `throw new Error`
- Make built-in refinements not work with `unknown`. Use `S.to` (manually & automatically) to deside the type first
- Better inline empty recursive schema operations (union convert)
- Don't iterate over JSON value when it's `S.json` convert without parsing
- Add `S.date.with(S.migrationFrom, S.string, <optionalParser>)`.
- Allow to pass {} instead of S.schema({}) to S.array and other schemas

### Final release fixes

- Add `S.env` to support coercion for union items separately. Like `rescript-envsafe` used to do with `preprocess`
- Make `S.record` accept two args
- Update docs

## v11 initial

- Add `s.parseChild` to EffectContext ???
- Start using rescript v12 (Fix unboxed types in JSONSchema module)
- Support arrays for `S.to`
- Remove fieldOr in favor of optionOr?
- Allow to pass custom error message via `.with`
- Make S.to extensible
- Add S.Date (S.instanceof) and remove S.datetime
- Add refinement info to the tagged type

## v???

- Remove `S.deepStrict` and `S.deepStrip` in favor of `S.deep` (if it works)
- Make S.serializeToJsonString super fast
- Somehow determine whether transformed or not (including shape)
- Add JSDoc
- s.optional for object
- S.transform(s => {
  s.reverse(input => input) // Or s.asyncReverse(input => Promise.resolve(input))
  input => input
  }) // or asyncTransform // Maybe format ?
- Clean up Caml_option.some, Js_dict.get
- Github Action: Add linter checking that the generated files are up to date (?)
- Support optional fields (can have problems with serializing) (???)
- S.mutateWith/S.produceWith (aka immer) (???)
- Add S.function (?) (An alternative for external ???)

```

let trimContract: S.contract<string => string> = S.contract(s => {
s.fn(s.arg(0, S.string))
}, ~return=S.string)

```

- Use internal transform for trim
- Add schema input to the error ??? What about build errors?
- async serializing support
- Add S.promise
- S.create / S.validate
- Add S.codegen
- Rename S.inline to S.toRescriptCode + Codegen type + Codegen schema using type
- Make `error.reason` tree-shakeable
- S.toJSON/S.castToJson ???
- S.produce
- S.mutator
- Check only number of fields for strict object schema when fields are not optional (bad idea since it's not possible to create a good error message, so we still need to have the loop)

```

```
