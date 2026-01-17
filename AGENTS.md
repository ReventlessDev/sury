# Test Guidelines

## Use Wallaby MCP first

- Use Wallaby.js for test results, errors, and debugging
- Leverage runtime values and coverage data when debugging tests
- Fall back to terminal only if Wallaby isn't available

1. Analyze failing tests with Wallaby and identify the cause of the failure.
2. Use Wallaby's covered files to find relevant implementation files or narrow your search.
3. Use Wallaby's runtime values tool and coverage tool to support your reasoning.
4. Suggest and explain a code fix that will resolve the failure.
5. After the fix, use Wallaby's reported test state to confirm that the test now passes.
6. If the test still fails, continue iterating with updated Wallaby data until it passes.
7. If a snapshot update is needed, use Wallaby's snapshot tools for it.

When responding:

- Explain your reasoning step by step.
- Use runtime and coverage data directly to justify your conclusions.

# Sury Architecture

A schema is a representation of TWO types. Input and output

1. S.string - Sometimes input and output are the same

   - Input: string
   - Output: string

2. S.schema({
   foo: S.string.with(S.to, S.number)
   }) - In this case, the input and output are different, even though the S.schema won't have .to property itself
   - Input: { foo: string }
   - Output: { foo: number }

When we modify a schema, we modify the output type.

```ts
S.schema({
  foo: S.string.with(S.to, S.number)
}).with(S.refine, () => {...})
```

Since the case doesn't have .to, we MUST deffirentiate between input and output refines to support `S.reverse` - Every schema should be able to be reversed from Input->Output to Output->Input, unless it's explicitly prevented.

We should also try to store every data-point on schema to be able to use them to compile a decode function.

The decode function should be created from a single schema and must transform schema input to output. For multiple schemas it automatically joins them by .to property and turns into a single one.

This makes schema to have the following properties and run them in order:

- inputRefiner - Custom validations to the input part of the schema value
- innerDecoder - Decoding of inner items like object fields
- outputRefiner - Custom validations to the output part of the schema value

If schema has .to property:

- parser - Custom transformation logic to the .to schema (serializer is a reverse of parser)

And if there's no .parser:

- encoder - Transformation logic from the current schema to the .to schema
- decoder - Transformation logic of the .to schema from any other schema

After the step either finish the decode function or continue with the inputRefiner.

Additionally for async support we should be aware of that every transformation might return an async value, so to continue the transformation chain we need to append .then and continue the logic in the callback function. For innerDecoder it should create a promise which collects all inner items.

Every transformation point is connected by a val