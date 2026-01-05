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

## Val Type - Critical Fields

- `schema`: What the value currently IS (its actual type)
- `expected`: What we're trying to parse/convert INTO (target type)
- `skipTo`: When `Some(true)`, prevents `parse` from following the `.to` chain

These must be compatible - decoder of `expected` must handle `schema`.

## Parse Flow

```
parse(val) → encoder → decoder → parser → if expected.to exists: recursive parse
```

The `skipTo` check happens twice:

1. Before decoder - if `Some(true)`, skips entire block
2. Before recursive `.to` parse - if `Some(true)`, stops recursion

## Shaped Schemas (S.shape, S.object)

### proxifyShapedSchema

- Wraps schema in Proxy to track field access
- Sets `from` array on each accessed field (path to value in input)
- Uses `getOutputSchema` to follow `.to` chain before copying

### Serialization Flow (shapedSerializer)

1. `prepareShapedSerializerAcc`: Builds acc mapping `from` paths to input vals
2. `getShapedSerializerOutput`: Traverses target schema, looks up vals from acc

### Key Issue Pattern

When `acc.val` exists in `getShapedSerializerOutput`:

- `val.schema` must match what `targetSchema` decoder expects
- If `val.schema` is wrong (e.g., parent object instead of field), get "Unsupported conversion" error

## Reversal

`reverse(schema)` swaps:

- `parser` ↔ `serializer`
- `to` chain is reversed (head becomes tail)
- Properties/items are recursively reversed

During reverse convert: what was `serializer` becomes `parser`.
