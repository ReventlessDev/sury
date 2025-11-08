@@uncurried
@@warning("-30")

type never

external castAnyToUnknown: 'any => unknown = "%identity"

module Obj = {
  external magic: 'a => 'b = "%identity"
}

module X = {
  module Proxy = {
    type traps<'a> = {get?: (~target: 'a, ~prop: unknown) => unknown}

    @new
    external make: ('a, traps<'a>) => 'a = "Proxy"
  }

  module Option = {
    external getUnsafe: option<'a> => 'a = "%identity"

    // external unsafeToBool: option<'a> => bool = "%identity"
  }

  module Promise = {
    type t<+'a> = promise<'a>

    @send
    external thenResolveWithCatch: (t<'a>, 'a => 'b, exn => 'b) => t<'b> = "then"

    @send
    external thenResolve: (t<'a>, 'a => 'b) => t<'b> = "then"

    @val @scope("Promise")
    external resolve: 'a => t<'a> = "resolve"
  }

  module Object = {
    let immutableEmpty = %raw(`{}`)

    @val external internalClass: Js.Types.obj_val => string = "Object.prototype.toString.call"

    // Define a type for the property descriptor
    type propertyDescriptor<'a> = {
      configurable?: bool,
      enumerable?: bool,
      writable?: bool,
      value?: 'a,
      get?: unit => 'a,
      set?: 'a => unit,
    }

    external defineProperty: ('obj, string, propertyDescriptor<'a>) => 'obj = "d"
  }

  module Array = {
    let immutableEmpty = %raw(`[]`)

    @send
    external append: (array<'a>, 'a) => array<'a> = "concat"

    @get_index
    external getUnsafeOptionByString: (array<'a>, string) => option<'a> = ""

    @get_index
    external getUnsafeOption: (array<'a>, int) => option<'a> = ""

    @inline
    let has = (array, idx) => {
      array->Js.Array2.unsafe_get(idx)->(Obj.magic: 'a => bool)
    }

    let isArray = Js.Array2.isArray

    @send
    external map: (array<'a>, 'a => 'b) => array<'b> = "map"

    @val external fromArguments: array<'a> => array<'a> = "Array.from"
  }

  module Exn = {
    type error

    @new
    external makeError: string => error = "Error"

    let throwAny = (any: 'any): 'a => any->Obj.magic->throw

    let throwError: error => 'a = throwAny
  }

  module Int = {
    @inline
    let plus = (int1: int, int2: int): int => {
      (int1->Js.Int.toFloat +. int2->Js.Int.toFloat)->(Obj.magic: float => int)
    }

    external unsafeToString: int => string = "%identity"
    external unsafeToBool: int => bool = "%identity"
  }

  module String = {
    let capitalize = (string: string): string => {
      string->Js.String2.slice(~from=0, ~to_=1)->Js.String2.toUpperCase ++
        string->Js.String2.sliceToEnd(~from=1)
    }

    external unsafeToBool: string => bool = "%identity"
  }

  module Dict = {
    let copy: dict<'a> => dict<'a> = %raw(`(d) => ({...d})`)

    @val
    external mixin: (dict<'a>, dict<'a>) => dict<'a> = "Object.assign"

    @get_index
    external getUnsafeOption: (dict<'a>, string) => option<'a> = ""

    @set_index
    external setByInt: (dict<'a>, int, 'a) => unit = ""

    @get_index
    external getUnsafeOptionByInt: (dict<'a>, int) => option<'a> = ""

    @get_index
    external getUnsafeOptionBySymbol: (dict<'a>, Js.Types.symbol) => option<'a> = ""
  }

  module Float = {
    external unsafeToString: float => string = "%identity"
  }

  module Set = {
    type t<'a>

    @new
    external make: unit => t<'a> = "Set"

    // @new
    // external fromArray: array<'a> => t<'a> = "Set"

    @send external add: (t<'a>, 'a) => unit = "add"

    // @send external has: (t<'a>, 'a) => bool = "has"

    @val external toArray: t<'a> => array<'a> = "Array.from"
  }

  module Function = {
    @variadic @new
    external _make: array<string> => 'function = "Function"

    @inline
    let make2 = (~ctxVarName1, ~ctxVarValue1, ~ctxVarName2, ~ctxVarValue2, ~inlinedFunction) => {
      _make([ctxVarName1, ctxVarName2, `return ${inlinedFunction}`])(ctxVarValue1, ctxVarValue2)
    }

    external toExpression: 'a => 'a = "%unsafe_to_method"
  }

  module Symbol = {
    type t = Js.Types.symbol

    @val external make: string => t = "Symbol"
  }

  module Inlined = {
    module Value = {
      let fromString = (string: string): string => {
        let rec loop = idx => {
          switch string->Js.String2.get(idx)->(Obj.magic: string => option<string>) {
          | None => `"${string}"`
          | Some("\"") | Some("\n") => string->Js.Json.stringifyAny->Obj.magic
          | Some(_) => loop(idx + 1)
          }
        }
        loop(0)
      }
    }
  }
}

module Path = {
  type t = string

  external toString: t => string = "%identity"

  @inline
  let empty = ""

  @inline
  let dynamic = "[]"

  let toArray = path => {
    switch path {
    | "" => []
    | _ =>
      path
      ->Js.String2.split(`"]["`)
      ->Js.Array2.joinWith(`","`)
      ->Js.Json.parseExn
      ->(Obj.magic: Js.Json.t => array<string>)
    }
  }

  @inline
  let fromInlinedLocation = inlinedLocation => `[${inlinedLocation}]`

  @inline
  let fromLocation = location => `[${location->X.Inlined.Value.fromString}]`

  let fromArray = array => {
    switch array {
    | [] => ""
    | [location] => fromLocation(location)
    | _ => array->Js.Array2.map(fromLocation)->Js.Array2.joinWith("")
    }
  }

  let concat = (path, concatedPath) => path ++ concatedPath
}

let vendor = "sury"
// Internal symbol to easily identify the error
let s = X.Symbol.make(vendor)
// Internal symbol to identify item proxy
let itemSymbol = X.Symbol.make(vendor ++ ":item")

type tag =
  | @as("string") String
  | @as("number") Number
  | @as("bigint") BigInt
  | @as("boolean") Boolean
  | @as("symbol") Symbol
  | @as("null") Null
  | @as("undefined") Undefined
  | @as("nan") NaN
  | @as("function") Function
  | @as("instance") Instance
  | @as("array") Array
  | @as("object") Object
  | @as("union") Union
  | @as("never") Never
  | @as("unknown") Unknown
  | @as("ref") Ref

// Use variables to reduce bundle size with min+gzip
// Also as a good practice (ignore that we have tag variant 😅)
let stringTag: tag = %raw(`"string"`)
let numberTag: tag = %raw(`"number"`)
let bigintTag: tag = %raw(`"bigint"`)
let booleanTag: tag = %raw(`"boolean"`)
let symbolTag: tag = %raw(`"symbol"`)
let nullTag: tag = %raw(`"null"`)
let undefinedTag: tag = %raw(`"undefined"`)
let nanTag: tag = %raw(`"nan"`)
// let functionTag: tag = %raw(`"function"`)
let instanceTag: tag = %raw(`"instance"`)
let arrayTag: tag = %raw(`"array"`)
let objectTag: tag = %raw(`"object"`)
let unionTag: tag = %raw(`"union"`)
let neverTag: tag = %raw(`"never"`)
let unknownTag: tag = %raw(`"unknown"`)
let refTag: tag = %raw(`"ref"`)

type standard = {
  version: int,
  vendor: string,
  validate: 'any 'value. 'any => {"value": 'value},
}

type internalDefault = {}

type numberFormat = | @as("int32") Int32 | @as("port") Port
type stringFormat = | @as("json") JSON

type format = | ...numberFormat | ...stringFormat

@unboxed
type additionalItemsMode = | @as("strip") Strip | @as("strict") Strict

@tag("type")
type rec t<'value> =
  private
  | @as("never") Never({name?: string, title?: string, description?: string, deprecated?: bool})
  | @as("unknown")
  Unknown({
      name?: string,
      description?: string,
      title?: string,
      deprecated?: bool,
      examples?: array<unknown>,
      default?: unknown,
    })
  | @as("string")
  String({
      const?: string,
      format?: stringFormat,
      name?: string,
      title?: string,
      description?: string,
      deprecated?: bool,
      examples?: array<string>,
      default?: string,
    })
  | @as("number")
  Number({
      const?: float,
      format?: numberFormat,
      name?: string,
      title?: string,
      description?: string,
      deprecated?: bool,
      examples?: array<float>,
      default?: float,
    })
  | @as("bigint")
  BigInt({
      const?: bigint,
      name?: string,
      title?: string,
      description?: string,
      deprecated?: bool,
      examples?: array<bigint>,
      default?: bigint,
    })
  | @as("boolean")
  Boolean({
      const?: bool,
      name?: string,
      title?: string,
      description?: string,
      deprecated?: bool,
      examples?: array<bool>,
      default?: bool,
    })
  | @as("symbol")
  Symbol({
      const?: Js.Types.symbol,
      name?: string,
      title?: string,
      description?: string,
      deprecated?: bool,
      examples?: array<Js.Types.symbol>,
      default?: Js.Types.symbol,
    })
  | @as("null")
  Null({
      const: Js.Types.null_val,
      name?: string,
      title?: string,
      description?: string,
      deprecated?: bool,
    })
  | @as("undefined")
  Undefined({
      const: unit,
      name?: string,
      title?: string,
      description?: string,
      deprecated?: bool,
    })
  | @as("nan")
  NaN({
      const: float,
      name?: string,
      title?: string,
      description?: string,
      deprecated?: bool,
    })
  | @as("function")
  Function({
      const?: Js.Types.function_val,
      name?: string,
      title?: string,
      description?: string,
      deprecated?: bool,
      examples?: array<Js.Types.function_val>,
      default?: Js.Types.function_val,
    })
  | @as("instance")
  Instance({
      class: unknown,
      const?: Js.Types.obj_val,
      name?: string,
      title?: string,
      description?: string,
      deprecated?: bool,
      examples?: array<Js.Types.obj_val>,
      default?: Js.Types.obj_val,
    })
  | @as("array")
  Array({
      items: array<t<unknown>>,
      additionalItems: additionalItems,
      unnest?: bool,
      name?: string,
      title?: string,
      description?: string,
      deprecated?: bool,
      examples?: array<array<unknown>>,
      default?: array<unknown>,
    })
  | @as("object")
  Object({
      properties: dict<t<unknown>>,
      additionalItems: additionalItems,
      name?: string,
      title?: string,
      description?: string,
      deprecated?: bool,
      examples?: array<dict<unknown>>,
      default?: dict<unknown>,
    }) // TODO: Add const for Object and Tuple
  | @as("union")
  Union({
      anyOf: array<t<unknown>>,
      has: has,
      name?: string,
      title?: string,
      description?: string,
      deprecated?: bool,
      examples?: array<unknown>,
      default?: unknown,
    })
  | @as("ref")
  Ref({
      @as("$ref")
      ref: string,
    })
@unboxed and additionalItems = | ...additionalItemsMode | Schema(t<unknown>)
and schema<'a> = t<'a>
and internal = {
  @as("type")
  mutable tag: tag,
  // A serial number for the schema
  // to use for caching operations
  mutable seq?: float,
  // Builder for transforming to the "to" schema
  // If missing, should apply coercion logic
  mutable parser?: builder,
  // A field on the "to" schema,
  // to turn it into "parser", when reversing
  mutable serializer?: builder,
  // Logic for built-in decoding to the schema type
  mutable decoder: builder,
  // Logic for built-in encoding from the schema type
  mutable encoder?: builder,
  // A schema we transform to
  mutable to?: internal,
  // When transforming with changing shape,
  // store from which path it came from
  // For S.object, S.tuple, and S.shape
  mutable from?: array<string>,
  // The index of the flattened schema
  // reshaping is happening from
  mutable fromFlattened?: int,
  mutable flattened?: array<internal>,
  mutable const?: char, // use char to avoid Caml_option.some
  mutable class?: char, // use char to avoid Caml_option.some
  mutable name?: string,
  mutable title?: string,
  mutable description?: string,
  mutable deprecated?: bool,
  mutable examples?: array<unknown>,
  mutable default?: internalDefault,
  mutable fromDefault?: internalDefault,
  mutable format?: format,
  mutable has?: dict<bool>,
  mutable anyOf?: array<internal>,
  mutable additionalItems?: additionalItems,
  mutable items?: array<internal>,
  mutable properties?: dict<internal>,
  mutable noValidation?: bool,
  mutable unnest?: bool,
  mutable space?: int,
  @as("$ref")
  mutable ref?: string,
  @as("$defs")
  mutable defs?: dict<internal>,
  mutable isAsync?: bool, // Optional value means that it's not lazily computed yet.
  @as("~standard")
  mutable standard?: standard, // This is optional for convenience. The object added on make call
}
and meta<'value> = {
  name?: string,
  title?: string,
  description?: string,
  deprecated?: bool,
  examples?: array<'value>,
}
and untagged = private {
  @as("type")
  tag: tag,
  @as("$ref")
  ref?: string,
  @as("$defs")
  defs?: dict<t<unknown>>,
  const?: unknown,
  class?: unknown,
  format?: format,
  name?: string,
  title?: string,
  description?: string,
  deprecated?: bool,
  examples?: array<unknown>,
  default?: unknown,
  unnest?: bool,
  noValidation?: bool,
  items?: array<t<unknown>>,
  properties?: dict<t<unknown>>,
  additionalItems?: additionalItems,
  anyOf?: array<t<unknown>>,
  has?: dict<bool>,
  to?: t<unknown>,
}
and has = {
  string?: bool,
  number?: bool,
  never?: bool,
  unknown?: bool,
  bigint?: bool,
  boolean?: bool,
  symbol?: bool,
  null?: bool,
  undefined?: bool,
  nan?: bool,
  function?: bool,
  instance?: bool,
  array?: bool,
  object?: bool,
}
and builder = (~input: val, ~selfSchema: internal) => val
and val = {
  mutable from?: val,
  // We might have the same value, but different instances of the val object
  // Use the bond field, to connect the var call
  @as("b")
  mutable bond?: val,
  @as("p")
  mutable parent?: val,
  @as("v")
  mutable var: unit => string,
  @as("i")
  mutable inline: string,
  @as("f")
  mutable flag: flag,
  // The schema of the value that is being parsed
  @as("s")
  mutable schema: internal,
  // The schema of the value that we expect to parse into
  @as("e")
  mutable expected: internal,
  @as("t")
  mutable skipTo?: bool,
  @as("d")
  mutable vals?: dict<val>,
  @as("fv")
  mutable flattenedVals?: array<val>,
  @as("c")
  mutable code: string,
  @as("cb")
  mutable codeBeforeValidation?: string,
  @as("l")
  mutable varsAllocation: string,
  @as("a")
  mutable allocate: string => unit,
  mutable validation: option<(~inputVar: string, ~negative: bool) => string>,
  @as("u")
  mutable isUnion?: bool,
  mutable path: Path.t,
  @as("g")
  global: bGlobal,
}
and bGlobal = {
  @as("v")
  mutable varCounter: int,
  @as("o")
  mutable flag: int,
  @as("e")
  embeded: array<unknown>,
  @as("d")
  mutable defs?: dict<internal>,
}
and flag = int
and error = private {
  message: string,
  reason: string,
  path: Path.t,
  code: errorCode,
  flag: flag,
}
and errorCode =
  | OperationFailed(string)
  | InvalidOperation({description: string})
  | InvalidType({
      expected: schema<unknown>,
      // This is always set, kept it optional for backward compatibility when creating the values manually
      // FIXME: Remove optional
      received?: schema<unknown>,
      value: unknown,
      unionErrors?: array<error>,
    })
  | FailedTransformation({from: schema<unknown>, to: schema<unknown>, error: exn})
  | UnsupportedTransformation({from: schema<unknown>, to: schema<unknown>})
  | ExcessField(string)
  | UnexpectedAsync
  | InvalidJsonSchema(schema<unknown>)
@tag("success")
and jsResult<'value> = | @as(true) Success({value: 'value}) | @as(false) Failure({error: error})

type exn += private Error(error)

external castToUnknown: t<'any> => t<unknown> = "%identity"
external castToAny: t<'value> => t<'any> = "%identity"
external castToInternal: t<'any> => internal = "%identity"
external castToPublic: internal => t<'any> = "%identity"
external untag: t<'any> => untagged = "%identity"

// This is dirty
@inline
let isSchemaObject = obj => (obj->Obj.magic).standard->Obj.magic

let constField = "const"
let isLiteral = (schema: internal) => schema->Obj.magic->Dict.has(constField)

let isOptional = schema => {
  schema.tag === undefinedTag ||
    (schema.tag === unionTag &&
      schema.has->X.Option.getUnsafe->Dict.has((undefinedTag: tag :> string)))
}

module ValFlag = {
  @inline let none = 0
  @inline let async = 1
}

module Flag = {
  @inline let none = 0
  @inline let async = 1
  // @inline let flatten = 64

  external with: (flag, flag) => flag = "%orint"
  @inline
  let without = (flags, flag) => flags->with(flag)->Int.bitwiseXor(flag)

  let unsafeHas = (acc: flag, flag) => acc->Int.bitwiseAnd(flag)->(Obj.magic: int => bool)
  let has = (acc: flag, flag) => acc->Int.bitwiseAnd(flag) !== 0
}

module TagFlag = {
  @inline let unknown = 1
  @inline let string = 2
  @inline let number = 4
  @inline let boolean = 8
  @inline let undefined = 16
  @inline let null = 32
  @inline let object = 64
  @inline let array = 128
  @inline let union = 256
  @inline let ref = 512
  @inline let bigint = 1024
  @inline let nan = 2048
  @inline let function = 4096
  @inline let instance = 8192
  @inline let never = 16384
  @inline let symbol = 32768

  let flags = %raw(`{
    [unknownTag]: 1,
    [stringTag]: 2,
    [numberTag]: 4,
    [booleanTag]: 8,
    [undefinedTag]: 16,
    [nullTag]: 32,
    [objectTag]: 64,
    [arrayTag]: 128,
    [unionTag]: 256,
    [refTag]: 512,
    [bigintTag]: 1024,
    [nanTag]: 2048,
    ["function"]: 4096,
    [instanceTag]: 8192,
    [neverTag]: 16384,
    [symbolTag]: 32768,
  }`)

  @inline
  let get = (tag: tag) => flags->Js.Dict.unsafeGet((tag :> string))
}

let rec stringify = unknown => {
  let tagFlag = unknown->Type.typeof->(Obj.magic: Type.t => tag)->TagFlag.get

  if tagFlag->Flag.unsafeHas(TagFlag.undefined) {
    (undefinedTag :> string)
  } else if tagFlag->Flag.unsafeHas(TagFlag.object) {
    if unknown === %raw(`null`) {
      (nullTag :> string)
    } else if unknown->X.Array.isArray {
      let array = unknown->(Obj.magic: unknown => array<unknown>)
      let string = ref("[")
      for i in 0 to array->Array.length - 1 {
        if i !== 0 {
          string := string.contents ++ ", "
        }
        string := string.contents ++ array->Js.Array2.unsafe_get(i)->stringify
      }
      string.contents ++ "]"
    } else if (
      (unknown->(Obj.magic: 'a => {"constructor": unknown}))["constructor"] === %raw("Object")
    ) {
      let dict = unknown->(Obj.magic: unknown => dict<unknown>)
      let keys = Js.Dict.keys(dict)
      let string = ref("{ ")
      for i in 0 to keys->Array.length - 1 {
        let key = keys->Js.Array2.unsafe_get(i)
        let value = dict->Js.Dict.unsafeGet(key)
        string := `${string.contents}${key}: ${stringify(value)}; `
      }
      string.contents ++ "}"
    } else {
      unknown->Obj.magic->X.Object.internalClass
    }
  } else if tagFlag->Flag.unsafeHas(TagFlag.string) {
    `"${unknown->Obj.magic}"`
  } else if tagFlag->Flag.unsafeHas(TagFlag.bigint) {
    `${unknown->Obj.magic}n`
  } else if tagFlag->Flag.unsafeHas(TagFlag.function) {
    `Function`
  } else {
    (unknown->Obj.magic)["toString"]()
  }
}

let rec toExpression = schema => {
  let schema = schema->castToInternal
  switch schema {
  | {name} => name
  | {const} => const->Obj.magic->stringify

  | {anyOf} =>
    anyOf
    ->(Obj.magic: array<internal> => array<t<'a>>)
    ->Js.Array2.map(toExpression)
    ->Js.Array2.joinWith(" | ")
  | {format} => (format :> string)
  | {tag: Object, ?properties, ?additionalItems} =>
    let properties = properties->X.Option.getUnsafe
    let locations = properties->Js.Dict.keys
    if locations->Js.Array2.length === 0 {
      if additionalItems->Js.typeof === (objectTag :> string) {
        let additionalItems: internal = additionalItems->Obj.magic
        `{ [key: string]: ${additionalItems->castToPublic->toExpression}; }`
      } else {
        `{}`
      }
    } else {
      `{ ${locations
        ->Js.Array2.map(location => {
          `${location}: ${properties->Js.Dict.unsafeGet(location)->castToPublic->toExpression};`
        })
        ->Js.Array2.joinWith(" ")} }`
    }

  | {tag: NaN} => "NaN"
  // Case for val
  | {tag} if %raw(`schema.b`) => (tag :> string)
  | {tag: Array, ?items, ?additionalItems} =>
    let items = items->X.Option.getUnsafe
    if additionalItems->Js.typeof === (objectTag :> string) {
      let additionalItems: internal = additionalItems->Obj.magic
      let itemName = additionalItems->castToPublic->toExpression
      if (additionalItems.tag :> string) === (unionTag :> string) {
        `(${itemName})`
      } else {
        itemName
      } ++ "[]"
    } else {
      `[${items
        ->Js.Array2.map(schema => schema->castToPublic->toExpression)
        ->Js.Array2.joinWith(", ")}]`
    }
  | {tag: Instance, ?class} => (class->Obj.magic)["name"]
  | {tag} => (tag :> string)
  }
}

module InternalError = {
  %%raw(`
class SuryError extends Error {
  constructor(code, flag, path) {
    super();
    this.flag = flag;
    this.code = code;
    this.path = path;
  }
}

var d = Object.defineProperty, p = SuryError.prototype;
d(p, 'message', {
  get() {
      return message(this);
  },
})
d(p, 'reason', {
  get() {
      return reason(this);
  }
})
d(p, 'name', {value: 'SuryError'})
d(p, 's', {value: s})
d(p, '_1', {
  get() {
    return this
  },
});
d(p, 'RE_EXN_ID', {
  value: $$Error,
});

var seq = 1;
var Schema = function() {}, sp = Object.create(null);
d(sp, 'with', {
  get() {
    return (fn, ...args) => fn(this, ...args)
  },
});
// Also has ~standard below
Schema.prototype = sp;
`)

  @new
  external make: (~code: errorCode, ~flag: int, ~path: Path.t) => error = "SuryError"

  let getOrRethrow = (exn: exn) => {
    if %raw("exn&&exn.s===s") {
      exn->(Obj.magic: exn => error)
    } else {
      throw(exn)
    }
  }

  // TODO: Throw S.Error
  @inline
  let panic = message => X.Exn.throwError(X.Exn.makeError(`[Sury] ${message}`))

  let rec reason = (error: error, ~nestedLevel=0) => {
    switch error.code {
    | OperationFailed(reason) => reason
    | InvalidOperation({description}) => description
    | UnsupportedTransformation({from, to}) =>
      `Unsupported transformation from ${from->toExpression} to ${to->toExpression}`
    | UnexpectedAsync => "Encountered unexpected async transform or refine. Use parseAsyncOrThrow operation instead"
    | ExcessField(fieldName) => `Unrecognized key "${fieldName}"`
    | FailedTransformation(_) => {
        let text = %raw(`"" + reason$1.error`)
        if text->String.startsWith("Error: ") {
          text->String.slice(~start=7)
        } else {
          text
        }
      }
    | InvalidType({expected: schema, value, ?unionErrors}) =>
      let m = ref(`Expected ${schema->toExpression}, received ${value->stringify}`)
      switch unionErrors {
      | Some(errors) => {
          let lineBreak = `\n${" "->Js.String2.repeat(nestedLevel * 2)}`
          let reasonsDict = Js.Dict.empty()
          for idx in 0 to errors->Js.Array2.length - 1 {
            let error = errors->Js.Array2.unsafe_get(idx)
            let reason = error->reason(~nestedLevel=nestedLevel->X.Int.plus(1))
            let location = switch error.path {
            | "" => ""
            | nonEmptyPath => `At ${nonEmptyPath}: `
            }
            let line = `- ${location}${reason}`
            if reasonsDict->Js.Dict.unsafeGet(line)->X.Int.unsafeToBool->not {
              reasonsDict->Js.Dict.set(line, 1)
              m := m.contents ++ lineBreak ++ line
            }
          }
        }
      | None => ()
      }
      m.contents
    | InvalidJsonSchema(schema) => `${schema->toExpression} is not valid JSON`
    }
  }

  let reason = error => reason(error)

  let message = (error: error) => {
    `${switch error.path {
      | "" => ""
      | nonEmptyPath => `Failed at ${nonEmptyPath}: `
      }}${error->reason}`
  }
}

type globalConfig = {
  @as("m")
  message: error => string,
  @as("d")
  mutable defsAccumulator: option<dict<internal>>,
  @as("a")
  mutable defaultAdditionalItems: additionalItems,
  @as("n")
  mutable disableNanNumberValidation: bool,
}

type globalConfigOverride = {
  defaultAdditionalItems?: additionalItemsMode,
  disableNanNumberValidation?: bool,
}

let initialOnAdditionalItems: additionalItemsMode = Strip
let initialDisableNanNumberProtection = false
let globalConfig: globalConfig = {
  message: InternalError.message,
  defsAccumulator: None,
  defaultAdditionalItems: (initialOnAdditionalItems :> additionalItems),
  disableNanNumberValidation: initialDisableNanNumberProtection,
}

@new
external base: unit => internal = "Schema"

let base = tag => {
  let s = base()
  s.tag = tag
  s.seq = %raw(`seq++`)
  s
}

let shakenRef = "as"

let shakenTraps: X.Proxy.traps<internal> = {
  get: (~target, ~prop) => {
    switch target->Obj.magic->X.Dict.getUnsafeOption(shakenRef) {
    | Some(l) if prop !== shakenRef->Obj.magic =>
      InternalError.panic(
        `Schema S.${l} is not enabled. To start using it, add S.enable${l->X.String.capitalize}() at the project root.`,
      )
    | _ => target->Obj.magic->Js.Dict.unsafeGet(prop->Obj.magic)
    }
  },
}

let shaken = (apiName: string) => {
  let mut = base(neverTag)
  mut->Obj.magic->Js.Dict.set(shakenRef, apiName)
  mut->X.Proxy.make(shakenTraps)
}

let unknown = base(unknownTag)
let bool = base(booleanTag)
let symbol = base(symbolTag)
let string = base(stringTag)
let int = base(numberTag)
int.format = Some(Int32)
let float = base(numberTag)
let bigint = base(bigintTag)
let unit = base(undefinedTag)
unit.const = %raw(`void 0`)
let nullLiteral = base(nullTag)
nullLiteral.const = %raw(`null`)
let nan = base(nanTag)
nan.const = %raw(`NaN`)

type s<'value> = {
  schema: t<'value>,
  fail: 'a. (string, ~path: Path.t=?) => 'a,
}

let copySchema: internal => internal = %raw(`(schema) => {
  let c = new Schema()
  for (let k in schema) {
    c[k] = schema[k]
  }
  c.seq = seq++
  return c
}`)
let updateOutput = (schema: internal, fn): t<'value> => {
  let root = schema->copySchema
  let mut = ref(root)
  while mut.contents.to->Obj.magic {
    let next = mut.contents.to->X.Option.getUnsafe->copySchema
    mut.contents.to = Some(next)
    mut := next
  }
  // This should be the Output schema
  fn(mut.contents)
  root->castToPublic
}

module ErrorClass = {
  type t

  let value: t = %raw("SuryError")

  let constructor = InternalError.make
}

module Builder = {
  type t = builder

  let make = (Obj.magic: ((~input: val, ~selfSchema: internal) => val) => t)

  module B = {
    let eq = (~negative) => negative ? "!==" : "==="
    let and_ = (~negative) => negative ? "||" : "&&"
    let exp = (~negative) => negative ? "!" : ""
    let lt = (~negative) => negative ? ">" : "<"

    let embed = (b: val, value) => {
      let e = b.global.embeded
      let l = e->Js.Array2.length
      e->Js.Array2.unsafe_set(l, value->castAnyToUnknown)
      `e[${l->(Obj.magic: int => string)}]`
    }

    let inlineConst = (b, schema) => {
      let tagFlag = schema.tag->TagFlag.get
      let const = schema.const
      if tagFlag->Flag.unsafeHas(TagFlag.undefined) {
        "void 0"
      } else if tagFlag->Flag.unsafeHas(TagFlag.string) {
        const->Obj.magic->X.Inlined.Value.fromString
      } else if tagFlag->Flag.unsafeHas(TagFlag.bigint) {
        const->Obj.magic ++ "n"
      } else if (
        tagFlag->Flag.unsafeHas(
          TagFlag.symbol->Flag.with(TagFlag.function)->Flag.with(TagFlag.instance),
        )
      ) {
        b->embed(schema.const->Obj.magic)
      } else {
        const->Obj.magic
      }
    }

    // Escape it once per compiled operation.
    // Use bGlobal as cache, so we don't allocate another object + it's garbage collected.
    let inlineLocation = (global: bGlobal, location) => {
      let key = `"${location}"`
      switch global->(Obj.magic: bGlobal => dict<string>)->X.Dict.getUnsafeOption(key) {
      | Some(i) => i
      | None => {
          let inlinedLocation = location->X.Inlined.Value.fromString
          global->(Obj.magic: bGlobal => dict<string>)->Js.Dict.set(key, inlinedLocation)
          inlinedLocation
        }
      }
    }

    let secondAllocate = v => {
      let b = %raw(`this`)
      b.varsAllocation = b.varsAllocation ++ "," ++ v
    }

    let initialAllocate = v => {
      let b = %raw(`this`)
      b.varsAllocation = v
      b.allocate = secondAllocate
    }

    let _var = () => (%raw(`this`)).inline

    let _bondVar = () => {
      let val = %raw(`this`)
      let bond = val.bond->X.Option.getUnsafe
      let v = bond.var()
      val.inline = v
      val.var = _var
      v
    }

    let varWithoutAllocation = (global: bGlobal) => {
      let newCounter = global.varCounter->X.Int.plus(1)
      global.varCounter = newCounter
      `v${newCounter->X.Int.unsafeToString}`
    }

    let _notVarBeforeValidation = () => {
      let val = %raw(`this`)
      let v = val.global->varWithoutAllocation
      val.codeBeforeValidation = Some(`let ${v}=${val.inline};`)
      val.inline = v
      val.var = _var
      v
    }

    let _notVarAtParent = () => {
      let val = %raw(`this`)
      let v = val.global->varWithoutAllocation
      (val.parent->X.Option.getUnsafe).allocate(`${v}=${val.inline}`)
      val.var = _var
      val.inline = v
      v
    }

    let _notVar = () => {
      let val: val = %raw(`this`)
      let v = val.global->varWithoutAllocation
      let target = switch val.from {
      | Some(from) => from
      | None => val // FIXME: Validate that this never happens
      }
      switch val.inline {
      | "" => target.allocate(v)
      | i => target.allocate(`${v}=${i}`)
      }
      val.var = _var
      val.inline = v
      v
    }

    @inline
    let operationArgVar = "i"

    let operationArg = (~schema, ~expected, ~flag, ~defs): val => {
      {
        code: "",
        var: _var,
        inline: operationArgVar,
        allocate: initialAllocate,
        flag: ValFlag.none,
        schema,
        expected,
        varsAllocation: "",
        // TODO: Add global varsAllocation here
        // Set all the vars to the varsAllocation
        // Measure performance
        // TODO: Also try setting values to embed without allocation
        // (Is it memory leak?)
        validation: None,
        path: Path.empty,
        global: {
          ?defs,
          flag,
          embeded: [],
          varCounter: -1,
        },
      }
    }

    let throw = (b: val, ~code, ~path) => {
      X.Exn.throwAny(InternalError.make(~code, ~flag=b.global.flag, ~path))
    }

    let failWithArg = (b: val, fn: 'arg => errorCode, arg) => {
      `${b->embed(arg => {
          b->throw(~path=b.path, ~code=fn(arg))
        })}(${arg})`
    }

    let embedInvalidType = (~input: val, ~expected) => {
      let received = input.schema->castToPublic
      input->failWithArg(value => InvalidType({
        expected: expected->castToPublic,
        received,
        value,
      }), input.var())
    }

    // TODO: Remove this later
    let embedInvalidTypeWithVarOnly = (b, ~inputVar, ~expected) => {
      b->failWithArg(value => InvalidType({
        expected: expected->castToPublic,
        received: unknown->castToPublic,
        value,
      }), inputVar)
    }

    let merge = (val: val): string => {
      let current = ref(Some(val))
      let code = ref("")

      while current.contents !== None {
        let val = current.contents->X.Option.getUnsafe
        current := val.from

        let itemCode = ref("")

        switch val.validation {
        | Some(validation) => {
            let inputVar = val.var()
            let validationCode = validation(~inputVar, ~negative=true)
            itemCode :=
              `if(${validationCode}){${embedInvalidType(~input=val, ~expected=val.expected)}}`
          }
        | None => ()
        }

        if val.varsAllocation !== "" {
          itemCode := itemCode.contents ++ `let ${val.varsAllocation};`
        }

        // Delete allocate,
        // this is used to handle Val.var
        // linked to allocated scopes
        let _ = %raw(`delete val$1.a`)

        itemCode := itemCode.contents ++ val.code

        code :=
          switch val.codeBeforeValidation {
          | Some(codeBeforeValidation) => codeBeforeValidation
          | None => ""
          } ++
          itemCode.contents ++
          code.contents
      }

      code.contents
    }

    let refineInPlace = (val: val, ~schema, ~validation) => {
      // if val.from !== None {
      //   let inputVar = val.var()
      //   if val.varsAllocation !== "" {
      //     val.code = val.code ++ `let ${val.varsAllocation};`
      //     val.varsAllocation = ""
      //     val.allocate = initialAllocate
      //   }
      //   val.code =
      //     val.code ++
      //     `if(${validation(~inputVar, ~negative=true)}){${embedInvalidType(
      //         ~input=val,
      //         ~expected=val.expected,
      //       )}}`
      // } else {

      let prevValidation = val.validation
      val.validation = Some(
        (~inputVar, ~negative) => {
          {
            switch prevValidation {
            | Some(prevValidation) => prevValidation(~inputVar, ~negative) ++ and_(~negative)
            | None => ""
            }
          } ++
          validation(~inputVar, ~negative)
        },
      )

      // }
      val.schema = schema
    }

    let val = (from: val, initial: string, ~schema, ~expected=from.expected): val => {
      {
        from,
        var: _notVar,
        inline: initial,
        flag: ValFlag.none,
        schema,
        expected,
        code: "",
        varsAllocation: "",
        allocate: initialAllocate,
        validation: None,
        path: from.path,
        global: from.global,
      }
    }

    let dynamicScope = (from: val, ~locationVar): val => {
      let v =
        from->val(
          `${from.var()}[${locationVar}]`,
          ~schema=from.schema.additionalItems->(Obj.magic: option<additionalItems> => internal),
          ~expected=from.expected.additionalItems->(Obj.magic: option<additionalItems> => internal),
        )
      v.from = None
      v.parent = Some(from)
      v.path = Path.empty
      v.var = _notVarBeforeValidation
      v
    }

    let allocateVal = (from: val, ~schema, ~expected=from.expected): val => {
      let var = from.global->varWithoutAllocation
      from.code = from.code ++ `let ${var};`
      let v = from->val(var, ~schema, ~expected)
      v.var = _var
      v
    }

    let newConst = (from: val, ~schema): val => {
      from->val(from->inlineConst(schema), ~schema)
    }

    let asyncVal = (from: val, initial: string): val => {
      let v = from->val(initial, ~schema=unknown)
      v.flag = ValFlag.async
      v
    }

    module Val = {
      let copy = (val: val): val => {
        let new = val->Obj.magic->X.Dict.copy->Obj.magic
        if val.var !== _var {
          new.var = () => {
            let v = val.var()
            new.inline = v
            new.var = _var
            v
          }
        }
        new
      }

      module Object = {
        type t = {
          ...val,
          @as("j")
          mutable join: (string, string) => string,
          @as("ac")
          mutable asyncCount: int,
          @as("r")
          mutable promiseAllContent: string,
        }

        let objectJoin = (inlinedLocation, value) => {
          `${inlinedLocation}:${value},`
        }

        let arrayJoin = (_inlinedLocation, value) => {
          value ++ ","
        }

        let add = (objectVal: t, ~location, val: val) => {
          if objectVal.schema.tag === arrayTag {
            objectVal.schema.items->X.Option.getUnsafe->Stdlib.Array.push(val.schema)
          } else {
            objectVal.schema.properties->X.Option.getUnsafe->Stdlib.Dict.set(location, val.schema)
          }
          objectVal.code = objectVal.code ++ val->merge
          let inlinedLocation = objectVal.global->inlineLocation(location)
          objectVal.vals->X.Option.getUnsafe->Js.Dict.set(location, val)
          if val.flag->Flag.unsafeHas(ValFlag.async) {
            objectVal.promiseAllContent = objectVal.promiseAllContent ++ val.inline ++ ","
            objectVal.inline =
              objectVal.inline ++ objectVal.join(inlinedLocation, `a[${%raw(`objectVal.ac++`)}]`)
          } else {
            objectVal.inline = objectVal.inline ++ objectVal.join(inlinedLocation, val.inline)
          }
        }

        let merge = (target: t, vals: dict<val>) => {
          let locations = vals->Js.Dict.keys
          for idx in 0 to locations->Js.Array2.length - 1 {
            let location = locations->Js.Array2.unsafe_get(idx)
            target->add(~location, vals->Js.Dict.unsafeGet(location))
          }
        }

        let complete = (objectVal: t) => {
          objectVal.inline = objectVal.schema.tag === arrayTag
            ? "[" ++ objectVal.inline ++ "]"
            : "{" ++ objectVal.inline ++ "}"
          if objectVal.asyncCount->Obj.magic {
            objectVal.flag = objectVal.flag->Flag.with(ValFlag.async)
            objectVal.inline = `Promise.all([${objectVal.promiseAllContent}]).then(a=>(${objectVal.inline}))`
          }
          // FIXME: Test whether it's needed
          // objectVal.additionalItems = Some(Strict)
          (objectVal :> val)
        }
      }

      @inline
      let var = (val: val) => {
        val.var()
      }

      let addKey = (objVal: val, key, value: val) => {
        `${objVal.var()}[${key}]=${value.inline}`
      }

      let set = (b: val, input: val, val) => {
        if input === val {
          ""
        } else {
          // FIXME: Remove original ValFlag
          let inputVar = b->var
          switch (
            input.flag->Flag.unsafeHas(ValFlag.async),
            val.flag->Flag.unsafeHas(ValFlag.async),
          ) {
          | (false, true) => {
              input.flag = input.flag->Flag.with(ValFlag.async)
              `${inputVar}=${val.inline}`
            }
          | (false, false)
          | (true, true) =>
            `${inputVar}=${val.inline}`
          | (true, false) => `${inputVar}=Promise.resolve(${val.inline})`
          }
        }
      }

      let cleanValFrom = (val: val) => {
        {
          ...val,
          var: val.var === _var ? _var : _bondVar,
          bond: val,
          from: ?None,
          code: "",
          codeBeforeValidation: ?None,
          isUnion: false,
          varsAllocation: "",
          allocate: initialAllocate,
          validation: None,
        }
      }

      let get = (parent: val, location) => {
        let vals = switch parent.vals {
        | Some(d) => d
        | None => {
            let d = Js.Dict.empty()
            parent.vals = Some(d)
            d
          }
        }

        switch vals->X.Dict.getUnsafeOption(location) {
        | Some(v) => v->cleanValFrom
        | None => {
            let locationSchema = if parent.schema.tag === objectTag {
              parent.schema.properties->X.Option.getUnsafe->X.Dict.getUnsafeOption(location)
            } else {
              parent.schema.items
              ->X.Option.getUnsafe
              ->X.Array.getUnsafeOptionByString(location)
            }
            let schema = switch locationSchema {
            | Some(s) => s
            | None =>
              switch parent.schema.additionalItems->X.Option.getUnsafe {
              | Schema(s) => s->castToInternal
              | _ => InternalError.panic("The schema doesn't have additional items")
              }
            }

            let pathAppend = Path.fromInlinedLocation(parent.global->inlineLocation(location))
            let item = parent->val(
              if schema->isLiteral {
                parent->inlineConst(schema)
              } else {
                `${parent->var}${pathAppend}`
              },
              ~schema,
            )
            item.from = None
            item.parent = Some(parent)
            item.path = parent.path->Path.concat(pathAppend)
            item.var = _notVarAtParent
            vals->Js.Dict.set(location, item)
            item
          }
        }
      }

      let setInlined = (b: val, input: val, inlined) => {
        `${b->var}=${inlined}`
      }

      let map = (from: val, inlinedFn) => {
        from->val(`${inlinedFn}(${from.inline})`, ~schema=unknown)
      }
    }

    @inline
    let isInternalError = (_b: val, var) => {
      `${var}&&${var}.s===s`
    }

    let transform = (b: val, ~input: val, operation) => {
      if input.flag->Flag.unsafeHas(ValFlag.async) {
        // let bb = b->scope(~path=b.path)
        let bb = b
        let operationInput: val = {
          // FIXME: This is probably wrong
          ...b,
          var: _var,
          inline: bb.global->varWithoutAllocation,
          flag: ValFlag.none,
          schema: unknown, // FIXME:
        }
        let operationOutputVal = operation(bb, ~input=operationInput)
        let operationCode = bb->merge

        input->asyncVal(
          `${input.inline}.then(${Val.var(
              operationInput,
            )}=>{${operationCode}return ${operationOutputVal.inline}})`,
        )
      } else {
        operation(b, ~input)
      }
    }

    let embedSyncOperation = (~input: val, ~fn: 'input => 'output) => {
      if input.flag->Flag.unsafeHas(ValFlag.async) {
        input->asyncVal(`${input.inline}.then(${input->embed(fn)})`)
      } else {
        input->Val.map(input->embed(fn))
      }
    }

    let embedAsyncOperation = (~input, ~fn: 'input => promise<'output>) => {
      if !(input.global.flag->Flag.unsafeHas(Flag.async)) {
        input->throw(~code=UnexpectedAsync, ~path=Path.empty)
      }
      let val = embedSyncOperation(~input, ~fn)
      val.flag = val.flag->Flag.with(ValFlag.async)
      val
    }

    let fail = (b: val, ~message) => {
      `${b->embed(() => {
          b->throw(~path=b.path, ~code=OperationFailed(message))
        })}()`
    }

    let effectCtx = (b, ~selfSchema) => {
      schema: selfSchema->castToPublic,
      fail: (message, ~path as customPath=Path.empty) => {
        b->throw(~path=b.path->Path.concat(customPath), ~code=OperationFailed(message))
      },
    }

    let invalidOperation = (val: val, ~description) => {
      val->throw(~path=val.path, ~code=InvalidOperation({description: description}))
    }

    let mergeWithCatch = (val: val, ~catch, ~appendSafe=?) => {
      let valCode = val->merge
      if (
        valCode === "" &&
          // FIXME: Instead of this wrap all S.transform in a try/catch
          !(val.flag->Flag.unsafeHas(ValFlag.async))
      ) {
        valCode ++
        switch appendSafe {
        | Some(append) => append()
        | None => ""
        }
      } else {
        let errorVar = val.global->varWithoutAllocation

        let catchCode = `if(${val->isInternalError(errorVar)}){${catch(
            ~errorVar,
          )}}throw ${errorVar}`

        if val.flag->Flag.unsafeHas(ValFlag.async) {
          val.inline = `${val.inline}.catch(${errorVar}=>{${catchCode}})`
        }
        `try{${valCode}${switch appendSafe {
          | Some(append) => append()
          | None => ""
          }}}catch(${errorVar}){${catchCode}}`
      }
    }

    let mergeWithPathPrepend = (val: val, ~locationVar=?, ~appendSafe=?) => {
      if val.path === Path.empty && locationVar === None {
        val->merge
      } else {
        val->mergeWithCatch(~appendSafe?, ~catch=(~errorVar) => {
          `${errorVar}.path=${switch val.parent {
            | None
            | Some({path: ""}) => ""
            | Some({path}) => `${path->X.Inlined.Value.fromString}+`
            }}${switch locationVar {
            | Some(var) => `'["'+${var}+'"]'+`
            | _ => ""
            }}${errorVar}.path`
        })
      }
    }

    let withPathPrepend = (
      b: val,
      ~input,
      ~dynamicLocationVar as maybeDynamicLocationVar=?,
      ~appendSafe=?,
      fn,
    ) => {
      if b.path === Path.empty && maybeDynamicLocationVar === None {
        fn(b, ~input)
      } else {
        fn(b, ~input)

        // try b->mergeWithCatch(
        //   ~path=Path.empty,
        //   ~input,
        //   ~catch=(b, ~errorVar) => {
        // b.code = `${errorVar}.path=${b.path->X.Inlined.Value.fromString}+${switch maybeDynamicLocationVar {
        //   | Some(var) => `'["'+${var}+'"]'+`
        //   | _ => ""
        //   }}${errorVar}.path`
        //     None
        //   },
        //   ~appendSafe?,
        //   b => {
        //     fn(b, ~input)
        //   },
        // ) catch {
        // | _ =>
        //   let error = %raw(`exn`)->InternalError.getOrRethrow
        //   X.Exn.throwAny(
        //     InternalError.make(
        //       ~path=b.path->Path.concat(Path.dynamic)->Path.concat(error.path),
        //       ~code=error.code,
        //       ~flag=error.flag,
        //     ),
        //   )
        // }
      }
    }

    let unsupportedTransform = (b, ~from, ~target) => {
      b->throw(
        ~code=UnsupportedTransformation({
          from: from->castToPublic,
          to: target->castToPublic,
        }),
        ~path=b.path,
      )
    }
  }

  let noopOperation = i => i->Obj.magic
}
// TODO: Split validation code and transformation code
module B = Builder.B

let inputToString = (input: val) => {
  input->B.val(`""+${input.inline}`, ~schema=string)
}

let numberDecoder = Builder.make((~input, ~selfSchema) => {
  let inputTagFlag = input.schema.tag->TagFlag.get
  if inputTagFlag->Flag.unsafeHas(TagFlag.unknown) {
    input->B.refineInPlace(~schema=selfSchema, ~validation=(~inputVar, ~negative) => {
      `typeof ${inputVar}${B.eq(~negative)}"${(numberTag :> string)}"` ++
      {
        switch selfSchema.format {
        | Some(Int32) =>
          `${B.and_(~negative)}${inputVar}${B.lt(~negative)}2147483647${B.and_(
              ~negative,
            )}${inputVar}${B.lt(~negative=!negative)}-2147483648${B.and_(
              ~negative,
            )}${inputVar}%1${B.eq(~negative)}0`

        | _ =>
          if globalConfig.disableNanNumberValidation {
            ""
          } else {
            `${B.and_(~negative)}${B.exp(~negative=!negative)}Number.isNaN(${inputVar})`
          }
        }
      }
    })
    input
  } else if inputTagFlag->Flag.unsafeHas(TagFlag.string) {
    // This weird solution is needed only to make sure that
    // received value in error message is input string instead of NaN
    let tmpInput = input->B.Val.cleanValFrom
    tmpInput.from = Some(input)

    let outputVar = input.global->B.varWithoutAllocation
    input.allocate(`${outputVar}=+${input.var()}`)
    let output = tmpInput->B.val(outputVar, ~schema=selfSchema)
    output.var = B._var

    // TODO: Reuse logic for unknown case
    tmpInput.validation = Some(
      (~inputVar as _, ~negative) => {
        switch selfSchema.format {
        | Some(Int32) =>
          `${outputVar}${B.lt(~negative)}2147483647${B.and_(~negative)}${outputVar}${B.lt(
              ~negative=!negative,
            )}-2147483648${B.and_(~negative)}${outputVar}%1${B.eq(~negative)}0`

        | _ => `${B.exp(~negative=!negative)}Number.isNaN(${outputVar})`
        }
      },
    )
    output
  } else if !(inputTagFlag->Flag.unsafeHas(TagFlag.number)) {
    input->B.unsupportedTransform(~from=input.schema, ~target=selfSchema)
  } else {
    input
  }
})

float.decoder = numberDecoder
int.decoder = numberDecoder

let stringDecoder = Builder.make((~input, ~selfSchema) => {
  let inputTagFlag = input.schema.tag->TagFlag.get
  if inputTagFlag->Flag.unsafeHas(TagFlag.unknown) {
    input->B.refineInPlace(~schema=string, ~validation=(~inputVar, ~negative) => {
      `typeof ${inputVar}${B.eq(~negative)}"${(stringTag :> string)}"`
    })
    input
  } else if (
    inputTagFlag->Flag.unsafeHas(
      TagFlag.boolean->Flag.with(
        TagFlag.number->Flag.with(
          TagFlag.bigint->Flag.with(
            TagFlag.undefined->Flag.with(TagFlag.null->Flag.with(TagFlag.nan)),
          ),
        ),
      ),
    ) && input.schema->isLiteral
  ) {
    let const = %raw(`""+input.s.const`)
    let schema = base(stringTag)
    schema.const = const->Obj.magic
    input->B.val(`"${const}"`, ~schema)
  } else if (
    inputTagFlag->Flag.unsafeHas(
      TagFlag.boolean->Flag.with(TagFlag.number->Flag.with(TagFlag.bigint)),
    )
  ) {
    input->inputToString
  } else if !(inputTagFlag->Flag.unsafeHas(TagFlag.string)) {
    input->B.unsupportedTransform(~from=input.schema, ~target=selfSchema)
  } else {
    input
  }
})

string.decoder = stringDecoder

let booleanDecoder = Builder.make((~input, ~selfSchema) => {
  let inputTagFlag = input.schema.tag->TagFlag.get
  if inputTagFlag->Flag.unsafeHas(TagFlag.unknown) {
    input->B.refineInPlace(~schema=selfSchema, ~validation=(~inputVar, ~negative) => {
      `typeof ${inputVar}${B.eq(~negative)}"${(booleanTag :> string)}"`
    })
    input
  } else if inputTagFlag->Flag.unsafeHas(TagFlag.string) {
    let output = input->B.allocateVal(~schema=selfSchema)
    let inputVar = input.var()
    output.code = `(${output.inline}=${inputVar}==="true")||${inputVar}==="false"||${B.embedInvalidType(
        ~input,
        ~expected=selfSchema,
      )};`
    output
  } else if !(inputTagFlag->Flag.unsafeHas(TagFlag.boolean)) {
    input->B.unsupportedTransform(~from=input.schema, ~target=selfSchema)
  } else {
    input
  }
})

bool.decoder = booleanDecoder

let bigintDecoder = Builder.make((~input, ~selfSchema) => {
  let inputTagFlag = input.schema.tag->TagFlag.get

  if inputTagFlag->Flag.unsafeHas(TagFlag.unknown) {
    input->B.refineInPlace(~schema=selfSchema, ~validation=(~inputVar, ~negative) => {
      `typeof ${inputVar}${B.eq(~negative)}"${(bigintTag :> string)}"`
    })
    input
  } // TODO: Skip formats which 100% don't match
  else if inputTagFlag->Flag.unsafeHas(TagFlag.string) {
    let output = input->B.allocateVal(~schema=selfSchema)
    let inputVar = input.var()
    output.code = `try{${output.inline}=BigInt(${inputVar})}catch(_){${B.embedInvalidType(
        ~input,
        ~expected=selfSchema,
      )}}`
    output
  } else if inputTagFlag->Flag.unsafeHas(TagFlag.number) {
    input->B.val(`BigInt(${input.inline})`, ~schema=selfSchema)
  } else if !(inputTagFlag->Flag.unsafeHas(TagFlag.bigint)) {
    input->B.unsupportedTransform(~from=input.schema, ~target=selfSchema)
  } else {
    input
  }
})

bigint.decoder = bigintDecoder

let setHas = (has, tag: tag) => {
  has->Js.Dict.set(
    tag->TagFlag.get->Flag.unsafeHas(TagFlag.union->Flag.with(TagFlag.ref))
      ? (unknownTag: tag :> string)
      : (tag: tag :> string),
    true,
  )
}

let jsonName = `JSON`

let jsonString = shaken("jsonString")

let jsonStringWithSpace = (space: int) => {
  let mut = jsonString->copySchema
  mut.space = Some(space)
  mut->castToPublic
}

let json = shaken("json")

module Literal = {
  let literalDecoder = Builder.make((~input, ~selfSchema) => {
    if selfSchema.noValidation->X.Option.getUnsafe {
      input->B.newConst(~schema=selfSchema)
    } else if input.schema->isLiteral {
      // FIXME: test NaN case
      if input.schema.const === selfSchema.const {
        input
      } else {
        input->B.newConst(~schema=selfSchema)
      }
    } else {
      let schemaTagFlag = selfSchema.tag->TagFlag.get

      if (
        input.schema.tag->TagFlag.get->Flag.unsafeHas(TagFlag.string) &&
          schemaTagFlag->Flag.unsafeHas(
            TagFlag.boolean->Flag.with(
              TagFlag.number->Flag.with(
                TagFlag.bigint->Flag.with(
                  TagFlag.undefined->Flag.with(TagFlag.null->Flag.with(TagFlag.nan)),
                ),
              ),
            ),
          )
      ) {
        // FIXME: Test, that when from item has a refinement
        // and we need to keep existing validation
        // S.string->S.check->S.to(S.literal(false))
        input.validation = Some(
          (~inputVar, ~negative) => {
            let inlinedConst = `"${selfSchema.const->Obj.magic}"`
            input.expected.name = Some(inlinedConst)
            `${inputVar}${B.eq(~negative)}${inlinedConst}`
          },
        )
        input->B.newConst(~schema=selfSchema)
      } else if schemaTagFlag->Flag.unsafeHas(TagFlag.nan) {
        input->B.refineInPlace(~schema=selfSchema, ~validation=(~inputVar, ~negative) => {
          `${B.exp(~negative)}Number.isNaN(${inputVar})`
        })
        input
      } else {
        // TODO: Determine impossible cases during compilation
        input.validation = Some(
          (~inputVar, ~negative) => {
            input.expected = selfSchema
            `${inputVar}${B.eq(~negative)}${input->B.inlineConst(selfSchema)}`
          },
        )
        input.schema = selfSchema
        input
      }
    }
  })

  nullLiteral.decoder = literalDecoder
  unit.decoder = literalDecoder
  nan.decoder = literalDecoder

  let parse = (value): internal => {
    let value = value->castAnyToUnknown
    if value === %raw(`null`) {
      nullLiteral
    } else {
      switch value->Type.typeof {
      | #undefined => unit
      | #number if value->(Obj.magic: unknown => float)->Js.Float.isNaN => nan
      | #object => {
          let s = base(instanceTag)
          s.class = (value->Obj.magic)["constructor"]
          s.const = value->Obj.magic
          s.decoder = literalDecoder
          s
        }
      | typeof => {
          let s = base(typeof->(Obj.magic: Type.t => tag))
          s.const = value->Obj.magic
          s.decoder = literalDecoder
          s
        }
      }
    }
  }
}

let rec parse = (input: val) => {
  let expected = input.expected

  if input.expected.defs->Obj.magic {
    input.global.defs = input.expected.defs
  }

  let output = ref(
    switch input.schema.encoder {
    | Some(encoder) => encoder(~input, ~selfSchema=expected)
    | None => input
    },
  )

  if output.contents.skipTo !== Some(true) {
    output := expected.decoder(~input=output.contents, ~selfSchema=expected)

    switch output.contents.expected.to {
    | Some(to) =>
      switch output.contents.expected {
      | {parser} => output := parser(~input=output.contents, ~selfSchema=output.contents.expected)
      | _ => ()
      }

      if output.contents.skipTo !== Some(true) {
        let next = output.contents->B.Val.cleanValFrom
        next.from = Some(output.contents)
        next.expected = to
        output := parse(next)
      }

    | None => ()
    }
  }

  output.contents
}
and parseDynamic = input => {
  try input->parse catch {
  | _ =>
    let error = %raw(`exn`)->InternalError.getOrRethrow
    X.Exn.throwAny(
      InternalError.make(
        ~path={
          // For the case parent must always be present
          switch input.parent {
          | Some(p) => p.path
          | None => Path.empty
          }->Path.concat(input.path->Path.concat(Path.dynamic)->Path.concat(error.path))
        },
        ~code=error.code,
        ~flag=error.flag,
      ),
    )
  }
}
and isAsyncInternal = (schema, ~defs) => {
  try {
    let input = B.operationArg(~flag=Flag.async, ~defs, ~schema=unknown, ~expected=schema)
    let output = input->parse
    let isAsync = output.flag->Flag.has(ValFlag.async)
    schema.isAsync = Some(isAsync)
    isAsync
  } catch {
  | _ => {
      let _ = %raw(`exn`)->InternalError.getOrRethrow
      false
    }
  }
}
and compileDecoder = (~schema, ~expected, ~flag, ~defs) => {
  let input = B.operationArg(
    ~flag,
    ~defs,
    ~schema=if schema->isLiteral {
      unknown
    } else {
      schema
    },
    ~expected,
  )

  let output = input->parse
  let code = output->B.merge

  let isAsync = output.flag->Flag.has(ValFlag.async)
  schema.isAsync = Some(isAsync)

  if code === "" && output === input && !(flag->Flag.unsafeHas(Flag.async)) {
    Builder.noopOperation
  } else {
    let inlinedOutput = ref(output.inline)
    if flag->Flag.unsafeHas(Flag.async) && !isAsync && !(defs->Obj.magic) {
      inlinedOutput := `Promise.resolve(${inlinedOutput.contents})`
    }

    let inlinedFunction = `${B.operationArgVar}=>{${code}return ${inlinedOutput.contents}}`

    // Js.log(inlinedFunction)

    X.Function.make2(
      ~ctxVarName1="e",
      ~ctxVarValue1=input.global.embeded,
      ~ctxVarName2="s",
      ~ctxVarValue2=s,
      ~inlinedFunction,
    )
  }
}
and valueOptions = Js.Dict.empty()
and valKey = "value"
and reverseKey = "r"
and getOutputSchema = (schema: internal) => {
  switch schema.to {
  | Some(to) => getOutputSchema(to)
  | None => schema
  }
}
// FIXME: Define it as a schema property
and reverse = (schema: internal) => {
  if schema->Obj.magic->Stdlib.Dict.has(reverseKey)->Obj.magic {
    schema->Obj.magic->Stdlib.Dict.getUnsafe(reverseKey)->Obj.magic
  } else {
    let reversedHead = ref(None)
    let current = ref(Some(schema))

    while current.contents->Obj.magic {
      let mut = current.contents->X.Option.getUnsafe->copySchema
      let next = mut.to
      switch reversedHead.contents {
      | None => %raw(`delete mut.to`)
      | Some(to) => mut.to = Some(to)
      }
      let parser = mut.parser
      switch mut.serializer {
      | Some(serializer) => mut.parser = Some(serializer)
      | None => %raw(`delete mut.parser`)
      }
      switch parser {
      | Some(parser) => mut.serializer = Some(parser)
      | None => %raw(`delete mut.serializer`)
      }
      let fromDefault = mut.fromDefault
      switch mut.default {
      | Some(default) => mut.fromDefault = Some(default)
      | None => %raw(`delete mut.fromDefault`)
      }
      switch fromDefault {
      | Some(fromDefault) => mut.default = Some(fromDefault)
      | None => %raw(`delete mut.default`)
      }
      switch mut.items {
      | Some(items) =>
        let newItems = Belt.Array.makeUninitializedUnsafe(items->Js.Array2.length)
        for idx in 0 to items->Js.Array2.length - 1 {
          newItems->Js.Array2.unsafe_set(idx, items->Js.Array2.unsafe_get(idx)->reverse)
        }
        mut.items = Some(newItems)

      | None => ()
      }
      switch mut.properties {
      | Some(properties) => {
          let newProperties = Js.Dict.empty()
          let keys = properties->Js.Dict.keys
          for idx in 0 to keys->Js.Array2.length - 1 {
            let key = keys->Js.Array2.unsafe_get(idx)
            newProperties->Js.Dict.set(key, properties->Js.Dict.unsafeGet(key)->reverse)
          }
          mut.properties = Some(newProperties)
        }
      // Skip tuple
      | None => ()
      }
      if mut.additionalItems->Type.typeof === #object {
        mut.additionalItems = Some(
          Schema(
            mut.additionalItems
            ->(Obj.magic: option<additionalItems> => internal)
            ->reverse
            ->castToPublic,
          ),
        )
      }
      switch mut.anyOf {
      | Some(anyOf) =>
        let has = Js.Dict.empty()
        let newAnyOf = []
        for idx in 0 to anyOf->Js.Array2.length - 1 {
          let s = anyOf->Js.Array2.unsafe_get(idx)
          let reversed = s->reverse
          newAnyOf->Js.Array2.push(reversed)->ignore
          has->setHas(reversed.tag)
        }
        mut.has = Some(has)
        mut.anyOf = Some(newAnyOf)
      | None => ()
      }
      switch mut.defs {
      | Some(defs) => {
          let reversedDefs = Js.Dict.empty()
          for idx in 0 to defs->Js.Dict.keys->Js.Array2.length - 1 {
            let key = defs->Js.Dict.keys->Js.Array2.unsafe_get(idx)
            reversedDefs->Js.Dict.set(key, defs->Js.Dict.unsafeGet(key)->reverse)
          }
          mut.defs = Some(reversedDefs)
        }
      | None => ()
      }
      reversedHead := Some(mut)
      current := next
    }

    // Use defineProperty even though it's slower
    // but it improves logging experience a lot
    // for some reason Wallaby still shows the property
    let r = reversedHead.contents->X.Option.getUnsafe
    valueOptions->Js.Dict.set(valKey, r->Obj.magic)
    let _ = X.Object.defineProperty(schema, reverseKey, valueOptions->Obj.magic)
    valueOptions->Js.Dict.set(valKey, schema->Obj.magic)
    let _ = X.Object.defineProperty(r, reverseKey, valueOptions->Obj.magic)
    r
  }
}

let getDecoder = (~s1 as _, ~flag as _=?) => {
  let args = %raw(`arguments`)
  let idx = ref(0)
  let flag = ref(None)
  let keyRef = ref("")
  let maxSeq = ref(0.)
  let cacheTarget = ref(None)

  while flag.contents === None {
    let arg = args->Js.Array2.unsafe_get(idx.contents)
    if !(arg->Obj.magic) {
      flag := Some(0)
      keyRef := keyRef.contents ++ "-0"
    } else if Js.typeof(arg->Obj.magic) === "number" {
      flag := Some(arg->Obj.magic)
      keyRef := keyRef.contents ++ "-" ++ arg->Obj.magic
    } else {
      let schema: internal = arg->Obj.magic
      let seq: float = schema.seq->Obj.magic
      if seq > maxSeq.contents {
        maxSeq := seq
        cacheTarget := Some(schema)
      }
      keyRef := keyRef.contents ++ seq->Obj.magic ++ "-"
      idx := idx.contents + 1
    }
  }

  switch cacheTarget.contents {
  | None => InternalError.panic("No schema provided for decoder.")
  | Some(cacheTarget) => {
      let key = keyRef.contents
      if cacheTarget->Obj.magic->Stdlib.Dict.has(key) {
        Js.log(key)
        cacheTarget->Obj.magic->Stdlib.Dict.getUnsafe(key)->Obj.magic
      } else {
        let schema = ref(args->Js.Array2.unsafe_get(idx.contents - 1))
        for i in idx.contents - 2 downto 0 {
          let to = schema.contents
          schema :=
            args
            ->Js.Array2.unsafe_get(i)
            ->updateOutput(mut => {
              mut.to = Some(to)
            })
            ->castToInternal
        }
        let f = compileDecoder(
          ~schema=schema.contents,
          ~expected=schema.contents,
          ~flag=flag.contents->X.Option.getUnsafe,
          ~defs=%raw(`0`),
        )
        // Reusing the same object makes it a little bit faster
        valueOptions->Js.Dict.set(valKey, f)
        // Use defineProperty, so the cache keys are not enumerable
        let _ = X.Object.defineProperty(cacheTarget, key, valueOptions->Obj.magic)
        f->(Obj.magic: (unknown => unknown) => 'from => 'to)
      }
    }
  }
}

@val
external getDecoder2: (~s1: internal, ~s2: internal, ~flag: flag=?) => 'a => 'b = "getDecoder"

@val
external getDecoder3: (~s1: internal, ~s2: internal, ~s3: internal, ~flag: flag=?) => 'a => 'b =
  "getDecoder"

let rec makeObjectVal = (from: val, ~schema): B.Val.Object.t => {
  {
    from,
    var: B._notVar,
    inline: "",
    flag: ValFlag.none,
    join: schema.tag === arrayTag ? B.Val.Object.arrayJoin : B.Val.Object.objectJoin,
    promiseAllContent: "",
    schema: schema.tag === arrayTag
      ? {
          tag: arrayTag,
          items: [],
          additionalItems: Strict,
          decoder: arrayDecoder,
        }
      : {
          {
            tag: objectTag,
            properties: Js.Dict.empty(),
            additionalItems: Strict,
            decoder: objectDecoder,
          }
        },
    expected: from.expected,
    vals: Js.Dict.empty(),
    code: "",
    varsAllocation: "",
    asyncCount: 0,
    allocate: B.initialAllocate,
    validation: None,
    path: from.path,
    global: from.global,
  }
}
and array = item => {
  let mut = base(arrayTag)
  mut.additionalItems = Some(Schema(item->castToInternal->castToPublic))
  mut.items = Some(X.Array.immutableEmpty)
  mut.decoder = arrayDecoder
  mut->castToPublic
}
and arrayDecoder: builder = (~input, ~selfSchema) => {
  let inputTagFlag = input.schema.tag->TagFlag.get
  let items = selfSchema.items->X.Option.getUnsafe
  let length = items->Js.Array2.length

  let input = ref(input)

  if inputTagFlag->Flag.unsafeHas(TagFlag.unknown->Flag.with(TagFlag.array)) {
    let isArrayInput = inputTagFlag->Flag.unsafeHas(TagFlag.array)
    if !isArrayInput {
      input.contents->B.refineInPlace(
        ~schema=array(unknown->castToPublic)->castToInternal,
        ~validation=(~inputVar, ~negative) => `${B.exp(~negative)}Array.isArray(${inputVar})`,
      )
    }
    let isExactSize = switch input.contents.schema.additionalItems->X.Option.getUnsafe {
    | Schema(_) => false
    | _ => input.contents.schema.items->X.Option.getUnsafe->Js.Array2.length === length
    }

    if !isExactSize {
      switch selfSchema.additionalItems->X.Option.getUnsafe {
      | Strict =>
        input.contents->B.refineInPlace(~schema=input.contents.schema, ~validation=(
          ~inputVar,
          ~negative,
        ) => `${inputVar}.length${B.eq(~negative)}${length->X.Int.unsafeToString}`)
      | Strip =>
        input.contents->B.refineInPlace(~schema=input.contents.schema, ~validation=(
          ~inputVar,
          ~negative,
        ) => `${inputVar}.length${B.lt(~negative=!negative)}${length->X.Int.unsafeToString}`)

      | _ => ()
      }
    }
  } else {
    input.contents->B.unsupportedTransform(~from=input.contents.schema, ~target=selfSchema)
  }

  let input = input.contents

  switch selfSchema.additionalItems->X.Option.getUnsafe {
  | Schema(itemSchema) => {
      let itemSchema = itemSchema->castToInternal
      if itemSchema === unknown {
        input
      } else {
        let inputVar = input->B.Val.var
        let iteratorVar = input.global->B.varWithoutAllocation

        let itemInput = input->B.dynamicScope(~locationVar=iteratorVar)
        let itemOutput = itemInput->parseDynamic
        let isTransformed = itemInput !== itemOutput
        let output = isTransformed
          ? input->B.val(`new Array(${inputVar}.length)`, ~schema=selfSchema)
          : input // FIXME: schema
        output.schema = selfSchema

        let itemCode =
          itemOutput->B.mergeWithPathPrepend(
            ~locationVar=iteratorVar,
            ~appendSafe=?isTransformed
              ? Some(() => output->B.Val.addKey(iteratorVar, itemOutput))
              : None,
          )

        if isTransformed || itemCode !== "" {
          output.code =
            output.code ++
            `for(let ${iteratorVar}=${length->X.Int.unsafeToString};${iteratorVar}<${inputVar}.length;++${iteratorVar}){${itemCode}}`
        }

        if itemOutput.flag->Flag.unsafeHas(ValFlag.async) {
          output->B.asyncVal(`Promise.all(${output.inline})`)
        } else {
          output
        }
      }
    }
  | _ =>
    let isUnion = input.isUnion->X.Option.getUnsafe

    let objectVal = input->makeObjectVal(~schema=selfSchema)
    let shouldRecreateInput = ref(
      switch selfSchema.additionalItems->X.Option.getUnsafe {
      // Since we have a check validating the exact properties existence
      | Strict => false
      | Strip =>
        switch input.schema.additionalItems->X.Option.getUnsafe {
        | Schema(_) => true
        | _ => input.schema.items->X.Option.getUnsafe->Js.Array2.length !== length
        }
      | _ => true
      },
    )

    for idx in 0 to length - 1 {
      let schema = items->Js.Array2.unsafe_get(idx)
      let key = idx->Js.Int.toString
      let itemInput = input->B.Val.get(key)
      itemInput.expected = schema
      itemInput.isUnion = Some(isUnion) // We want to controll validation on the decoder side
      let itemOutput = itemInput->parse

      switch itemOutput.validation {
      | Some(validation) if isUnion && schema->isLiteral =>
        let _ = input->B.refineInPlace(~schema=input.schema, ~validation=(~inputVar, ~negative) => {
          validation(
            ~inputVar=inputVar ++ input.global->B.inlineLocation(key)->Path.fromInlinedLocation,
            ~negative,
          )
        })
        itemOutput.validation = None
      | _ => ()
      }

      objectVal->B.Val.Object.add(~location=key, itemOutput)
      if !shouldRecreateInput.contents {
        shouldRecreateInput := itemOutput !== itemInput
      }
    }

    // After input.schema was used, set it to selfSchema
    // so it has a more accurate name in error messages

    if shouldRecreateInput.contents {
      objectVal->B.Val.Object.complete
    } else {
      input.code = objectVal.code // FIXME: Delete from and merge?
      input.vals = objectVal.vals
      input
    }
  }
}
and objectDecoder: Builder.t = (~input, ~selfSchema) => {
  let inputTagFlag = input.schema.tag->TagFlag.get

  if inputTagFlag->Flag.unsafeHas(TagFlag.unknown) {
    let isObjectInput = inputTagFlag->Flag.unsafeHas(TagFlag.object)
    if !isObjectInput {
      // TODO: Use dictFactory here
      let mut = base(objectTag)
      mut.properties = Some(X.Object.immutableEmpty)
      mut.additionalItems = Some(Schema(unknown->castToPublic))

      input->B.refineInPlace(~schema=mut, ~validation=(~inputVar, ~negative) =>
        `typeof ${inputVar}${B.eq(~negative)}"${(objectTag :> string)}"${B.and_(~negative)}${B.exp(
            ~negative,
          )}${inputVar}`
      )
    }
    if selfSchema.additionalItems->X.Option.getUnsafe !== Strip {
      // For strip case we recreate the value
      // For other cases we might optimize it,
      // so check that input is not an array
      let _ =
        input->B.refineInPlace(~schema=input.schema, ~validation=(~inputVar, ~negative) =>
          `${B.exp(~negative=!negative)}Array.isArray(${inputVar})`
        )
    }
  } else if !(inputTagFlag->Flag.unsafeHas(TagFlag.object)) {
    input->B.unsupportedTransform(~from=input.schema, ~target=selfSchema)
  }

  switch selfSchema.additionalItems->X.Option.getUnsafe {
  | Schema(itemSchema) => {
      let itemSchema = itemSchema->castToInternal
      if itemSchema === unknown {
        input
      } else {
        let inputVar = input.var()
        let keyVar = input.global->B.varWithoutAllocation
        let itemInput = input->B.dynamicScope(~locationVar=keyVar)
        let itemOutput = itemInput->parseDynamic

        let isTransformed = itemInput !== itemOutput
        let output = isTransformed ? input->B.val("{}", ~schema=selfSchema) : input
        output.schema = selfSchema

        let itemCode =
          itemOutput->B.mergeWithPathPrepend(
            ~locationVar=keyVar,
            ~appendSafe=?isTransformed
              ? Some(() => output->B.Val.addKey(keyVar, itemOutput))
              : None,
          )

        if isTransformed || itemCode !== "" {
          output.code = output.code ++ `for(let ${keyVar} in ${inputVar}){${itemCode}}`
        }

        if itemOutput.flag->Flag.unsafeHas(ValFlag.async) {
          let resolveVar = output.global->B.varWithoutAllocation
          let rejectVar = output.global->B.varWithoutAllocation
          let asyncParseResultVar = output.global->B.varWithoutAllocation
          let counterVar = output.global->B.varWithoutAllocation
          let outputVar = B.Val.var(output)
          output->B.asyncVal(
            `new Promise((${resolveVar},${rejectVar})=>{let ${counterVar}=Object.keys(${outputVar}).length;for(let ${keyVar} in ${outputVar}){${outputVar}[${keyVar}].then(${asyncParseResultVar}=>{${outputVar}[${keyVar}]=${asyncParseResultVar};if(${counterVar}--===1){${resolveVar}(${outputVar})}},${rejectVar})}})`,
          )
        } else {
          output
        }
      }
    }
  | _ => {
      let isUnion = input.isUnion->X.Option.getUnsafe

      let properties = selfSchema.properties->X.Option.getUnsafe
      let keys = Js.Dict.keys(properties)
      let keysCount = keys->Js.Array2.length

      let objectVal = input->makeObjectVal(~schema=selfSchema)
      let shouldRecreateInput = ref(
        switch selfSchema.additionalItems->X.Option.getUnsafe {
        // Since we have a check validating the exact properties existence
        | Strict => false
        | Strip =>
          switch input.schema.additionalItems->X.Option.getUnsafe {
          | Schema(_) => true
          | _ =>
            input.schema.properties->X.Option.getUnsafe->Js.Dict.keys->Js.Array2.length !==
              keysCount
          }
        | _ => true
        },
      )

      for idx in 0 to keysCount - 1 {
        let key = keys->Js.Array2.unsafe_get(idx)
        let schema = properties->Js.Dict.unsafeGet(key)
        let itemInput = input->B.Val.get(key)
        itemInput.expected = schema
        itemInput.isUnion = Some(isUnion) // We want to controll validation on the decoder side
        let itemOutput = itemInput->parse

        switch itemOutput.validation {
        | Some(validation) if isUnion && schema->isLiteral =>
          let _ = input->B.refineInPlace(~schema=input.schema, ~validation=(
            ~inputVar,
            ~negative,
          ) => {
            validation(
              ~inputVar=inputVar ++ input.global->B.inlineLocation(key)->Path.fromInlinedLocation,
              ~negative,
            )
          })
          itemOutput.validation = None
        | _ => ()
        }

        objectVal->B.Val.Object.add(~location=key, itemOutput)
        if !shouldRecreateInput.contents {
          shouldRecreateInput := itemOutput !== itemInput
        }
      }

      if (
        selfSchema.additionalItems === Some(Strict) &&
          switch input.schema.additionalItems->X.Option.getUnsafe {
          | Schema(_) => true
          | _ => false
          }
      ) {
        let keyVar = objectVal.global->B.varWithoutAllocation
        input.allocate(keyVar)
        objectVal.code = objectVal.code ++ `for(${keyVar} in ${input.var()}){if(`
        switch keys {
        | [] => objectVal.code = objectVal.code ++ "true"
        | _ =>
          for idx in 0 to keys->Js.Array2.length - 1 {
            let key = keys->Js.Array2.unsafe_get(idx)
            if idx !== 0 {
              objectVal.code = objectVal.code ++ "&&"
            }
            objectVal.code = objectVal.code ++ `${keyVar}!==${input.global->B.inlineLocation(key)}`
          }
        }
        objectVal.code =
          objectVal.code ++
          `){${input->B.failWithArg(exccessFieldName => ExcessField(exccessFieldName), keyVar)}}}`
      }

      // After input.schema was used, set it to selfSchema
      // so it has a more accurate name in error messages

      if shouldRecreateInput.contents {
        objectVal->B.Val.Object.complete
      } else {
        input.code = objectVal.code // FIXME: Delete from and merge?
        input.vals = objectVal.vals
        input
      }
    }
  }
}

let recursiveDecoder = Builder.make((~input, ~selfSchema) => {
  let ref = selfSchema.ref->X.Option.getUnsafe
  let defs = input.global.defs->X.Option.getUnsafe
  // Ignore #/$defs/
  let identifier = ref->Js.String2.sliceToEnd(~from=8)
  let def = defs->Js.Dict.unsafeGet(identifier)
  // let flag = if selfSchema.noValidation->X.Option.getUnsafe {
  //   input.global.flag->Flag.without(Flag.typeValidation)
  // } else {
  //   input.global.flag->Flag.with(Flag.typeValidation)
  // }
  let flag = input.global.flag
  // FIXME: Instead of flags it should accept from and to schemas
  let recOperation = switch def->Obj.magic->X.Dict.getUnsafeOptionByInt(flag) {
  | Some(fn) =>
    // A hacky way to prevent infinite recursion
    if fn === %raw(`0`) {
      input->B.embed(def) ++ `[${flag->X.Int.unsafeToString}]`
    } else {
      input->B.embed(fn)
    }
  | None => {
      def
      ->Obj.magic
      ->X.Dict.setByInt(flag, 0)
      let fn = compileDecoder(~schema=input.schema, ~expected=def, ~flag, ~defs=Some(defs))
      def
      ->Obj.magic
      ->X.Dict.setByInt(flag, fn)
      input->B.embed(fn)
    }
  }
  let output = input->B.withPathPrepend(~input, (_, ~input) => {
    let output = input->B.Val.map(recOperation)
    if def.isAsync === None {
      let defsMut = defs->X.Dict.copy
      defsMut->Js.Dict.set(identifier, unknown)
      let _ = def->isAsyncInternal(~defs=Some(defsMut))
    }
    if def.isAsync->X.Option.getUnsafe {
      output.flag = output.flag->Flag.with(ValFlag.async)
    }
    output
  })
  // Force rec function execution
  // for the case when the value is not used
  let _ = output.var()
  output
})

let instanceDecoder = Builder.make((~input, ~selfSchema) => {
  let inputTagFlag = input.schema.tag->TagFlag.get

  if inputTagFlag->Flag.unsafeHas(TagFlag.unknown) {
    input->B.refineInPlace(~schema=selfSchema, ~validation=(~inputVar, ~negative) => {
      let c = `${inputVar} instanceof ${input->B.embed(selfSchema.class)}`
      negative ? `!(${c})` : c
    })
    input
  } else if (
    inputTagFlag->Flag.unsafeHas(TagFlag.instance) && input.schema.class === selfSchema.class
  ) {
    input
  } else {
    input->B.unsupportedTransform(~from=input.schema, ~target=selfSchema)
  }
})

let instance = class_ => {
  let mut = base(instanceTag)
  mut.class = class_->Obj.magic
  mut.decoder = instanceDecoder
  mut->castToPublic
}

X.Object.defineProperty(
  %raw(`sp`),
  "~standard",
  {
    get: (
      () => {
        let schema = %raw(`this`)
        {
          version: 1,
          vendor,
          validate: input => {
            try {
              {
                "value": getDecoder2(~s1=unknown, ~s2=schema)(input->Obj.magic)->Obj.magic,
              }
            } catch {
            | _ => {
                let error = %raw(`exn`)->InternalError.getOrRethrow
                {
                  "issues": [
                    {
                      "message": error->InternalError.reason,
                      "path": error.path === Path.empty ? None : Some(error.path->Path.toArray),
                    },
                  ],
                }->Obj.magic
              }
            }
          },
        }
      }
    )->X.Function.toExpression,
  },
)

X.Object.defineProperty(
  %raw(`sp`),
  "decoder",
  {
    writable: true,
    value: (
      (~input, ~selfSchema as _) => {
        input
      }: Builder.t
    ),
  },
)

let makeConvertOrThrow = (type from to, from: t<from>, to: t<to>, ~flag=?): (from => to) => {
  getDecoder2(~s1=from->castToInternal->reverse, ~s2=to->castToInternal, ~flag?)
}
let makeAsyncConvertOrThrow = (type from to, from: t<from>, to: t<to>, ~flag=Flag.none): (
  from => promise<to>
) =>
  getDecoder2(
    ~s1=from->castToInternal->reverse,
    ~s2=to->castToInternal,
    ~flag=flag->Flag.with(Flag.async),
  )

// =============
// Operations
// =============

@inline
let parseOrThrow = (any, schema) => {
  getDecoder2(~s1=unknown, ~s2=schema->castToInternal)(any)
}

let parseJsonOrThrow = (any, schema) => {
  getDecoder2(~s1=json, ~s2=schema->castToInternal)(any)
}

let parseJsonStringOrThrow = (any, schema) => {
  getDecoder2(~s1=jsonString, ~s2=schema->castToInternal)(any)
}

let parseAsyncOrThrow = (any, schema) => {
  getDecoder2(~s1=unknown, ~s2=schema->castToInternal, ~flag=Flag.async)(any)
}

let convertOrThrow = (input, schema) => {
  getDecoder(~s1=schema->castToInternal)(input)
}

let convertToJsonOrThrow = (input, schema) => {
  getDecoder2(~s1=schema->castToInternal, ~s2=json)(input)
}

let convertToJsonStringOrThrow = (input, schema) => {
  getDecoder2(~s1=schema->castToInternal, ~s2=jsonString)(input)
}

let convertAsyncOrThrow = (input, schema) => {
  getDecoder(~s1=schema->castToInternal, ~flag=Flag.async)(input)
}

let reverseConvertOrThrow = (value, schema) => {
  getDecoder(~s1=schema->castToInternal->reverse)(value)
}

let reverseConvertToJsonOrThrow = (value, schema) => {
  getDecoder2(~s1=schema->castToInternal->reverse, ~s2=json)(value)
}

let reverseConvertToJsonStringOrThrow = (value: 'value, schema: t<'value>, ~space=?): string => {
  getDecoder2(
    ~s1=schema->castToInternal->reverse,
    ~s2=switch space {
    | None
    | Some(0) => jsonString
    | Some(v) => jsonStringWithSpace(v)->castToInternal
    },
  )(value)
}

let assertResult = unit->copySchema
assertResult.noValidation = Some(true)

let assertOrThrow = (any, schema) => {
  getDecoder3(~s1=unknown, ~s2=schema->castToInternal, ~s3=assertResult)(any)
}

let isAsync = schema => {
  let schema = schema->castToInternal
  switch schema.isAsync {
  | None => schema->isAsyncInternal(~defs=%raw(`0`))
  | Some(v) => v
  }
}

let wrapExnToFailure = exn => {
  if %raw("exn&&exn.s===s") {
    Failure({error: exn->(Obj.magic: exn => error)})
  } else {
    throw(exn)
  }
}

let js_safe = fn => {
  try {
    Success({
      value: fn(),
    })
  } catch {
  | _ => wrapExnToFailure(%raw(`exn`))
  }
}

let js_safeAsync = fn => {
  try {
    fn()->X.Promise.thenResolveWithCatch(value => Success({value: value}), wrapExnToFailure)
  } catch {
  | _ => X.Promise.resolve(wrapExnToFailure(%raw(`exn`)))
  }
}

module Metadata = {
  module Id: {
    type t<'metadata>
    let make: (~namespace: string, ~name: string) => t<'metadata>
    let internal: string => t<'metadata>
    external toKey: t<'metadata> => string = "%identity"
  } = {
    type t<'metadata> = string

    let make = (~namespace, ~name) => {
      `m:${namespace}:${name}`
    }

    let internal = name => {
      `m:${name}`
    }

    external toKey: t<'metadata> => string = "%identity"
  }

  let get = (schema, ~id: Id.t<'metadata>) => {
    schema->(Obj.magic: t<'a> => dict<option<'metadata>>)->Js.Dict.unsafeGet(id->Id.toKey)
  }

  @inline
  let setInPlace = (schema, ~id: Id.t<'metadata>, metadata: 'metadata) => {
    schema->(Obj.magic: internal => dict<'metadata>)->Js.Dict.set(id->Id.toKey, metadata)
  }

  let set = (schema, ~id: Id.t<'metadata>, metadata: 'metadata) => {
    let schema = schema->castToInternal
    let mut = schema->copySchema
    mut->setInPlace(~id, metadata)
    mut->castToPublic
  }
}

let defsPath = `#/$defs/`
let recursive = (name, fn) => {
  let ref = `${defsPath}${name}`
  let refSchema = base(refTag)
  refSchema.ref = Some(ref)
  refSchema.name = Some(name)
  refSchema.decoder = recursiveDecoder

  // This is for mutual recursion
  let isNestedRec = globalConfig.defsAccumulator->Obj.magic
  if !isNestedRec {
    globalConfig.defsAccumulator = Some(Js.Dict.empty())
  }
  let def = fn(refSchema->castToPublic)->castToInternal
  if def.name->Obj.magic {
    refSchema.name = def.name
  } else {
    def.name = Some(name)
  }
  globalConfig.defsAccumulator
  ->X.Option.getUnsafe
  ->Js.Dict.set(name, def)

  if isNestedRec {
    refSchema->castToPublic
  } else {
    let schema = base(refTag)
    schema.name = def.name
    schema.ref = Some(ref)
    schema.defs = globalConfig.defsAccumulator
    schema.decoder = recursiveDecoder

    globalConfig.defsAccumulator = None

    schema->castToPublic
  }
}

let noValidation = (schema, value) => {
  let schema = schema->castToInternal
  let mut = schema->copySchema

  // TODO: Test for discriminant literal
  // TODO: Better test reverse
  mut.noValidation = Some(value)
  mut->castToPublic
}

let appendRefiner = (~existingDecoder: builder, refiner) => {
  (~input, ~selfSchema) => {
    let output = existingDecoder(~input, ~selfSchema)
    output.code = output.code ++ output->refiner(~inputVar=output.var(), ~selfSchema)
    output
  }
}

let internalRefine = (schema, refiner) => {
  let schema = schema->castToInternal
  updateOutput(schema, mut => {
    mut.decoder = appendRefiner(~existingDecoder=mut.decoder, refiner(mut))
  })
}

let refine: (t<'value>, s<'value> => 'value => unit) => t<'value> = (schema, refiner) => {
  schema->internalRefine(_ =>
    (b, ~inputVar, ~selfSchema) => {
      `${b->B.embed(refiner(b->B.effectCtx(~selfSchema)))}(${inputVar});`
    }
  )
}

let addRefinement = (schema, ~metadataId, ~refinement, ~refiner) => {
  schema->internalRefine(mut => {
    mut->Metadata.setInPlace(
      ~id=metadataId,
      switch schema->Metadata.get(~id=metadataId) {
      | Some(refinements) => refinements->X.Array.append(refinement)
      | None => [refinement]
      },
    )

    refiner
  })
}

type transformDefinition<'input, 'output> = {
  @as("p")
  parser?: 'input => 'output,
  @as("a")
  asyncParser?: 'input => promise<'output>,
  @as("s")
  serializer?: 'output => 'input,
}
let transform: (t<'input>, s<'output> => transformDefinition<'input, 'output>) => t<'output> = (
  schema,
  transformer,
) => {
  let schema = schema->castToInternal
  updateOutput(schema, mut => {
    mut.parser = Some(
      Builder.make((~input, ~selfSchema) => {
        switch transformer(input->B.effectCtx(~selfSchema)) {
        | {parser, asyncParser: ?None} => B.embedSyncOperation(~input, ~fn=parser)
        | {parser: ?None, asyncParser} => B.embedAsyncOperation(~input, ~fn=asyncParser)
        | {parser: ?None, asyncParser: ?None, serializer: ?None} => input
        | {parser: ?None, asyncParser: ?None, serializer: _} =>
          input->B.invalidOperation(~description=`The S.transform parser is missing`)
        | {parser: _, asyncParser: _} =>
          input->B.invalidOperation(
            ~description=`The S.transform doesn't allow parser and asyncParser at the same time. Remove parser in favor of asyncParser`,
          )
        }
      }),
    )
    mut.to = Some({
      let to = base(unknownTag)
      to.serializer = Some(
        (~input, ~selfSchema) => {
          switch transformer(input->B.effectCtx(~selfSchema)) {
          | {serializer} => B.embedSyncOperation(~input, ~fn=serializer)
          | {parser: ?None, asyncParser: ?None, serializer: ?None} => input
          | {serializer: ?None, asyncParser: ?Some(_)}
          | {serializer: ?None, parser: ?Some(_)} =>
            input->B.invalidOperation(~description=`The S.transform serializer is missing`)
          }
        },
      )
      to
    })
    let _ = %raw(`delete mut.isAsync`)
  })
}

let nullAsUnit = base(nullTag)
nullAsUnit.const = %raw(`null`)
nullAsUnit.to = Some(unit)
nullAsUnit.decoder = Literal.literalDecoder
let nullAsUnit = nullAsUnit->castToPublic

let neverBuilder = Builder.make((~input, ~selfSchema) => {
  input.code = input.code ++ B.embedInvalidType(~input, ~expected=selfSchema) ++ ";"
  input.skipTo = Some(true)
  input
})

let never = base(neverTag)
never.decoder = neverBuilder
let never: t<never> = never->castToPublic

let nestedLoc = "BS_PRIVATE_NESTED_SOME_NONE"

module Dict = {
  let factory = item => {
    let item = item->castToInternal
    let mut = base(objectTag)
    mut.properties = Some(X.Object.immutableEmpty)
    mut.additionalItems = Some(Schema(item->castToPublic))
    mut.decoder = objectDecoder
    mut->castToPublic
  }
}

module Array = {
  module Refinement = {
    type kind =
      | Min({length: int})
      | Max({length: int})
      | Length({length: int})
    type t = {
      kind: kind,
      message: string,
    }

    let metadataId: Metadata.Id.t<array<t>> = Metadata.Id.internal("Array.refinements")
  }

  let refinements = schema => {
    switch schema->Metadata.get(~id=Refinement.metadataId) {
    | Some(m) => m
    | None => []
    }
  }
}

module Union = {
  @unboxed
  type itemCode = Single(string) | Multiple(array<string>)

  let isPriority = (tagFlag, byKey: dict<array<unknown>>) => {
    (tagFlag->Flag.unsafeHas(TagFlag.array->Flag.with(TagFlag.instance)) &&
      byKey->Stdlib.Dict.has((objectTag: tag :> string))) ||
      (tagFlag->Flag.unsafeHas(TagFlag.nan) && byKey->Stdlib.Dict.has((numberTag: tag :> string)))
  }

  let isWiderUnionSchema = (~schemaAnyOf, ~inputAnyOf) => {
    inputAnyOf->Js.Array2.everyi((inputSchema, idx) => {
      switch schemaAnyOf->X.Array.getUnsafeOption(idx) {
      | Some(schema) =>
        !(
          inputSchema.tag
          ->TagFlag.get
          ->Flag.unsafeHas(
            TagFlag.array
            ->Flag.with(TagFlag.instance)
            ->Flag.with(TagFlag.ref)
            ->Flag.with(TagFlag.union)
            ->Flag.with(TagFlag.array)
            ->Flag.with(TagFlag.object),
          )
        ) &&
        inputSchema.tag === schema.tag &&
        inputSchema.const === schema.const &&
        inputSchema.to === None
      | None => false
      }
    })
  }

  let unionDecoder = Builder.make((~input, ~selfSchema) => {
    let schemas = selfSchema.anyOf->X.Option.getUnsafe
    let initialInputTagFlag = input.schema.tag->TagFlag.get

    let toPerCase = switch selfSchema {
    | {parser: ?None, to} => Some(to)
    | _ => None
    }

    if (
      initialInputTagFlag->Flag.unsafeHas(TagFlag.union) &&
      isWiderUnionSchema(
        ~schemaAnyOf=schemas,
        ~inputAnyOf=input.schema.anyOf->X.Option.getUnsafe,
      ) &&
      toPerCase === None
    ) {
      input
    } else {
      if (
        input.schema.encoder === None &&
          initialInputTagFlag->Flag.unsafeHas(TagFlag.union->Flag.with(TagFlag.ref))
      ) {
        input.schema = unknown
      }

      let initialInline = input.inline

      let fail = caught => {
        `${input->B.embed(
            (
              _ => {
                let args = %raw(`arguments`)
                input->B.throw(
                  ~path=input.path,
                  ~code=InvalidType({
                    expected: selfSchema->castToPublic,
                    received: unknown->castToPublic,
                    value: args->Js.Array2.unsafe_get(0),
                    unionErrors: ?(
                      args->Js.Array2.length > 1
                        ? Some(args->X.Array.fromArguments->Js.Array2.sliceFrom(1))
                        : None
                    ),
                  }),
                )
              }
            )->X.Function.toExpression,
          )}(${input.var()}${caught})`
      }

      let getArrItemsCode = (arr: array<unknown>, ~isDeopt) => {
        let typeValidationInput = arr->Js.Array2.unsafe_get(0)->(Obj.magic: unknown => val)
        let typeValidationOutput = arr->Js.Array2.unsafe_get(1)->(Obj.magic: unknown => val)

        let itemStart = ref("")
        let itemEnd = ref("")
        let itemNextElse = ref(false)
        let itemNoop = ref("")
        let caught = ref("")

        // Accumulate schemas code by refinement (discriminant)
        // so if we have two schemas with the same discriminant
        // We can generate a single switch statement
        // with try/catch blocks for each item
        // If we come across an item without a discriminant
        // we need to dump all accumulated schemas in try block
        // and have the item without discriminant as catch all
        // If we come across an item without a discriminant
        // and without any code, it means that this item is always valid
        // and we should exit early
        let byDiscriminant = ref(Js.Dict.empty())

        let preItems = 2
        let itemIdx = ref(preItems)
        let lastIdx = arr->Js.Array2.length - 1
        while itemIdx.contents <= lastIdx {
          // Copy it one more time, since every case decoder
          // might mutate the input
          let input = typeValidationOutput->B.Val.cleanValFrom
          input.isUnion = Some(true)

          let isLast = itemIdx.contents === lastIdx
          let isFirst = itemIdx.contents === preItems
          let withExhaustiveCheck = ref(!(isFirst && isLast))

          let itemCode = ref("")
          let itemCond = ref("")
          try {
            let itemOutput = input->parse
            switch input.validation {
            | Some(validation) => {
                itemCond := validation(~inputVar=input.var(), ~negative=false)
                input.validation = None
              }
            | None => ()
            }

            if itemOutput !== input ? itemOutput.inline !== typeValidationInput.inline : false {
              if itemOutput.flag->Flag.unsafeHas(ValFlag.async) {
                typeValidationInput.flag = input.flag->Flag.with(ValFlag.async)
              }
              itemOutput.code =
                itemOutput.code ++
                // Need to allocate a var here, so we don't mutate the input object field
                `${typeValidationInput.var()}=${itemOutput.inline}`
            }

            itemCode := itemOutput->B.merge
          } catch {
          | _ => {
              let errorVar = input->B.embed(%raw(`exn`)->InternalError.getOrRethrow)
              if isLast {
                // FIXME:
                withExhaustiveCheck := false
              }
              itemCode := (
                  isLast && !isDeopt
                    ? {
                        withExhaustiveCheck := false
                        fail(`,${errorVar}`)
                      }
                    : "throw " ++ errorVar
                )
            }
          }
          let itemCond = itemCond.contents
          let itemCode = itemCode.contents

          // Accumulate item parser when it has a discriminant
          if itemCond->X.String.unsafeToBool {
            if itemCode->X.String.unsafeToBool {
              switch byDiscriminant.contents->X.Dict.getUnsafeOption(itemCond) {
              | Some(Multiple(arr)) => arr->Js.Array2.push(itemCode)->ignore
              | Some(Single(code)) =>
                byDiscriminant.contents->Js.Dict.set(itemCond, Multiple([code, itemCode]))
              | None => byDiscriminant.contents->Js.Dict.set(itemCond, Single(itemCode))
              }
            } else {
              // We have a condition but without additional parsing logic
              // So we accumulate it in case it's needed for a refinement later
              itemNoop := (
                  itemNoop.contents->X.String.unsafeToBool
                    ? `${itemNoop.contents}||${itemCond}`
                    : itemCond
                )
            }
          }

          // Allocate all accumulated discriminants
          // If we have an item without a discriminant
          // and need to deopt. Or we are at the last item
          if itemCond->X.String.unsafeToBool->not || isLast {
            let accedDiscriminants = byDiscriminant.contents->Js.Dict.keys
            for idx in 0 to accedDiscriminants->Js.Array2.length - 1 {
              let discrim = accedDiscriminants->Js.Array2.unsafe_get(idx)
              let if_ = itemNextElse.contents ? "else if" : "if"
              itemStart := itemStart.contents ++ if_ ++ `(${discrim}){`
              switch byDiscriminant.contents->Js.Dict.unsafeGet(discrim) {
              | Single(code) => itemStart := itemStart.contents ++ code ++ "}"
              | Multiple(arr) =>
                let caught = ref("")
                for idx in 0 to arr->Js.Array2.length - 1 {
                  let code = arr->Js.Array2.unsafe_get(idx)
                  let errorVar = `e` ++ idx->X.Int.unsafeToString
                  itemStart := itemStart.contents ++ `try{${code}}catch(${errorVar}){`
                  caught := `${caught.contents},${errorVar}`
                }
                itemStart :=
                  itemStart.contents ++
                  fail(caught.contents) ++
                  Js.String2.repeat("}", arr->Js.Array2.length) ++ "}"
              }
              itemNextElse := true
            }
            byDiscriminant.contents = Js.Dict.empty()
          }

          if itemCond->X.String.unsafeToBool->not {
            if itemCode->X.String.unsafeToBool->not {
              // If we don't have a condition (discriminant)
              // and additional parsing logic,
              // it means that this item is always passes
              // so we can remove preceding accumulated refinements
              // and exit early even if there are other items
              itemNoop := ""
              itemIdx := lastIdx
              withExhaustiveCheck := false
            } else {
              // The item without refinement should switch to deopt mode
              // Since there might be validation in the body
              if itemNoop.contents->X.String.unsafeToBool {
                let if_ = itemNextElse.contents ? "else if" : "if"
                itemStart := itemStart.contents ++ if_ ++ `(!(${itemNoop.contents})){`
                itemEnd := "}" ++ itemEnd.contents
                itemNoop := ""
                itemNextElse := false
              }
              if isLast && (isDeopt || !withExhaustiveCheck.contents || isFirst) {
                // For the last item don't add try/catch
                itemStart :=
                  itemStart.contents ++ `${itemNextElse.contents ? "else{" : ""}${itemCode}`
                itemEnd := (itemNextElse.contents ? "}" : "") ++ itemEnd.contents
              } else {
                let errorVar = `e` ++ (itemIdx.contents - preItems)->X.Int.unsafeToString
                itemStart :=
                  itemStart.contents ++
                  `${itemNextElse.contents ? "else{" : ""}try{${itemCode}}catch(${errorVar}){`
                itemEnd := (itemNextElse.contents ? "}" : "") ++ "}" ++ itemEnd.contents
                caught := `${caught.contents},${errorVar}`
                itemNextElse := false
              }
            }
          }
          if isLast {
            if itemNoop.contents->X.String.unsafeToBool {
              if itemStart.contents->X.String.unsafeToBool {
                let if_ = itemNextElse.contents ? "else if" : "if"
                itemStart :=
                  itemStart.contents ++ if_ ++ `(!(${itemNoop.contents})){${fail(caught.contents)}}`
              } else {
                let _ = typeValidationOutput->B.refineInPlace(
                  ~schema=typeValidationOutput.schema,
                  ~validation=(~inputVar as _, ~negative) => {
                    `${B.exp(~negative)}(${itemNoop.contents})`
                  },
                )
              }
            } else if withExhaustiveCheck.contents {
              let errorCode = fail(caught.contents)
              itemStart :=
                itemStart.contents ++ (itemNextElse.contents ? `else{${errorCode}}` : errorCode)
            }
          }

          itemIdx := itemIdx.contents->X.Int.plus(1)
        }

        itemStart.contents ++ itemEnd.contents
      }

      let start = ref("")
      let end = ref("")
      let caught = ref("")
      // If we got a case which always passes,
      // we can exit early
      let exit = ref(false)

      let lastIdx = schemas->Js.Array2.length - 1
      let byKey: ref<dict<array<unknown>>> = ref(Js.Dict.empty())
      let keys = ref([])
      let updatedSchemas = []
      for idx in 0 to lastIdx {
        let schema = switch toPerCase {
        | Some(target) =>
          updateOutput(schemas->Js.Array2.unsafe_get(idx), mut => {
            // switch selfSchema.refiner {
            // | Some(refiner) => mut.refiner = Some(appendRefiner(mut.refiner, refiner))
            // | None => ()
            // }
            mut.to = Some(target)
          })->castToInternal
        | _ => schemas->Js.Array2.unsafe_get(idx)
        }
        updatedSchemas->Js.Array2.push(schema)->ignore
        let tag = schema.tag
        let tagFlag = TagFlag.get(tag)
        let key =
          tagFlag->Flag.unsafeHas(TagFlag.instance)
            ? (schema.class->Obj.magic)["name"]
            : (tag :> string)

        if (
          tagFlag->Flag.unsafeHas(TagFlag.undefined) &&
            selfSchema->Obj.magic->Stdlib.Dict.has("fromDefault")
        ) {
          // skip it
          ()
        } else {
          let initialArr = byKey.contents->X.Dict.getUnsafeOption(key)
          switch initialArr {
          | Some(arr) =>
            if (
              tagFlag->Flag.unsafeHas(TagFlag.object) &&
                schema.properties->X.Option.getUnsafe->Stdlib.Dict.has(nestedLoc)
            ) {
              // This is a special case for https://github.com/DZakh/sury/issues/150
              // When nested option goes together with an empty object schema
              // Since we put None case check second, we need to change priority here.
              arr
              ->Stdlib.Array.splice(
                ~start=arr->Stdlib.Array.length - 1,
                ~remove=0,
                ~insert=[schema->(Obj.magic: internal => unknown)],
              )
              ->ignore
            } else if (
              // TODO: Is this check needed?
              // There can only be one valid. Dedupe
              !(
                tagFlag->Flag.unsafeHas(
                  TagFlag.undefined->Flag.with(TagFlag.null)->Flag.with(TagFlag.nan),
                )
              )
            ) {
              arr->Js.Array2.push(schema->(Obj.magic: internal => unknown))->ignore
            }
          | None =>
            // Recreate input val for every schema
            // since we will mutate it
            let typeValidationInput = input->B.Val.cleanValFrom
            typeValidationInput.expected = if tagFlag->Flag.unsafeHas(TagFlag.null) {
              nullLiteral
            } else if tagFlag->Flag.unsafeHas(TagFlag.undefined) {
              unit
            } else if tagFlag->Flag.unsafeHas(TagFlag.object) {
              Dict.factory(unknown->castToPublic)->castToInternal
            } else if tagFlag->Flag.unsafeHas(TagFlag.array) {
              array(unknown->castToPublic)->castToInternal
            } else if tagFlag->Flag.unsafeHas(TagFlag.instance) {
              instance(schema.class)->castToInternal
            } else if tagFlag->Flag.unsafeHas(TagFlag.nan) {
              nan
            } else if tagFlag->Flag.unsafeHas(TagFlag.string) {
              string
            } else if tagFlag->Flag.unsafeHas(TagFlag.number) {
              float
            } else if tagFlag->Flag.unsafeHas(TagFlag.boolean) {
              bool
            } else if tagFlag->Flag.unsafeHas(TagFlag.bigint) {
              bigint
            } else if tagFlag->Flag.unsafeHas(TagFlag.symbol) {
              symbol
            } else {
              unknown
            }
            let typeValidationOutput = typeValidationInput->parse
            typeValidationInput.expected = schema

            if isPriority(tagFlag, byKey.contents) {
              // Not the fastest way, but it's the simplest way
              // to make sure NaN is checked before number
              // And instance and array checked before object
              keys.contents->Js.Array2.unshift(key)->ignore
            } else {
              keys.contents->Js.Array2.push(key)->ignore
            }
            byKey.contents->Js.Dict.set(
              key,
              [
                typeValidationInput->(Obj.magic: val => unknown),
                typeValidationOutput->(Obj.magic: val => unknown),
                schema->(Obj.magic: internal => unknown),
              ],
            )

            switch typeValidationInput.validation {
            | Some(_) => ()
            | None => {
                for keyIdx in 0 to keys.contents->Stdlib.Array.length - 1 {
                  let key = keys.contents->Stdlib.Array.getUnsafe(keyIdx)
                  if !exit.contents {
                    let arr = byKey.contents->Stdlib.Dict.getUnsafe(key)
                    let typeValidationOutput =
                      arr->Stdlib.Array.getUnsafe(1)->(Obj.magic: unknown => val)
                    let itemsCode = getArrItemsCode(arr, ~isDeopt=true)
                    let blockCode = typeValidationOutput->B.merge ++ itemsCode

                    if blockCode->X.String.unsafeToBool {
                      let errorVar = `e` ++ (idx + keyIdx)->X.Int.unsafeToString
                      start := start.contents ++ `try{${blockCode}}catch(${errorVar}){`
                      end := "}" ++ end.contents
                      caught := `${caught.contents},${errorVar}`
                    } else {
                      exit := true
                    }
                  }
                }

                byKey := Js.Dict.empty()
                keys := []
              }
            }
          }
        }
      }

      let byKey = byKey.contents
      let keys = keys.contents

      let typeValidation = true

      if !exit.contents {
        let nextElse = ref(false)
        let noop = ref("")

        for idx in 0 to keys->Js.Array2.length - 1 {
          let arr = byKey->Js.Dict.unsafeGet(keys->Js.Array2.unsafe_get(idx))
          let typeValidationInput = arr->Stdlib.Array.getUnsafe(0)->(Obj.magic: unknown => val)
          let typeValidationOutput = arr->Stdlib.Array.getUnsafe(1)->(Obj.magic: unknown => val)
          let firstSchema = arr->Stdlib.Array.getUnsafe(2)->(Obj.magic: unknown => internal)

          let itemsCode = getArrItemsCode(arr, ~isDeopt=false)

          // Make cond as a weird callback, to prevent input.var call until it's needed
          let cond = ref(
            switch typeValidationInput.validation {
            | None => InternalError.panic("No validation") // This shouldn't happen, but I didn't test it 100%
            | Some(validation) =>
              (~input as _) => {
                validation(~inputVar=input.var(), ~negative=false)
              }
            },
          )
          typeValidationInput.validation = None

          let blockCode = typeValidationOutput->B.merge ++ itemsCode

          if blockCode->X.String.unsafeToBool || isPriority(firstSchema.tag->TagFlag.get, byKey) {
            let if_ = nextElse.contents ? "else if" : "if"
            start := start.contents ++ if_ ++ `(${cond.contents(~input)}){${blockCode}}`
            nextElse := true
          } else if typeValidation {
            let cond = cond.contents(~input)
            noop := (noop.contents->X.String.unsafeToBool ? `${noop.contents}||${cond}` : cond)
          }
        }

        if typeValidation || keys->Stdlib.Array.length === 0 {
          let errorCode = fail(caught.contents)
          start :=
            start.contents ++ if noop.contents->X.String.unsafeToBool {
              let if_ = nextElse.contents ? "else if" : "if"
              if_ ++ `(!(${noop.contents})){${errorCode}}`
            } else if nextElse.contents {
              `else{${errorCode}}`
            } else {
              errorCode
            }
        }
      }

      let output = input->B.Val.cleanValFrom
      output.from = Some(input)
      output.code = output.code ++ start.contents ++ end.contents

      let o = if output.flag->Flag.unsafeHas(ValFlag.async) {
        output->B.asyncVal(`Promise.resolve(${output.inline})`)
      } else if output.var === B._var {
        // TODO: Think how to make it more robust
        // Recreate to not break the logic to determine
        // whether the output is changed

        // Use output.b instead of b because of mergeWithCatch
        // Should refactor mergeWithCatch to make it simpler
        // All of this is a hack to make mergeWithCatch think that there are no changes. eg S.array(S.option(item))
        if (
          input.code === "" &&
          output.code === "" &&
          (output.varsAllocation === `${output.inline}=${initialInline}` || initialInline === "i")
        ) {
          // FIXME: Might not be not needed
          input.varsAllocation = ""
          input.allocate = B.initialAllocate
          input.var = B._notVar
          input.inline = initialInline
          input
        } else {
          output->B.Val.copy
        }
      } else {
        output
      }

      o.schema = switch toPerCase {
      | Some(to) => {
          o.skipTo = Some(true)
          to->getOutputSchema
        }
      | _ => selfSchema
      }

      o
    }
  })

  let factory = schemas => {
    let schemas: array<internal> = schemas->Obj.magic
    // TODO:
    // 1. Fitler out items without parser
    // 2. Remove duplicate schemas
    // 3. Spread Union and JSON if they are not transformed
    // 4. Provide correct `has` value for Union and JSON
    switch schemas {
    | [] => InternalError.panic("S.union requires at least one item")
    | [schema] => schema->castToPublic
    | _ =>
      let has = Js.Dict.empty()
      let anyOf = X.Set.make()

      for idx in 0 to schemas->Js.Array2.length - 1 {
        let schema = schemas->Js.Array2.unsafe_get(idx)

        // Check if the union is not transformed
        if schema.tag === unionTag && schema.to === None {
          schema.anyOf
          ->X.Option.getUnsafe
          ->Js.Array2.forEach(item => {
            anyOf->X.Set.add(item)
          })
          let _ = has->X.Dict.mixin(schema.has->X.Option.getUnsafe)
        } else {
          anyOf->X.Set.add(schema)
          has->setHas(schema.tag)
        }
      }
      let mut = base(unionTag)
      mut.anyOf = Some(anyOf->X.Set.toArray)
      mut.decoder = unionDecoder
      mut.has = Some(has)
      mut->castToPublic
    }
  }
}

module Option = {
  type default = Value(unknown) | Callback(unit => unknown)

  let nestedOption = {
    let nestedNone = () => {
      let itemSchema = Literal.parse(0)
      // FIXME: dict{}
      let properties = Js.Dict.empty()
      properties->Js.Dict.set(nestedLoc, itemSchema)
      {
        tag: objectTag,
        properties,
        additionalItems: Strip,
        decoder: objectDecoder,
        // TODO: Support this as a default coercion
        serializer: Builder.make((~input, ~selfSchema) => {
          input->B.newConst(~schema=selfSchema.to->X.Option.getUnsafe)
        }),
      }
    }

    let parser = Builder.make((~input, ~selfSchema) => {
      input->B.val(
        `{${nestedLoc}:${(
            (selfSchema->getOutputSchema).properties
            ->X.Option.getUnsafe
            ->Js.Dict.unsafeGet(nestedLoc)
          ).const->Obj.magic}}`,
        ~schema=selfSchema.to->X.Option.getUnsafe,
      )
    })

    item => {
      item
      ->updateOutput(mut => {
        mut.to = Some(nestedNone())
        mut.parser = Some(parser)
      })
      ->castToInternal
    }
  }

  let factory = (item, ~unit=unit->castToPublic) => {
    let item = item->castToInternal

    switch item->getOutputSchema {
    | {tag: Undefined} => Union.factory([unit->castToUnknown, item->nestedOption->castToPublic])
    | {tag: Union, ?anyOf, ?has} =>
      item->updateOutput(mut => {
        let schemas = anyOf->X.Option.getUnsafe
        let mutHas = has->X.Option.getUnsafe->X.Dict.copy

        let newAnyOf = []
        for idx in 0 to schemas->Stdlib.Array.length - 1 {
          let schema = schemas->Js.Array2.unsafe_get(idx)
          newAnyOf
          ->Js.Array2.push(
            switch schema->getOutputSchema {
            | {tag: Undefined} => {
                mutHas->Js.Dict.set(((unit->castToInternal).tag: tag :> string), true)
                newAnyOf->Js.Array2.push(unit->castToInternal)->ignore
                schema->nestedOption
              }
            | {properties} =>
              switch properties->X.Dict.getUnsafeOption(nestedLoc) {
              | Some(nestedSchema) =>
                schema
                ->updateOutput(mut => {
                  // FIXME: dict{}
                  let properties = Js.Dict.empty()
                  properties->Js.Dict.set(
                    nestedLoc,
                    {
                      ...nestedSchema,
                      const: nestedSchema.const->Obj.magic->X.Int.plus(1)->Obj.magic,
                    },
                  )
                  mut.properties = Some(properties)
                })
                ->castToInternal
              | None => schema
              }
            | _ => schema
            },
          )
          ->ignore
        }

        if newAnyOf->Js.Array2.length === schemas->Js.Array2.length {
          mutHas->Js.Dict.set(((unit->castToInternal).tag: tag :> string), true)
          newAnyOf->Js.Array2.push(unit->castToInternal)->ignore
        }

        mut.anyOf = Some(newAnyOf)
        mut.has = Some(mutHas)
      })
    | _ => Union.factory([item->castToPublic, unit->castToUnknown])
    }
  }

  let getWithDefault = (schema: t<option<'value>>, default) => {
    schema
    ->castToInternal
    ->updateOutput(mut => {
      switch mut.anyOf {
      | Some(anyOf) => {
          let item = ref(None)
          let itemOutputSchema = ref(None)

          for idx in 0 to anyOf->Stdlib.Array.length - 1 {
            let schema = anyOf->Stdlib.Array.getUnsafe(idx)
            let outputSchema = schema->getOutputSchema
            switch outputSchema.tag {
            | Undefined => ()
            | _ =>
              switch item.contents {
              | None => {
                  item := Some(schema)
                  itemOutputSchema := Some(outputSchema)
                }
              | Some(_) =>
                InternalError.panic(`Can't set default for ${mut->castToPublic->toExpression}`)
              }
            }
          }

          let item = switch item.contents {
          | None => InternalError.panic(`Can't set default for ${mut->castToPublic->toExpression}`)
          | Some(s) => s
          }

          // FIXME: Should delete schema.unnest on reverse?
          // FIXME: Ensure that default has the same type as the item
          // Or maybe not, but need to make it properly with JSON Schema

          mut.parser = Some(
            Builder.make((~input, ~selfSchema) => {
              input->B.transform(
                ~input,
                (b, ~input) => {
                  let inputVar = input.var()
                  b->B.val(
                    `${inputVar}===void 0?${switch default {
                      | Value(v) => b->B.inlineConst(Literal.parse(v))
                      | Callback(cb) => `${b->B.embed(cb)}()`
                      }}:${inputVar}`,
                    ~schema=selfSchema.to->X.Option.getUnsafe,
                  )
                },
              )
            }),
          )
          let to = itemOutputSchema.contents->X.Option.getUnsafe->copySchema

          to.serializer = Some(to.decoder)
          to.decoder = (~input, ~selfSchema as _) => input

          mut.to = Some(to)

          switch default {
          | Value(v) =>
            try mut.default =
              getDecoder(~s1=item->reverse)(v)->(
                Obj.magic: unknown => option<internalDefault>
              ) catch {
            | _ => ()
            }
          | Callback(_) => ()
          }
        }
      | None => InternalError.panic(`Can't set default for ${mut->castToPublic->toExpression}`)
      }
    })
  }

  let getOr = (schema, defalutValue) =>
    schema->getWithDefault(Value(defalutValue->castAnyToUnknown))
  let getOrWith = (schema, defalutCb) =>
    schema->getWithDefault(Callback(defalutCb->(Obj.magic: (unit => 'a) => unit => unknown)))
}

module Object = {
  type rec s = {
    @as("f") field: 'value. (string, t<'value>) => 'value,
    fieldOr: 'value. (string, t<'value>, 'value) => 'value,
    tag: 'value. (string, 'value) => unit,
    nested: string => s,
    flatten: 'value. t<'value> => 'value,
  }

  let rec setAdditionalItems = (schema, additionalItems, ~deep) => {
    let schema = schema->castToInternal
    switch schema {
    | {additionalItems: currentAdditionalItems}
      if currentAdditionalItems !== additionalItems &&
        currentAdditionalItems->Js.typeof !== (objectTag :> string) => {
        let mut = schema->copySchema
        mut.additionalItems = Some(additionalItems)
        if deep {
          switch schema.items {
          | Some(items) => {
              let newItems = []
              for idx in 0 to items->Js.Array2.length - 1 {
                let s = items->Js.Array2.unsafe_get(idx)
                newItems
                ->Js.Array2.push(
                  s->castToPublic->setAdditionalItems(additionalItems, ~deep)->castToInternal,
                )
                ->ignore
              }
              mut.items = Some(newItems)
            }
          | None => ()
          }

          switch schema.properties {
          | Some(properties) => {
              let newProperties = Js.Dict.empty()
              let keys = properties->Js.Dict.keys
              for idx in 0 to keys->Js.Array2.length - 1 {
                let key = keys->Js.Array2.unsafe_get(idx)
                newProperties->Js.Dict.set(
                  key,
                  properties
                  ->Js.Dict.unsafeGet(key)
                  ->castToPublic
                  ->setAdditionalItems(additionalItems, ~deep)
                  ->castToInternal,
                )
              }
              mut.properties = Some(newProperties)
            }
          | None => ()
          }
        }
        mut->castToPublic
      }
    | _ => schema->castToPublic
    }
  }
}

let strip = schema => {
  schema->Object.setAdditionalItems(Strip, ~deep=false)
}

let deepStrip = schema => {
  schema->Object.setAdditionalItems(Strip, ~deep=true)
}

let strict = schema => {
  schema->Object.setAdditionalItems(Strict, ~deep=false)
}

let deepStrict = schema => {
  schema->Object.setAdditionalItems(Strict, ~deep=true)
}

module Tuple = {
  type s = {
    item: 'value. (int, t<'value>) => 'value,
    tag: 'value. (int, 'value) => unit,
  }
}

module String = {
  module Refinement = {
    type kind =
      | Min({length: int})
      | Max({length: int})
      | Length({length: int})
      | Email
      | Uuid
      | Cuid
      | Url
      | Pattern({re: Js.Re.t})
      | Datetime
    type t = {
      kind: kind,
      message: string,
    }

    let metadataId: Metadata.Id.t<array<t>> = Metadata.Id.internal("String.refinements")
  }

  let refinements = schema => {
    switch schema->Metadata.get(~id=Refinement.metadataId) {
    | Some(m) => m
    | None => []
    }
  }

  let cuidRegex = /^c[^\s-]{8,}$/i
  let uuidRegex = /^[0-9a-fA-F]{8}\b-[0-9a-fA-F]{4}\b-[0-9a-fA-F]{4}\b-[0-9a-fA-F]{4}\b-[0-9a-fA-F]{12}$/i
  // Adapted from https://stackoverflow.com/a/46181/1550155
  let emailRegex = /^(?!\.)(?!.*\.\.)([A-Z0-9_'+\-\.]*)[A-Z0-9_+-]@([A-Z0-9][A-Z0-9\-]*\.)+[A-Z]{2,}$/i
  // Adapted from https://stackoverflow.com/a/3143231
  let datetimeRe = /^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(\.\d+)?Z$/
}

let jsonEncoder = Builder.make((~input, ~selfSchema as to) => {
  let toTagFlag = to.tag->TagFlag.get

  if (
    toTagFlag->Flag.unsafeHas(
      TagFlag.string
      ->Flag.with(TagFlag.boolean)
      ->Flag.with(TagFlag.number)
      ->Flag.with(TagFlag.null),
    )
  ) {
    input.schema = unknown
    input
  } else if toTagFlag->Flag.unsafeHas(TagFlag.bigint) {
    input.schema = unknown
    input.expected = string
    let output = stringDecoder(~input, ~selfSchema=input.expected)
    output.expected = to
    output
  } else if toTagFlag->Flag.unsafeHas(TagFlag.undefined->Flag.with(TagFlag.nan)) {
    input.schema = unknown
    Literal.literalDecoder(~input, ~selfSchema=nullLiteral)
  } else if toTagFlag->Flag.unsafeHas(TagFlag.array) {
    // Validate that the input is an array
    // and then update the schema to be an array of json instead of array of unknown
    input.schema = unknown
    input.expected = array(unknown->castToPublic)->castToInternal
    let output = arrayDecoder(~input, ~selfSchema=input.expected)
    output.schema.additionalItems = Some(Schema(json->castToPublic))
    output.expected = to
    output
  } else if toTagFlag->Flag.unsafeHas(TagFlag.object) {
    // Validate that the input is an object
    // and then update the schema to be an object of json instead of object of unknown
    input.schema = unknown
    input.expected = Dict.factory(unknown->castToPublic)->castToInternal
    let output = objectDecoder(~input, ~selfSchema=input.expected)
    output.schema.additionalItems = Some(Schema(json->castToPublic))
    output.expected = to
    output
  } else {
    input
  }
})

let jsonDecoder = Builder.make((~input, ~selfSchema) => {
  let inputTagFlag = input.schema.tag->TagFlag.get

  if (
    inputTagFlag->Flag.unsafeHas(
      TagFlag.string
      ->Flag.with(TagFlag.number)
      ->Flag.with(TagFlag.boolean)
      ->Flag.with(TagFlag.null),
    ) || input.schema.ref === json.ref
  ) {
    input
  } else if inputTagFlag->Flag.unsafeHas(TagFlag.undefined->Flag.with(TagFlag.nan)) {
    input->B.newConst(~schema=nullLiteral)
  } else if inputTagFlag->Flag.unsafeHas(TagFlag.bigint) {
    input->inputToString
  } else if inputTagFlag->Flag.unsafeHas(TagFlag.array) {
    let mut = base(arrayTag)
    mut.items = Some(
      input.schema.items
      ->X.Option.getUnsafe
      ->Js.Array2.map(_ => json),
    )
    mut.additionalItems = Some(
      switch input.schema.additionalItems->X.Option.getUnsafe {
      | Schema(_) => Schema(json->castToPublic)
      | v => v
      },
    )
    arrayDecoder(~input, ~selfSchema=mut)
  } else if inputTagFlag->Flag.unsafeHas(TagFlag.object) {
    let mut = base(objectTag)
    let properties = Js.Dict.empty()
    input.schema.properties
    ->X.Option.getUnsafe
    ->Stdlib.Dict.keysToArray
    ->Stdlib.Array.forEach(key => {
      properties->Stdlib.Dict.set(key, json)
    })
    mut.properties = Some(properties)
    mut.additionalItems = Some(
      switch input.schema.additionalItems->X.Option.getUnsafe {
      | Schema(_) => Schema(json->castToPublic)
      | v => v
      },
    )
    objectDecoder(~input, ~selfSchema=mut)
  } else if inputTagFlag->Flag.unsafeHas(TagFlag.unknown) {
    let to = selfSchema.to->X.Option.getUnsafe
    // Whether we can optimize encoding during decoding
    let preEncode: bool = to->Obj.magic && !(selfSchema.parser->Obj.magic) // && !(selfSchema.refiner->Obj.magic) FIXME:
    if preEncode {
      input.schema = json
      jsonEncoder(~input, ~selfSchema)
    } else if selfSchema.noValidation->X.Option.getUnsafe {
      input.schema = json
      input
    } else {
      recursiveDecoder(~input, ~selfSchema)
    }
  } else {
    input->B.unsupportedTransform(~from=input.schema, ~target=selfSchema)
  }
})

let enableJson = () => {
  if json->Obj.magic->Js.Dict.unsafeGet(shakenRef)->Obj.magic {
    let _ = %raw(`delete json.as`)
    let jsonRef = base(refTag)
    jsonRef.ref = Some(`${defsPath}${jsonName}`)
    jsonRef.name = Some(jsonName)

    // FIXME: Validate whether dcoders needs to be here
    jsonRef.decoder = jsonDecoder
    jsonRef.encoder = Some(jsonEncoder)
    json.tag = jsonRef.tag
    json.ref = jsonRef.ref
    json.name = Some(jsonName)
    json.decoder = jsonDecoder
    json.encoder = Some(jsonEncoder)
    let defs = Js.Dict.empty()
    let anyOf = [
      string,
      bool,
      float,
      nullLiteral,
      Dict.factory(jsonRef->castToPublic)->castToInternal,
      array(jsonRef->castToPublic)->castToInternal,
    ]
    let has = Js.Dict.empty()
    anyOf->Js.Array2.forEach(schema => {
      has->Js.Dict.set((schema.tag :> string), true)
    })
    defs->Js.Dict.set(
      jsonName,
      {
        name: jsonName,
        tag: unionTag,
        anyOf,
        has,
        decoder: Union.unionDecoder,
      },
    )
    json.defs = Some(defs)
  }
}

let enableJsonString = {
  let inlineJsonString = (input, ~schema) => {
    let tagFlag = schema.tag->TagFlag.get
    let const = schema.const
    if tagFlag->Flag.unsafeHas(TagFlag.undefined->Flag.with(TagFlag.null)) {
      `"null"`
    } else if tagFlag->Flag.unsafeHas(TagFlag.string) {
      const->Obj.magic->X.Inlined.Value.fromString->Js.Json.stringifyAny->Obj.magic
    } else if tagFlag->Flag.unsafeHas(TagFlag.bigint) {
      `"\\"${const->Obj.magic}\\""`
    } else if tagFlag->Flag.unsafeHas(TagFlag.number->Flag.with(TagFlag.boolean)) {
      `"${const->Obj.magic}"`
    } else {
      input->B.unsupportedTransform(~from=schema, ~target=input.expected)
    }
  }

  let jsonStringEncoder = Builder.make((~input, ~selfSchema as to) => {
    if to.format !== Some(JSON) {
      if to->isLiteral {
        input.validation = Some(
          (~inputVar, ~negative) => {
            let inlinedJsonStringConst = input->inlineJsonString(~schema=to)

            input.expected.name = Some(inlinedJsonStringConst)
            `${inputVar}${B.eq(~negative)}${inlinedJsonStringConst}`
          },
        )

        input->B.newConst(~schema=to)
      } else {
        let inputVar = input.var()
        let output = input->B.allocateVal(~schema=json, ~expected=to)
        input.code =
          input.code ++
          `try{${output.inline}=JSON.parse(${inputVar})}catch(t){${B.embedInvalidType(
              ~input,
              ~expected=input.expected,
            )}}`
        let v = output->parse
        v.skipTo = Some(true)
        v
      }
    } else {
      input
    }
  })

  () => {
    if jsonString->Obj.magic->Js.Dict.unsafeGet(shakenRef)->Obj.magic {
      let _ = %raw(`delete jsonString.as`)
      enableJson()

      jsonString.tag = stringTag
      jsonString.format = Some(JSON)
      jsonString.name = Some(`${jsonName} string`)
      jsonString.encoder = Some(jsonStringEncoder)
      jsonString.decoder = Builder.make((~input, ~selfSchema) => {
        let inputTagFlag = input.schema.tag->TagFlag.get

        if inputTagFlag->Flag.unsafeHas(TagFlag.unknown) {
          let to = selfSchema.to->X.Option.getUnsafe
          // Whether we can optimize encoding during decoding
          let preEncode: bool = to->Obj.magic && !(selfSchema.parser->Obj.magic) // && !(selfSchema.refiner->Obj.magic) FIXME:

          let input = stringDecoder(~input, ~selfSchema)

          if preEncode {
            jsonStringEncoder(~input, ~selfSchema=to)
          } else {
            input.code =
              input.code ++
              `try{JSON.parse(${input.var()})}catch(t){${B.embedInvalidType(
                  ~input,
                  ~expected=input.expected,
                )}}`
            input
          }
        } else if input.schema.format === Some(JSON) {
          input
        } else if input.schema->isLiteral {
          input->B.val(input->inlineJsonString(~schema=input.schema), ~schema=selfSchema)
        } else if inputTagFlag->Flag.unsafeHas(TagFlag.string) {
          input->B.val(`JSON.stringify(${input.inline})`, ~schema=selfSchema)
        } else if inputTagFlag->Flag.unsafeHas(TagFlag.number->Flag.with(TagFlag.boolean)) {
          let o = input->inputToString
          o.schema = selfSchema
          o
        } else if inputTagFlag->Flag.unsafeHas(TagFlag.bigint) {
          input->B.val(`"\\""+${input.inline}+"\\""`, ~schema=selfSchema)
        } else if inputTagFlag->Flag.unsafeHas(TagFlag.object->Flag.with(TagFlag.array)) {
          input.expected = json
          let jsonVal = jsonDecoder(~input, ~selfSchema=input.expected)
          jsonVal.expected = selfSchema
          jsonVal->B.val(
            `JSON.stringify(${jsonVal.inline}${switch selfSchema.space {
              | Some(0)
              | None => ""
              | Some(v) => `,null,${v->X.Int.unsafeToString}`
              }})`,
            ~schema=selfSchema,
          )
        } else {
          input->B.unsupportedTransform(~from=input.schema, ~target=selfSchema)
        }
      })
    }
  }
}

let uint8Array = shaken("uint8Array")

let enableUint8Array = () => {
  if uint8Array->Obj.magic->Js.Dict.unsafeGet(shakenRef)->Obj.magic {
    let _ = %raw(`delete uint8Array.as`)
    uint8Array.tag = instanceTag
    uint8Array.class = %raw(`Uint8Array`)
    uint8Array.decoder = Builder.make((~input as inputArg, ~selfSchema) => {
      let inputTagFlag = inputArg.schema.tag->TagFlag.get
      let input = ref(inputArg)

      if inputTagFlag->Flag.unsafeHas(TagFlag.string) {
        input :=
          input.contents->B.val(
            `${input.contents->B.embed(
                %raw(`new TextEncoder()`),
              )}.encode(${input.contents.inline})`,
            ~schema=uint8Array,
          )
      } else if inputTagFlag->Flag.unsafeHas(TagFlag.unknown->Flag.with(TagFlag.instance)) {
        input := instanceDecoder(~input=input.contents, ~selfSchema)
      }

      switch selfSchema {
      | {to, parser: ?None} => {
          let toTagFlag = to.tag->TagFlag.get
          if toTagFlag->Flag.unsafeHas(TagFlag.string) {
            input :=
              input.contents->B.val(
                `${input.contents->B.embed(
                    %raw(`new TextDecoder()`),
                  )}.decode(${input.contents.inline})`,
                ~schema=string,
              )
          }
          input.contents
        }
      | _ => input.contents
      }
    })
  }
}

module Int = {
  module Refinement = {
    type kind =
      | Min({value: int})
      | Max({value: int})

    type t = {
      kind: kind,
      message: string,
    }

    let metadataId: Metadata.Id.t<array<t>> = Metadata.Id.internal("Int.refinements")
  }

  let refinements = schema => {
    switch schema->Metadata.get(~id=Refinement.metadataId) {
    | Some(m) => m
    | None => []
    }
  }
}

module Float = {
  module Refinement = {
    type kind =
      | Min({value: float})
      | Max({value: float})
    type t = {
      kind: kind,
      message: string,
    }

    let metadataId: Metadata.Id.t<array<t>> = Metadata.Id.internal("Float.refinements")
  }

  let refinements = schema => {
    switch schema->Metadata.get(~id=Refinement.metadataId) {
    | Some(m) => m
    | None => []
    }
  }
}

let to = (from, target) => {
  let from = from->castToInternal
  let target = target->castToInternal

  // It makes sense, since S.to quite often will be used
  // inside of a framework where we don't control what's the to argument
  if from === target {
    from->castToPublic
  } else {
    updateOutput(from, mut => {
      mut.to = Some(target)
      // A tricky part about parser is that we don't know the input type in ReScript
      // so we need to directly parse to output instead of input
      // switch parser {
      // | Some(p) =>
      //   mut.parser = Some(
      //     Builder.make((b, ~input, ~selfSchema as _, ~path as _) => {
      //       // TODO: Support async, reverse, nested parsing
      //       b->B.embedSyncOperation(~input, ~fn=p)
      //     }),
      //   )
      // | None => ()
      // }
    })
  }
}

let list = schema => {
  schema
  ->array
  ->transform(_ => {
    parser: array => array->Belt.List.fromArray,
    serializer: list => list->Belt.List.toArray,
  })
}

// TODO: Better test reverse
let meta = (schema: t<'value>, data: meta<'value>) => {
  let schema = schema->castToInternal
  let mut = schema->copySchema
  switch data.name {
  | Some("") => mut.name = None
  | Some(name) => mut.name = Some(name)
  | None => ()
  }
  switch data.title {
  | Some("") => mut.title = None
  | Some(title) => mut.title = Some(title)
  | None => ()
  }
  switch data.description {
  | Some("") => mut.description = None
  | Some(description) => mut.description = Some(description)
  | None => ()
  }
  switch data.deprecated {
  | Some(deprecated) => mut.deprecated = Some(deprecated)
  | None => ()
  }
  switch data.examples {
  | Some([]) => mut.examples = None // FIXME: Delete instead of None
  | Some(examples) => mut.examples = Some(examples->X.Array.map(getDecoder(~s1=schema->reverse)))
  | None => ()
  }
  mut->castToPublic
}

let brand = (schema: t<'value>, id: string) => {
  let schema = schema->castToInternal
  let mut = schema->copySchema
  mut.name = Some(id)
  mut->castToPublic
}

module Schema = {
  type rec shapedSerializerAcc = {
    mutable val?: val,
    mutable properties?: dict<shapedSerializerAcc>,
    mutable flattened?: array<shapedSerializerAcc>,
  }

  type s = {@as("m") matches: 'value. t<'value> => 'value}

  let inputFrom = X.Array.immutableEmpty

  type advancedObjectCtx = {
    // Public API for JS/TS users.
    // It shouldn't be used from ReScript and
    // needed only because we use @as for field to reduce bundle-size
    // of ReScript compiled code
    @as("field") _jsField: 'value. (string, schema<'value>) => 'value,
    // Public API for ReScript users
    ...Object.s,
  }

  module Definition = {
    type t<'embeded>

    @inline
    let isNode = (definition: 'any) =>
      definition->Type.typeof === #object && definition !== %raw(`null`)

    @inline
    let toEmbededItem = (definition: t<'embeded>): option<'embeded> =>
      definition->Obj.magic->X.Dict.getUnsafeOptionBySymbol(itemSymbol)
  }

  let rec proxifyShapedSchema = (schema: internal, ~from, ~fromFlattened=?): 'a => {
    let mut = schema->getOutputSchema->copySchema
    mut.from = Some(from)
    switch fromFlattened {
    | Some(index) => mut.fromFlattened = Some(index)
    | None => ()
    }
    mut
    ->X.Proxy.make({
      get: (~target, ~prop) => {
        if prop === itemSymbol->Obj.magic {
          target->Obj.magic
        } else {
          let location = prop->(Obj.magic: unknown => string)

          {
            let maybeField = switch target {
            | {properties} => properties->X.Dict.getUnsafeOption(location)
            // If there are no properties, then it must be Tuple
            | {items} => items->X.Array.getUnsafeOptionByString(location)
            | _ => None
            }
            if maybeField === None {
              InternalError.panic(
                `Cannot read property "${location}" of ${target
                  ->castToPublic
                  ->toExpression}`,
              )
            }
            maybeField->X.Option.getUnsafe
          }
          ->proxifyShapedSchema(
            ~from=target.from->X.Option.getUnsafe->X.Array.append(location),
            ~fromFlattened=?target.fromFlattened,
          )
          ->Obj.magic
        }
      },
    })
    ->Obj.magic
  }

  let rec shape = {
    (schema: t<'value>, definer: 'value => 'variant): t<'variant> => {
      let schema = schema->castToInternal
      schema->updateOutput(mut => {
        let fromProxy = mut->proxifyShapedSchema(~from=inputFrom)
        let definition: unknown = definer(fromProxy)->Obj.magic
        if definition === fromProxy {
          ()
        } else {
          mut.parser = Some(shapedParser)
          mut.to = Some(definitionToShapedSchema(definition))
        }
      })
    }
  }
  and nested = fieldName => {
    let parentCtx = %raw(`this`) // TODO: Add a check that it's binded?
    let cacheId = `~${fieldName}`

    switch parentCtx->X.Dict.getUnsafeOption(cacheId) {
    | Some(ctx) => ctx
    | None => {
        let properties = Js.Dict.empty()

        let schema = {
          let schema = base(objectTag)
          schema.properties = Some(properties)
          schema.additionalItems = Some(globalConfig.defaultAdditionalItems)
          schema.decoder = objectDecoder
          schema->castToPublic
        }

        let parentSchema = (
          parentCtx.field(fieldName, schema)
          ->Definition.toEmbededItem
          ->X.Option.getUnsafe: internal
        )

        let field:
          type value. (string, schema<value>) => value =
          (fieldName, schema) => {
            let schema = schema->castToInternal
            let inlinedLocation = fieldName->X.Inlined.Value.fromString
            if properties->Stdlib.Dict.has(fieldName) {
              InternalError.panic(`The field ${inlinedLocation} defined twice`)
            }
            properties->Js.Dict.set(fieldName, schema)
            schema->proxifyShapedSchema(
              ~from=parentSchema.from->X.Option.getUnsafe->X.Array.append(fieldName),
              ~fromFlattened=?parentSchema.fromFlattened,
            )
          }

        let tag = (tag, asValue) => {
          let _ = field(tag, definitionToSchema(asValue->Obj.magic)->castToPublic)
        }

        let fieldOr = (fieldName, schema, or) => {
          field(fieldName, Option.factory(schema)->Option.getOr(or))
        }

        let flatten = schema => {
          let schema = schema->castToInternal
          switch schema {
          | {tag: Object, properties: ?flattenedProperties, ?to} => {
              if to->Obj.magic {
                InternalError.panic(
                  `Unsupported nested flatten for transformed object schema ${schema
                    ->castToPublic
                    ->toExpression}`,
                )
              }
              let flattenedProperties = flattenedProperties->X.Option.getUnsafe
              let flattenedKeys = flattenedProperties->Js.Dict.keys
              let result = Js.Dict.empty()
              for idx in 0 to flattenedKeys->Js.Array2.length - 1 {
                let key = flattenedKeys->Js.Array2.unsafe_get(idx)
                result->Js.Dict.set(
                  key,
                  field(key, flattenedProperties->Js.Dict.unsafeGet(key)->castToPublic),
                )
              }
              result->Obj.magic
            }
          | _ => InternalError.panic(`Can't flatten ${schema->castToPublic->toExpression} schema`)
          }
        }

        let ctx: advancedObjectCtx = {
          // js/ts methods
          _jsField: field,
          // methods
          field,
          fieldOr,
          tag,
          nested,
          flatten,
        }

        parentCtx->Js.Dict.set(cacheId, ctx)

        (ctx :> Object.s)
      }
    }
  }
  and object:
    type value. (Object.s => value) => schema<value> =
    definer => {
      let flattened: option<array<internal>> = %raw(`void 0`)
      let properties = Js.Dict.empty()

      let flatten = schema => {
        let schema = schema->castToInternal
        switch schema {
        | {tag: Object, properties: ?flattenedProperties} => {
            let flattenedProperties = flattenedProperties->X.Option.getUnsafe
            let flattenedKeys = flattenedProperties->Js.Dict.keys
            for idx in 0 to flattenedKeys->Js.Array2.length - 1 {
              let key = flattenedKeys->Js.Array2.unsafe_get(idx)
              let flattenedSchema = flattenedProperties->Js.Dict.unsafeGet(key)
              switch properties->X.Dict.getUnsafeOption(key) {
              | Some(schema) if schema === flattenedSchema => ()
              | Some(_) =>
                InternalError.panic(`The field "${key}" defined twice with incompatible schemas`)
              | None => properties->Js.Dict.set(key, flattenedSchema)
              }
            }
            let f = %raw(`flattened || (flattened = [])`)
            schema->proxifyShapedSchema(
              ~from=inputFrom,
              ~fromFlattened=f->Js.Array2.push(schema) - 1,
            )
          }
        | _ =>
          InternalError.panic(
            `The '${schema->castToPublic->toExpression}' schema can't be flattened`,
          )
        }
      }

      let field:
        type value. (string, schema<value>) => value =
        (fieldName, schema) => {
          let schema = schema->castToInternal

          if properties->Stdlib.Dict.has(fieldName) {
            InternalError.panic(`The field "${fieldName}" defined twice with incompatible schemas`)
          }
          properties->Js.Dict.set(fieldName, schema)
          schema->proxifyShapedSchema(~from=[fieldName])
        }

      let tag = (tag, asValue) => {
        let _ = field(tag, definitionToSchema(asValue->Obj.magic)->castToPublic)
      }

      let fieldOr = (fieldName, schema, or) => {
        field(fieldName, Option.factory(schema)->Option.getOr(or))
      }

      let ctx = {
        // js/ts methods
        _jsField: field,
        // methods
        field,
        fieldOr,
        tag,
        nested,
        flatten,
      }

      let definition = definer((ctx :> Object.s))->(Obj.magic: value => unknown)

      let mut = base(objectTag)
      mut.properties = Some(properties)
      mut.additionalItems = Some(globalConfig.defaultAdditionalItems)
      mut.decoder = objectDecoder
      mut.parser = Some(shapedParser)
      mut.to = Some(definitionToShapedSchema(definition))
      if flattened !== None {
        mut.flattened = flattened
      }
      mut->castToPublic
    }
  and tuple = definer => {
    let items = []

    let ctx: Tuple.s = {
      let item:
        type value. (int, schema<value>) => value =
        (idx, schema) => {
          let schema = schema->castToInternal
          let location = idx->Js.Int.toString
          if items->X.Array.has(idx) {
            InternalError.panic(`The item [${location}] is defined multiple times`)
          } else {
            items->Js.Array2.unsafe_set(idx, schema)
            schema->proxifyShapedSchema(~from=[idx->Js.Int.toString])
          }
        }

      let tag = (idx, asValue) => {
        let _ = item(idx, definitionToSchema(asValue->Obj.magic)->castToPublic)
      }

      {
        item,
        tag,
      }
    }
    let definition = definer(ctx)->(Obj.magic: 'any => unknown)

    for idx in 0 to items->Js.Array2.length - 1 {
      if items->Js.Array2.unsafe_get(idx)->Obj.magic->not {
        items->Js.Array2.unsafe_set(idx, unit)
      }
    }

    let mut = base(arrayTag)
    mut.items = Some(items)
    mut.additionalItems = Some(Strict)
    mut.decoder = arrayDecoder
    mut.parser = Some(shapedParser)
    mut.to = Some(definitionToShapedSchema(definition))
    mut->castToPublic
  }
  and getValByFrom = (~input, ~from, ~idx) => {
    // FIXME: TODO: something with flattened
    switch from->X.Array.getUnsafeOption(idx) {
    | Some(key) =>
      getValByFrom(
        ~input=input.vals->X.Option.getUnsafe->Js.Dict.unsafeGet(key),
        ~from,
        ~idx=idx + 1,
      )
    | None => input
    }
  }
  and getShapedParserOutput = (~input, ~targetSchema) => {
    let v = switch targetSchema {
    | {fromFlattened} =>
      getValByFrom(
        ~input=input.flattenedVals->X.Option.getUnsafe->Js.Array2.unsafe_get(fromFlattened),
        ~from=targetSchema.from->X.Option.getUnsafe,
        ~idx=0,
      )->B.Val.cleanValFrom
    | {from} => getValByFrom(~input, ~from, ~idx=0)->B.Val.cleanValFrom
    | _ =>
      if targetSchema->isLiteral {
        input->B.newConst(~schema=targetSchema)
      } else {
        let output = makeObjectVal(input, ~schema=targetSchema)
        switch targetSchema {
        | {items} =>
          for idx in 0 to items->Js.Array2.length - 1 {
            let location = idx->Js.Int.toString
            output->B.Val.Object.add(
              ~location,
              getShapedParserOutput(~input, ~targetSchema=items->Js.Array2.unsafe_get(idx)),
            )
          }
        | {properties} => {
            let keys = properties->Js.Dict.keys
            for idx in 0 to keys->Js.Array2.length - 1 {
              let location = keys->Js.Array2.unsafe_get(idx)
              output->B.Val.Object.add(
                ~location,
                getShapedParserOutput(
                  ~input,
                  ~targetSchema=properties->Js.Dict.unsafeGet(location),
                ),
              )
            }
          }
        | _ =>
          // FIXME: Use a path
          InternalError.panic(
            `Don't know where the value is coming from: ${targetSchema
              ->castToPublic
              ->toExpression}`,
          )
        }
        output->B.Val.Object.complete
      }
    }
    v.from = None
    v
  }
  and shapedParser = (~input, ~selfSchema) => {
    switch selfSchema.flattened {
    | Some(flattened) =>
      let flattenedVals = []
      for idx in 0 to flattened->Js.Array2.length - 1 {
        let flattenedSchema = flattened->Js.Array2.unsafe_get(idx)
        let flattenedInput = input->B.Val.cleanValFrom
        flattenedInput.expected = flattenedSchema
        flattenedVals->Js.Array2.push(flattenedInput->parse)->ignore
      }
      input.flattenedVals = Some(flattenedVals)
    | None => ()
    }

    let targetSchema = selfSchema.to->X.Option.getUnsafe
    let output = getShapedParserOutput(~input, ~targetSchema)
    output.from = Some(input)
    output.skipTo = Some(targetSchema.to === None)
    output
  }

  and prepareShapedSerializerAcc = (~acc: shapedSerializerAcc, ~input: val) => {
    switch input {
    | {expected: {from, ?fromFlattened}} =>
      let accAtFrom = ref(
        switch fromFlattened {
        | Some(idx) => {
            if acc.flattened === None {
              acc.flattened = Some([])
            }
            switch acc.flattened->X.Option.getUnsafe->X.Array.getUnsafeOption(idx) {
            | None => {
                let newAcc: shapedSerializerAcc = {}
                acc.flattened->X.Option.getUnsafe->Js.Array2.unsafe_set(idx, newAcc)
                newAcc
              }
            | Some(acc) => acc
            }
          }
        | None => acc
        },
      )
      for idx in 0 to from->Js.Array2.length - 1 {
        let key = from->Js.Array2.unsafe_get(idx)
        let p = switch accAtFrom.contents.properties {
        | Some(p) => p
        | None => {
            let p = Js.Dict.empty()

            accAtFrom.contents.properties = Some(p)
            p
          }
        }
        accAtFrom :=
          switch p->X.Dict.getUnsafeOption(key) {
          | Some(acc) => acc
          | None => {
              let newAcc: shapedSerializerAcc = {}
              p->Js.Dict.set(key, newAcc)
              newAcc
            }
          }
      }
      accAtFrom.contents.val = Some(input)
    | {vals} => {
        let keys = vals->Js.Dict.keys
        for idx in 0 to keys->Js.Array2.length - 1 {
          prepareShapedSerializerAcc(
            ~acc,
            ~input=vals->Js.Dict.unsafeGet(keys->Js.Array2.unsafe_get(idx)),
          )
        }
      }
    | _ => ()
    }
  }
  and getShapedSerializerOutput = (
    ~cleanRootInput,
    ~acc: option<shapedSerializerAcc>,
    ~targetSchema: internal,
    ~path,
  ) => {
    switch acc {
    | Some({val}) => {
        let v = val->B.Val.cleanValFrom
        v.expected = targetSchema // FIXME: Is this line needed?
        // Use parse to walk through all possible transformations
        v->parse
      }
    | _ =>
      if targetSchema->isLiteral {
        let v = cleanRootInput->B.newConst(~schema=targetSchema)
        v.from = None
        v.expected = targetSchema // FIXME: Is this line needed?
        v->parse
      } else {
        let output = makeObjectVal(cleanRootInput, ~schema=targetSchema)
        output.from = None
        switch targetSchema {
        | {items} =>
          for idx in 0 to items->Js.Array2.length - 1 {
            let location = idx->Js.Int.toString
            output->B.Val.Object.add(
              ~location,
              getShapedSerializerOutput(
                ~cleanRootInput,
                ~acc=switch acc {
                | Some({properties}) => properties->X.Dict.getUnsafeOption(location)
                | _ => None
                },
                ~targetSchema=items->Js.Array2.unsafe_get(idx),
                ~path=path->Path.concat(
                  Path.fromInlinedLocation(cleanRootInput.global->B.inlineLocation(location)),
                ),
              ),
            )
          }
        | {properties, ?flattened} => {
            switch (flattened, acc) {
            | (Some(flattenedSchemas), Some({flattened: flattenedAcc})) =>
              flattenedAcc->Js.Array2.forEachi((acc, idx) => {
                let flattenedOutput = getShapedSerializerOutput(
                  ~cleanRootInput,
                  ~acc=Some(acc),
                  ~targetSchema=flattenedSchemas->Js.Array2.unsafe_get(idx)->reverse,
                  ~path,
                )
                output->B.Val.Object.merge(flattenedOutput.vals->X.Option.getUnsafe)
              })
            | _ => ()
            }

            let keys = properties->Js.Dict.keys
            for idx in 0 to keys->Js.Array2.length - 1 {
              let location = keys->Js.Array2.unsafe_get(idx)

              // Skip fields added by flattened
              if !(output.vals->X.Option.getUnsafe->Stdlib.Dict.has(location)) {
                output->B.Val.Object.add(
                  ~location,
                  getShapedSerializerOutput(
                    ~cleanRootInput,
                    ~acc=switch acc {
                    | Some({properties}) => properties->X.Dict.getUnsafeOption(location)
                    | _ => None
                    },
                    ~targetSchema=properties->Js.Dict.unsafeGet(location),
                    ~path=path->Path.concat(
                      Path.fromInlinedLocation(cleanRootInput.global->B.inlineLocation(location)),
                    ),
                  ),
                )
              }
            }
          }
        | _ =>
          let path = switch targetSchema.from {
          | Some(from) => path ++ from->Js.Array2.map(item => `["${item}"]`)->Js.Array2.joinWith("")
          | None => path
          }
          cleanRootInput->B.invalidOperation(
            ~description={
              `Missing input for ${targetSchema->castToPublic->toExpression}` ++
              switch path {
              | "" => ""
              | _ => ` at ${path}`
              }
            },
          )
        }

        output->B.Val.Object.complete
      }
    }
  }
  and shapedSerializer = (~input, ~selfSchema) => {
    let acc: shapedSerializerAcc = {}
    prepareShapedSerializerAcc(~acc, ~input)

    let targetSchema = selfSchema.to->X.Option.getUnsafe
    let output = getShapedSerializerOutput(
      ~cleanRootInput=input->B.Val.cleanValFrom,
      ~acc=Some(acc),
      ~targetSchema,
      ~path=Path.empty,
    )

    output.from = Some(input)
    output.skipTo = Some(targetSchema.to === None)

    output
  }

  and definitionToShapedSchema = definition => {
    let s =
      definition
      ->traverseDefinition(
        ~onNode=Definition.toEmbededItem->(
          Obj.magic: (Definition.t<internal> => option<internal>) => unknown => option<internal>
        ),
      )
      ->copySchema
    s.serializer = Some(shapedSerializer)
    s
  }
  and definitionToSchema = definition =>
    definition->traverseDefinition(~onNode=node => {
      if node->isSchemaObject {
        node->(Obj.magic: unknown => option<internal>)
      } else {
        None
      }
    })
  and traverseDefinition = (definition: unknown, ~onNode): internal => {
    if definition->Definition.isNode {
      switch onNode(definition) {
      | Some(s) => s
      | None =>
        if definition->X.Array.isArray {
          let node = definition->(Obj.magic: unknown => array<unknown>)
          for idx in 0 to node->Js.Array2.length - 1 {
            let schema = node->Js.Array2.unsafe_get(idx)->traverseDefinition(~onNode)
            node->Js.Array2.unsafe_set(idx, schema->(Obj.magic: internal => unknown))
          }
          let items = node->(Obj.magic: array<unknown> => array<internal>)

          let mut = base(arrayTag)
          mut.items = Some(items)
          mut.additionalItems = Some(Strict)
          mut.decoder = arrayDecoder
          mut
        } else {
          let cnstr = (definition->Obj.magic)["constructor"]
          if cnstr->Obj.magic && cnstr !== %raw(`Object`) {
            let mut = base(instanceTag)
            mut.class = cnstr
            mut.const = definition->Obj.magic
            mut.decoder = Literal.literalDecoder
            mut
          } else {
            let node = definition->(Obj.magic: unknown => dict<unknown>)
            let fieldNames = node->Js.Dict.keys
            let length = fieldNames->Js.Array2.length
            for idx in 0 to length - 1 {
              let location = fieldNames->Js.Array2.unsafe_get(idx)
              let schema = node->Js.Dict.unsafeGet(location)->traverseDefinition(~onNode)
              node->Js.Dict.set(location, schema->(Obj.magic: internal => unknown))
            }
            let mut = base(objectTag)
            mut.properties = Some(node->(Obj.magic: dict<unknown> => dict<internal>))
            mut.additionalItems = Some(globalConfig.defaultAdditionalItems)
            mut.decoder = objectDecoder
            mut
          }
        }
      }
    } else {
      Literal.parse(definition)
    }
  }

  let matches:
    type value. schema<value> => value =
    schema => schema->(Obj.magic: schema<value> => value)
  let ctx = {
    matches: matches,
  }
  let factory = definer => {
    definer(ctx->(Obj.magic: s => 'value))
    ->(Obj.magic: 'definition => unknown)
    ->definitionToSchema
    ->castToPublic
  }
}

module Null = {
  let factory = item => {
    Option.factory(item, ~unit=nullAsUnit)
  }
}

let schema = Schema.factory

let js_schema = definition => definition->Obj.magic->Schema.definitionToSchema->castToPublic
let literal = js_schema

let enum = values => Union.factory(values->Js.Array2.map(literal))

let unnestSerializer = Builder.make((~input, ~selfSchema) => {
  let schema = selfSchema.additionalItems->(Obj.magic: option<additionalItems> => internal)
  let items = schema.items->X.Option.getUnsafe

  let inputVar = input.var()
  let iteratorVar = input.global->B.varWithoutAllocation
  let outputVar = input.global->B.varWithoutAllocation

  let b = input

  // let bb = b->B.scope(~path=b.path)
  let bb = b
  let itemInput = {
    ...bb,
    // FIXME: This is probably wrong
    var: B._var,
    inline: `${inputVar}[${iteratorVar}]`,
    flag: ValFlag.none,
    schema: unknown, // FIXME:
  }
  let itemOutput = bb->B.withPathPrepend(
    ~input=itemInput,
    ~dynamicLocationVar=iteratorVar,
    ~appendSafe=(bb, ~output) => {
      let initialArraysCode = ref("")
      let settingCode = ref("")
      for idx in 0 to items->Js.Array2.length - 1 {
        let toItem = items->Js.Array2.unsafe_get(idx)
        initialArraysCode := initialArraysCode.contents ++ `new Array(${inputVar}.length),`
        settingCode :=
          settingCode.contents ++
          `${outputVar}[${idx->X.Int.unsafeToString}][${iteratorVar}]=${(
              output->B.Val.get("toItem.location") // FIXME:
            ).inline};`
      }
      b.allocate(`${outputVar}=[${initialArraysCode.contents}]`)
      bb.code = bb.code ++ settingCode.contents
    },
    (b, ~input) => {
      // b->parse(~schema, ~input)
      b->parse
    },
  )
  let itemCode = bb->B.merge

  b.code =
    b.code ++
    `for(let ${iteratorVar}=0;${iteratorVar}<${inputVar}.length;++${iteratorVar}){${itemCode}}`

  if itemOutput.flag->Flag.unsafeHas(ValFlag.async) {
    {
      ...b,
      // FIXME: This is probably wrong
      var: B._notVar,
      inline: `Promise.all(${outputVar})`,
      flag: ValFlag.async,
      schema: base(arrayTag), // FIXME: full schema
    }
  } else {
    {
      ...b,
      // FIXME: This is probably wrong
      var: B._var,
      inline: outputVar,
      flag: ValFlag.none,
      schema: base(arrayTag), // FIXME: full schema
    }
  }
})

let unnest = schema => {
  switch schema {
  | Object({properties}) =>
    let keys = properties->Js.Dict.keys
    if keys->Js.Array2.length === 0 {
      InternalError.panic("Invalid empty object for S.unnest schema.")
    }
    let schema = schema->castToInternal
    let mut = base(arrayTag)
    mut.items = Some(
      keys->Js.Array2.map(key => {
        array(properties->Js.Dict.unsafeGet(key))->castToInternal
      }),
    )
    mut.additionalItems = Some(Strict)
    mut.parser = Some(
      Builder.make((~input, ~selfSchema) => {
        let b = input
        let inputTagFlag = input.schema.tag->TagFlag.get
        if inputTagFlag->Flag.unsafeHas(TagFlag.unknown) {
          b.validation = Some(
            (~inputVar, ~negative) => {
              `${B.exp(~negative)}Array.isArray(${inputVar})` ++
              `${B.and_(~negative)}${inputVar}.length${B.eq(~negative)}${keys
                ->Js.Array2.length
                ->X.Int.unsafeToString}` ++
              `${keys
                ->Js.Array2.mapi((_, idx) => {
                  `${B.and_(~negative)}${B.exp(
                      ~negative,
                    )}Array.isArray(${inputVar}[${idx->X.Int.unsafeToString}])`
                })
                ->Js.Array2.joinWith("")}`
            },
          )
          let mut = base(arrayTag)
          let itemSchema = array(unknown->castToPublic)
          mut.items = Some(
            keys->Js.Array2.map(_ => {
              itemSchema->castToInternal
            }),
          )
          mut.additionalItems = Some(Strict)
          input.schema = mut
        } else if (
          inputTagFlag->Flag.unsafeHas(TagFlag.array) &&
          input.schema.items->X.Option.getUnsafe->Js.Array2.length === keys->Js.Array2.length &&
          input.schema.items
          ->X.Option.getUnsafe
          ->Js.Array2.every(s =>
            s.tag === arrayTag &&
              switch s.additionalItems {
              | Some(Schema(_)) => true
              | _ => false
              }
          )
        ) {
          ()
        } else {
          b->B.unsupportedTransform(~from=input.schema, ~target=selfSchema)
        }

        let inputVar = input.var()
        let iteratorVar = b.global->B.varWithoutAllocation

        // let bb = b->B.scope(~path=b.path) // FIXME: The path looks wrong here
        let bb = b
        let itemInput = bb->makeObjectVal(~schema)
        let lengthCode = ref("")
        for idx in 0 to keys->Js.Array2.length - 1 {
          let key = keys->Js.Array2.unsafe_get(idx)
          itemInput->B.Val.Object.add(
            ~location=key,
            bb->B.val(
              `${inputVar}[${idx->X.Int.unsafeToString}][${iteratorVar}]`,
              ~schema=(
                input.schema.items->X.Option.getUnsafe->Js.Array2.unsafe_get(idx)
              ).additionalItems->(Obj.magic: option<additionalItems> => internal),
            ),
          )
          lengthCode := lengthCode.contents ++ `${inputVar}[${idx->X.Int.unsafeToString}].length,`
        }

        let output =
          b->B.val(
            `new Array(Math.max(${lengthCode.contents}))`,
            ~schema=selfSchema.to->X.Option.getUnsafe,
          )
        let outputVar = B.Val.var(output)

        let itemInput = itemInput->B.Val.Object.complete
        let itemOutput = bb->B.withPathPrepend(
          ~input=itemInput,
          ~dynamicLocationVar=iteratorVar,
          ~appendSafe=(bb, ~output as itemOutput) => {
            bb.code = bb.code ++ output->B.Val.addKey(iteratorVar, itemOutput) ++ ";"
          },
          (b, ~input) => {
            // b->parse(~schema, ~input)
            b->parse
          },
        )
        let itemCode = bb->B.merge

        b.code =
          b.code ++
          `for(let ${iteratorVar}=0;${iteratorVar}<${outputVar}.length;++${iteratorVar}){${itemCode}}`

        if itemOutput.flag->Flag.unsafeHas(ValFlag.async) {
          output->B.asyncVal(`Promise.all(${output.inline})`)
        } else {
          output
        }
      }),
    )

    let to = base(arrayTag)
    to.items = Some(X.Array.immutableEmpty)
    to.additionalItems = Some(Schema(schema->castToPublic))
    to.serializer = Some(unnestSerializer)

    mut.unnest = Some(true)
    mut.to = Some(to)

    mut->castToPublic
  | _ => InternalError.panic("S.unnest supports only object schemas.")
  }
}

// let inline = {
//   let rec internalInline = (schema, ~variant as maybeVariant=?, ()) => {
//     let mut = schema->castToInternal->copy

//     let inlinedSchema = switch mut {
//     | {?const} if isLiteral(mut) => `S.literal(%raw(\`${literal->Literal.toString}\`))`
//     | {anyOf} => {
//         let variantNamesCounter = Js.Dict.empty()
//         `S.union([${anyOf
//           ->Js.Array2.map(s => {
//             let variantName = s.name()
//             let numberOfVariantNames = switch variantNamesCounter->Js.Dict.get(variantName) {
//             | Some(n) => n
//             | None => 0
//             }
//             variantNamesCounter->Js.Dict.set(variantName, numberOfVariantNames->X.Int.plus(1))
//             let variantName = switch numberOfVariantNames {
//             | 0 => variantName
//             | _ =>
//               variantName ++ numberOfVariantNames->X.Int.plus(1)->X.Int.unsafeToString
//             }
//             let inlinedVariant = `#${variantName->X.Inlined.Value.fromString}`
//             s->internalInline(~variant=inlinedVariant, ())
//           })
//           ->Js.Array2.joinWith(", ")}])`
//       }
//     | {tag: JSON} => `S.json(~validate=${validated->(Obj.magic: bool => string)})`
//     | {tag: TupleTuple({items: [s0]}) => `S.tuple1(${s0.schema->internalInline()})`
//     | Tuple({items: [s0, s1]}) =>
//       `S.tuple2(${s0.schema->internalInline()}, ${s1.schema->internalInline()})`
//     | Tuple({items: [s0, s1, s2]}) =>
//       `S.tuple3(${s0.schema->internalInline()}, ${s1.schema->internalInline()}, ${s2.schema->internalInline()})`
//     | Tuple({items}) =>
//       `S.tuple(s => (${items
//         ->Js.Array2.mapi((schema, idx) =>
//           `s.item(${idx->X.Int.unsafeToString}, ${schema.schema->internalInline()})`
//         )
//         ->Js.Array2.joinWith(", ")}))`
//     | Object({items: []}) => `S.object(_ => ())`
//     | Object({items}) =>
//       `S.object(s =>
//   {
//     ${items
//         ->Js.Array2.map(item => {
//           `${item.inlinedLocation}: s.field(${item.inlinedLocation}, ${item.schema->internalInline()})`
//         })
//         ->Js.Array2.joinWith(",\n    ")},
//   }
// )`
//     | String => `S.string`
//     | Int => `S.int`
//     | Float => `S.float`
//     | BigInt => `S.bigint`
//     | Bool => `S.bool`
//     | Option(schema) => `S.option(${schema->internalInline()})`
//     | Null(schema) => `S.null(${schema->internalInline()})`
//     | Never => `S.never`
//     | Unknown => `S.unknown`
//     | Array(schema) => `S.array(${schema->internalInline()})`
//     | Dict(schema) => `S.dict(${schema->internalInline()})`
//     }

//     let inlinedSchema = switch schema->Option.default {
//     | Some(default) => {
//         metadataMap->X.Dict.deleteInPlace(Option.defaultMetadataId->Metadata.Id.toKey)
//         switch default {
//         | Value(defaultValue) =>
//           inlinedSchema ++
//           `->S.Option.getOr(%raw(\`${defaultValue->X.Inlined.Value.stringify}\`))`
//         | Callback(defaultCb) =>
//           inlinedSchema ++
//           `->S.Option.getOrWith(() => %raw(\`${defaultCb()->X.Inlined.Value.stringify}\`))`
//         }
//       }

//     | None => inlinedSchema
//     }

//     let inlinedSchema = switch schema->deprecation {
//     | Some(message) => {
//         metadataMap->X.Dict.deleteInPlace(deprecationMetadataId->Metadata.Id.toKey)
//         inlinedSchema ++ `->S.deprecate(${message->X.Inlined.Value.fromString})`
//       }

//     | None => inlinedSchema
//     }

//     let inlinedSchema = switch schema->description {
//     | Some(message) => {
//         metadataMap->X.Dict.deleteInPlace(descriptionMetadataId->Metadata.Id.toKey)
//         inlinedSchema ++ `->S.describe(${message->X.Inlined.Value.stringify})`
//       }

//     | None => inlinedSchema
//     }

//     let inlinedSchema = switch schema->classify {
//     | Object({additionalItems: Strict}) => inlinedSchema ++ `->S.strict`
//     | _ => inlinedSchema
//     }

//     let inlinedSchema = switch schema->classify {
//     | String
//     | Literal(String(_)) =>
//       switch schema->String.refinements {
//       | [] => inlinedSchema
//       | refinements =>
//         metadataMap->X.Dict.deleteInPlace(String.Refinement.metadataId->Metadata.Id.toKey)
//         inlinedSchema ++
//         refinements
//         ->Js.Array2.map(refinement => {
//           switch refinement {
//           | {kind: Email, message} =>
//             `->S.email(~message=${message->X.Inlined.Value.fromString})`
//           | {kind: Url, message} => `->S.url(~message=${message->X.Inlined.Value.fromString})`
//           | {kind: Uuid, message} =>
//             `->S.uuid(~message=${message->X.Inlined.Value.fromString})`
//           | {kind: Cuid, message} =>
//             `->S.cuid(~message=${message->X.Inlined.Value.fromString})`
//           | {kind: Min({length}), message} =>
//             `->S.stringMinLength(${length->X.Int.unsafeToString}, ~message=${message->X.Inlined.Value.fromString})`
//           | {kind: Max({length}), message} =>
//             `->S.stringMaxLength(${length->X.Int.unsafeToString}, ~message=${message->X.Inlined.Value.fromString})`
//           | {kind: Length({length}), message} =>
//             `->S.stringLength(${length->X.Int.unsafeToString}, ~message=${message->X.Inlined.Value.fromString})`
//           | {kind: Pattern({re}), message} =>
//             `->S.pattern(%re(${re
//               ->X.Re.toString
//               ->X.Inlined.Value.fromString}), ~message=${message->X.Inlined.Value.fromString})`
//           | {kind: Datetime, message} =>
//             `->S.datetime(~message=${message->X.Inlined.Value.fromString})`
//           }
//         })
//         ->Js.Array2.joinWith("")
//       }
//     | Int =>
//       // | Literal(Int(_)) ???
//       switch schema->Int.refinements {
//       | [] => inlinedSchema
//       | refinements =>
//         metadataMap->X.Dict.deleteInPlace(Int.Refinement.metadataId->Metadata.Id.toKey)
//         inlinedSchema ++
//         refinements
//         ->Js.Array2.map(refinement => {
//           switch refinement {
//           | {kind: Max({value}), message} =>
//             `->S.intMax(${value->X.Int.unsafeToString}, ~message=${message->X.Inlined.Value.fromString})`
//           | {kind: Min({value}), message} =>
//             `->S.intMin(${value->X.Int.unsafeToString}, ~message=${message->X.Inlined.Value.fromString})`
//           | {kind: Port, message} =>
//             `->S.port(~message=${message->X.Inlined.Value.fromString})`
//           }
//         })
//         ->Js.Array2.joinWith("")
//       }
//     | Float =>
//       // | Literal(Float(_)) ???
//       switch schema->Float.refinements {
//       | [] => inlinedSchema
//       | refinements =>
//         metadataMap->X.Dict.deleteInPlace(Float.Refinement.metadataId->Metadata.Id.toKey)
//         inlinedSchema ++
//         refinements
//         ->Js.Array2.map(refinement => {
//           switch refinement {
//           | {kind: Max({value}), message} =>
//             `->S.floatMax(${value->X.Inlined.Float.toRescript}, ~message=${message->X.Inlined.Value.fromString})`
//           | {kind: Min({value}), message} =>
//             `->S.floatMin(${value->X.Inlined.Float.toRescript}, ~message=${message->X.Inlined.Value.fromString})`
//           }
//         })
//         ->Js.Array2.joinWith("")
//       }

//     | Array(_) =>
//       switch schema->Array.refinements {
//       | [] => inlinedSchema
//       | refinements =>
//         metadataMap->X.Dict.deleteInPlace(Array.Refinement.metadataId->Metadata.Id.toKey)
//         inlinedSchema ++
//         refinements
//         ->Js.Array2.map(refinement => {
//           switch refinement {
//           | {kind: Max({length}), message} =>
//             `->S.arrayMaxLength(${length->X.Int.unsafeToString}, ~message=${message->X.Inlined.Value.fromString})`
//           | {kind: Min({length}), message} =>
//             `->S.arrayMinLength(${length->X.Int.unsafeToString}, ~message=${message->X.Inlined.Value.fromString})`
//           | {kind: Length({length}), message} =>
//             `->S.arrayLength(${length->X.Int.unsafeToString}, ~message=${message->X.Inlined.Value.fromString})`
//           }
//         })
//         ->Js.Array2.joinWith("")
//       }

//     | _ => inlinedSchema
//     }

//     let inlinedSchema = if metadataMap->Js.Dict.keys->Js.Array2.length !== 0 {
//       `{
//   let s = ${inlinedSchema}
//   let _ = %raw(\`s.m = ${metadataMap->Js.Json.stringifyAny->Belt.Option.getUnsafe}\`)
//   s
// }`
//     } else {
//       inlinedSchema
//     }

//     let inlinedSchema = switch maybeVariant {
//     | Some(variant) => inlinedSchema ++ `->S.shape(v => ${variant}(v))`
//     | None => inlinedSchema
//     }

//     inlinedSchema
//   }

//   schema => {
//     schema->castToUnknown->internalInline()
//   }
// }

let object = Schema.object
let null = Null.factory
let option = item => item->Option.factory(~unit=unit->castToPublic)
let array = array
let dict = Dict.factory
let shape = Schema.shape
let tuple = Schema.tuple
let tuple1 = v0 => tuple(s => s.item(0, v0))
let tuple2 = (v0, v1) =>
  Schema.definitionToSchema([v0->castToUnknown, v1->castToUnknown]->Obj.magic)->castToPublic
let tuple3 = (v0, v1, v2) =>
  Schema.definitionToSchema(
    [v0->castToUnknown, v1->castToUnknown, v2->castToUnknown]->Obj.magic,
  )->castToPublic
let union = Union.factory

// =============
// Built-in refinements
// =============

let intMin = (schema, minValue, ~message as maybeMessage=?) => {
  let message = switch maybeMessage {
  | Some(m) => m
  | None => `Number must be greater than or equal to ${minValue->X.Int.unsafeToString}`
  }
  schema->addRefinement(
    ~metadataId=Int.Refinement.metadataId,
    ~refiner=(b, ~inputVar, ~selfSchema as _) => {
      `if(${inputVar}<${b->B.embed(minValue)}){${b->B.fail(~message)}}`
    },
    ~refinement={
      kind: Min({value: minValue}),
      message,
    },
  )
}

let intMax = (schema, maxValue, ~message as maybeMessage=?) => {
  let message = switch maybeMessage {
  | Some(m) => m
  | None => `Number must be lower than or equal to ${maxValue->X.Int.unsafeToString}`
  }
  schema->addRefinement(
    ~metadataId=Int.Refinement.metadataId,
    ~refiner=(b, ~inputVar, ~selfSchema as _) => {
      `if(${inputVar}>${b->B.embed(maxValue)}){${b->B.fail(~message)}}`
    },
    ~refinement={
      kind: Max({value: maxValue}),
      message,
    },
  )
}

let port = (schema, ~message=?) => {
  schema->internalRefine(mut => {
    mut.format = Some(Port)
    (b, ~inputVar, ~selfSchema) => {
      `${inputVar}>0&&${inputVar}<65536&&${inputVar}%1===0||${switch message {
        | Some(m) => b->B.fail(~message=m)
        | None => b->B.embedInvalidTypeWithVarOnly(~expected=selfSchema, ~inputVar)
        }};`
    }
  })
}

let floatMin = (schema, minValue, ~message as maybeMessage=?) => {
  let message = switch maybeMessage {
  | Some(m) => m
  | None => `Number must be greater than or equal to ${minValue->X.Float.unsafeToString}`
  }
  schema->addRefinement(
    ~metadataId=Float.Refinement.metadataId,
    ~refiner=(b, ~inputVar, ~selfSchema as _) => {
      `if(${inputVar}<${b->B.embed(minValue)}){${b->B.fail(~message)}}`
    },
    ~refinement={
      kind: Min({value: minValue}),
      message,
    },
  )
}

let floatMax = (schema, maxValue, ~message as maybeMessage=?) => {
  let message = switch maybeMessage {
  | Some(m) => m
  | None => `Number must be lower than or equal to ${maxValue->X.Float.unsafeToString}`
  }
  schema->addRefinement(
    ~metadataId=Float.Refinement.metadataId,
    ~refiner=(b, ~inputVar, ~selfSchema as _) => {
      `if(${inputVar}>${b->B.embed(maxValue)}){${b->B.fail(~message)}}`
    },
    ~refinement={
      kind: Max({value: maxValue}),
      message,
    },
  )
}

let arrayMinLength = (schema, length, ~message as maybeMessage=?) => {
  let message = switch maybeMessage {
  | Some(m) => m
  | None => `Array must be ${length->X.Int.unsafeToString} or more items long`
  }
  schema->addRefinement(
    ~metadataId=Array.Refinement.metadataId,
    ~refiner=(b, ~inputVar, ~selfSchema as _) => {
      `if(${inputVar}.length<${b->B.embed(length)}){${b->B.fail(~message)}}`
    },
    ~refinement={
      kind: Min({length: length}),
      message,
    },
  )
}

let arrayMaxLength = (schema, length, ~message as maybeMessage=?) => {
  let message = switch maybeMessage {
  | Some(m) => m
  | None => `Array must be ${length->X.Int.unsafeToString} or fewer items long`
  }
  schema->addRefinement(
    ~metadataId=Array.Refinement.metadataId,
    ~refiner=(b, ~inputVar, ~selfSchema as _) => {
      `if(${inputVar}.length>${b->B.embed(length)}){${b->B.fail(~message)}}`
    },
    ~refinement={
      kind: Max({length: length}),
      message,
    },
  )
}

let arrayLength = (schema, length, ~message as maybeMessage=?) => {
  let message = switch maybeMessage {
  | Some(m) => m
  | None => `Array must be exactly ${length->X.Int.unsafeToString} items long`
  }
  schema->addRefinement(
    ~metadataId=Array.Refinement.metadataId,
    ~refiner=(b, ~inputVar, ~selfSchema as _) => {
      `if(${inputVar}.length!==${b->B.embed(length)}){${b->B.fail(~message)}}`
    },
    ~refinement={
      kind: Length({length: length}),
      message,
    },
  )
}

let stringMinLength = (schema, length, ~message as maybeMessage=?) => {
  let message = switch maybeMessage {
  | Some(m) => m
  | None => `String must be ${length->X.Int.unsafeToString} or more characters long`
  }
  schema->addRefinement(
    ~metadataId=String.Refinement.metadataId,
    ~refiner=(b, ~inputVar, ~selfSchema as _) => {
      `if(${inputVar}.length<${b->B.embed(length)}){${b->B.fail(~message)}}`
    },
    ~refinement={
      kind: Min({length: length}),
      message,
    },
  )
}

let stringMaxLength = (schema, length, ~message as maybeMessage=?) => {
  let message = switch maybeMessage {
  | Some(m) => m
  | None => `String must be ${length->X.Int.unsafeToString} or fewer characters long`
  }
  schema->addRefinement(
    ~metadataId=String.Refinement.metadataId,
    ~refiner=(b, ~inputVar, ~selfSchema as _) => {
      `if(${inputVar}.length>${b->B.embed(length)}){${b->B.fail(~message)}}`
    },
    ~refinement={
      kind: Max({length: length}),
      message,
    },
  )
}

let stringLength = (schema, length, ~message as maybeMessage=?) => {
  let message = switch maybeMessage {
  | Some(m) => m
  | None => `String must be exactly ${length->X.Int.unsafeToString} characters long`
  }
  schema->addRefinement(
    ~metadataId=String.Refinement.metadataId,
    ~refiner=(b, ~inputVar, ~selfSchema as _) => {
      `if(${inputVar}.length!==${b->B.embed(length)}){${b->B.fail(~message)}}`
    },
    ~refinement={
      kind: Length({length: length}),
      message,
    },
  )
}

let email = (schema, ~message=`Invalid email address`) => {
  Js.log("foo")
  schema->addRefinement(
    ~metadataId=String.Refinement.metadataId,
    ~refiner=(b, ~inputVar, ~selfSchema as _) => {
      `if(!${b->B.embed(String.emailRegex)}.test(${inputVar})){${b->B.fail(~message)}}`
    },
    ~refinement={
      kind: Email,
      message,
    },
  )
}

let uuid = (schema, ~message=`Invalid UUID`) => {
  schema->addRefinement(
    ~metadataId=String.Refinement.metadataId,
    ~refiner=(b, ~inputVar, ~selfSchema as _) => {
      `if(!${b->B.embed(String.uuidRegex)}.test(${inputVar})){${b->B.fail(~message)}}`
    },
    ~refinement={
      kind: Uuid,
      message,
    },
  )
}

let cuid = (schema, ~message=`Invalid CUID`) => {
  schema->addRefinement(
    ~metadataId=String.Refinement.metadataId,
    ~refiner=(b, ~inputVar, ~selfSchema as _) => {
      `if(!${b->B.embed(String.cuidRegex)}.test(${inputVar})){${b->B.fail(~message)}}`
    },
    ~refinement={
      kind: Cuid,
      message,
    },
  )
}

let url = (schema, ~message=`Invalid url`) => {
  schema->addRefinement(
    ~metadataId=String.Refinement.metadataId,
    ~refiner=(b, ~inputVar, ~selfSchema as _) => {
      `try{new URL(${inputVar})}catch(_){${b->B.fail(~message)}}`
    },
    ~refinement={
      kind: Url,
      message,
    },
  )
}

let pattern = (schema, re, ~message=`Invalid`) => {
  schema->addRefinement(
    ~metadataId=String.Refinement.metadataId,
    ~refiner=(b, ~inputVar, ~selfSchema as _) => {
      if re->Js.Re.global {
        // TODO Write a regression test when it's needed
        `${b->B.embed(re)}.lastIndex=0;`
      } else {
        ""
      } ++
      `if(!${b->B.embed(re)}.test(${inputVar})){${b->B.fail(~message)}}`
    },
    ~refinement={
      kind: Pattern({re: re}),
      message,
    },
  )
}

let datetime = (schema, ~message=`Invalid datetime string! Expected UTC`) => {
  let refinement = {
    String.Refinement.kind: Datetime,
    message,
  }
  schema
  ->Metadata.set(
    ~id=String.Refinement.metadataId,
    {
      switch schema->Metadata.get(~id=String.Refinement.metadataId) {
      | Some(refinements) => refinements->X.Array.append(refinement)
      | None => [refinement]
      }
    },
  )
  ->transform(s => {
    parser: string => {
      if String.datetimeRe->Js.Re.test_(string)->not {
        s.fail(message)
      }
      Js.Date.fromString(string)
    },
    serializer: date => date->Js.Date.toISOString,
  })
}

let trim = schema => {
  let transformer = string => string->Js.String2.trim
  schema->transform(_ => {parser: transformer, serializer: transformer})
}

let nullable = schema => {
  Union.factory([schema->castToUnknown, unit->castToPublic, nullLiteral->castToPublic])
}

let nullableAsOption = schema => {
  Union.factory([schema->castToUnknown, unit->castToPublic, nullAsUnit->castToUnknown])
}

// =============
// JS/TS API
// =============

let parser = %raw(`(...args) => getDecoder(unknown, ...args)`)

let asyncParser = %raw(`(...args) => getDecoder(unknown, ...args, 1)`)

let asyncDecoder = %raw(`(...args) => getDecoder(...args, 1)`)

let encoder = %raw(`(...args) => getDecoder(...args.map(reverse))`)

let asyncEncoder = %raw(`(...args) => getDecoder(...args.map(reverse), 1)`)

let js_assert = (schema, data) => {
  getDecoder3(~s1=unknown, ~s2=schema->castToInternal, ~s3=assertResult)(data)
}

let js_union = values =>
  Union.factory(
    values->Js.Array2.map(Schema.definitionToSchema)->(Obj.magic: array<internal> => array<'a>),
  )

let js_to = {
  // FIXME: Test how it'll work if we have async var as input
  // FIXME: Might not work well with object targets
  let customBuilder = (~from, ~target, ~fn) => {
    Builder.make((~input, ~selfSchema as _) => {
      let output = input->B.allocateVal(~schema=target)
      output.code = `try{${output.inline}=${input->B.embed(
          fn,
        )}(${input.inline})}catch(x){${output->B.failWithArg(e => FailedTransformation({
          from: from->castToPublic,
          to: target->castToPublic,
          error: e,
        }), `x`)}}`
      output
    })
  }

  (
    schema,
    target,
    ~decoder as maybeDecoder: option<'value => 'target>=?,
    ~encoder as maybeEncoder: option<'target => 'value>=?,
  ) => {
    updateOutput(schema->castToInternal, mut => {
      let target = target->castToInternal
      let target = switch maybeEncoder {
      | Some(fn) =>
        let targetMut = target->copySchema
        targetMut.serializer = Some(
          customBuilder(~from=target, ~target=schema->castToInternal, ~fn),
        )
        targetMut
      | None => target
      }
      mut.to = Some(target)
      switch maybeDecoder {
      | Some(fn) => mut.parser = Some(customBuilder(~from=schema->castToInternal, ~target, ~fn))
      | None => ()
      }
    })
  }
}

let js_refine = (schema, refiner) => {
  schema->refine(s => {
    v => refiner(v, s)
  })
}

let noop = a => a
let js_asyncParserRefine = (schema, refine) => {
  schema->transform(s => {
    {
      asyncParser: v => refine(v, s)->X.Promise.thenResolve(() => v),
      serializer: noop,
    }
  })
}

let js_optional = (schema, maybeOr) => {
  // TODO: maybeOr should be part of the unit schema
  let schema = Union.factory([schema->castToUnknown, unit->castToPublic])
  switch maybeOr {
  | Some(or) if Js.typeof(or) === "function" => schema->Option.getOrWith(or->Obj.magic)->Obj.magic
  | Some(or) => schema->Option.getOr(or->Obj.magic)->Obj.magic
  | None => schema
  }
}

let js_nullable = (schema, maybeOr) => {
  // TODO: maybeOr should be part of the unit schema
  let schema = Union.factory([schema->castToUnknown, nullAsUnit->castToUnknown])
  switch maybeOr {
  | Some(or) if Js.typeof(or) === "function" => schema->Option.getOrWith(or->Obj.magic)->Obj.magic
  | Some(or) => schema->Option.getOr(or->Obj.magic)->Obj.magic
  | None => schema
  }
}

let js_merge = (s1, s2) => {
  switch switch (s1, s2) {
  | (
      Object({properties: properties1, additionalItems: additionalItems1}),
      Object({properties: properties2, additionalItems: additionalItems2}),
    )
    // Filter out S.record schemas
    if additionalItems1->Type.typeof === #string &&
    additionalItems2->Type.typeof === #string &&
    !((s1->castToInternal).to->Obj.magic) &&
    !((s2->castToInternal).to->Obj.magic) =>
    let properties = properties1->X.Dict.copy
    let keys2 = properties2->Js.Dict.keys

    for idx in 0 to keys2->Js.Array2.length - 1 {
      let key = keys2->Js.Array2.unsafe_get(idx)
      properties->Js.Dict.set(key, properties2->Js.Dict.unsafeGet(key))
    }

    let mut = base(objectTag)
    mut.properties = Some(properties->(Obj.magic: dict<t<unknown>> => dict<internal>))
    mut.additionalItems = Some(additionalItems1)
    mut.decoder = objectDecoder
    Some(mut->castToPublic)
  | _ => None
  } {
  | Some(s) => s
  | None =>
    InternalError.panic("The merge supports only structured object schemas without transformations")
  }
}

let global = override => {
  globalConfig.defaultAdditionalItems = (switch override.defaultAdditionalItems {
  | Some(defaultAdditionalItems) => defaultAdditionalItems
  | None => initialOnAdditionalItems
  } :> additionalItems)
  globalConfig.disableNanNumberValidation = switch override.disableNanNumberValidation {
  | Some(disableNanNumberValidation) => disableNanNumberValidation
  | None => initialDisableNanNumberProtection
  }
}

let reverse = reverse->Obj.magic

module RescriptJSONSchema = {
  include JSONSchema

  let jsonSchemaMetadataId: Metadata.Id.t<t> = Metadata.Id.internal("JSONSchema")

  @val
  external merge: (@as(json`{}`) _, t, t) => t = "Object.assign"

  let rec internalToJSONSchema = (schema: schema<unknown>, ~path, ~defs, ~parent): JSONSchema.t => {
    let jsonSchema: Mutable.t = {}
    switch schema {
    | String({?const}) =>
      jsonSchema.type_ = Some(Arrayable.single(#string))
      schema
      ->String.refinements
      ->Js.Array2.forEach(refinement => {
        switch refinement {
        | {kind: Email} => jsonSchema.format = Some("email")
        | {kind: Url} => jsonSchema.format = Some("uri")
        | {kind: Uuid} => jsonSchema.format = Some("uuid")
        | {kind: Datetime} => jsonSchema.format = Some("date-time")
        | {kind: Cuid} => ()
        | {kind: Length({length})} => {
            jsonSchema.minLength = Some(length)
            jsonSchema.maxLength = Some(length)
          }
        | {kind: Max({length})} => jsonSchema.maxLength = Some(length)
        | {kind: Min({length})} => jsonSchema.minLength = Some(length)
        | {kind: Pattern({re})} => jsonSchema.pattern = Some(re->Js.String2.make)
        }
      })
      switch const {
      | Some(value) => jsonSchema.const = Some(Js.Json.string(value))
      | None => ()
      }
    | Number({?format, ?const}) =>
      switch format {
      | Some(Int32) =>
        jsonSchema.type_ = Some(Arrayable.single(#integer))
        schema
        ->Int.refinements
        ->Js.Array2.forEach(refinement => {
          switch refinement {
          | {kind: Max({value})} => jsonSchema.maximum = Some(value->Js.Int.toFloat)
          | {kind: Min({value})} => jsonSchema.minimum = Some(value->Js.Int.toFloat)
          }
        })
      | Some(Port) => {
          jsonSchema.type_ = Some(Arrayable.single(#integer))
          jsonSchema.maximum = Some(65535.)
          jsonSchema.minimum = Some(0.)
        }
      | None =>
        jsonSchema.type_ = Some(Arrayable.single(#number))
        schema
        ->Float.refinements
        ->Js.Array2.forEach(refinement => {
          switch refinement {
          | {kind: Max({value})} => jsonSchema.maximum = Some(value)
          | {kind: Min({value})} => jsonSchema.minimum = Some(value)
          }
        })
      }
      switch const {
      | Some(value) => jsonSchema.const = Some(Js.Json.number(value))
      | None => ()
      }
    | Boolean({?const}) => {
        jsonSchema.type_ = Some(Arrayable.single(#boolean))
        switch const {
        | Some(value) => jsonSchema.const = Some(Js.Json.boolean(value))
        | None => ()
        }
      }
    | Array({additionalItems, items}) =>
      switch additionalItems {
      | Schema(childSchema) =>
        jsonSchema.items = Some(
          Arrayable.single(
            Schema(
              internalToJSONSchema(
                childSchema,
                ~parent=schema,
                ~path=path->Path.concat(Path.dynamic),
                ~defs,
              ),
            ),
          ),
        )
        jsonSchema.type_ = Some(Arrayable.single(#array))
        schema
        ->Array.refinements
        ->Js.Array2.forEach(refinement => {
          switch refinement {
          | {kind: Max({length})} => jsonSchema.maxItems = Some(length)
          | {kind: Min({length})} => jsonSchema.minItems = Some(length)
          | {kind: Length({length})} => {
              jsonSchema.maxItems = Some(length)
              jsonSchema.minItems = Some(length)
            }
          }
        })
      | _ => {
          let items = items->Js.Array2.mapi((itemSchema, idx) => {
            Schema(
              internalToJSONSchema(
                itemSchema,
                ~parent=schema,
                ~path=path->Path.concat(Path.fromLocation(idx->Js.Int.toString)),
                ~defs,
              ),
            )
          })
          let itemsNumber = items->Js.Array2.length

          jsonSchema.items = Some(Arrayable.array(items))
          jsonSchema.type_ = Some(Arrayable.single(#array))
          jsonSchema.minItems = Some(itemsNumber)
          jsonSchema.maxItems = Some(itemsNumber)
        }
      }

    | Union({anyOf}) => {
        let literals = []
        let items = []

        anyOf->Js.Array2.forEach(childSchema => {
          switch childSchema {
          // Filter out undefined to support optional fields
          | Undefined(_) if (parent->castToInternal).tag === objectTag => ()
          | _ => {
              items
              ->Js.Array2.push(
                Schema(internalToJSONSchema(childSchema, ~parent=schema, ~path, ~defs)),
              )
              ->ignore
              switch childSchema->castToInternal->isLiteral {
              | true =>
                literals
                ->Js.Array2.push(
                  (childSchema->castToInternal).const->(Obj.magic: option<char> => Js.Json.t),
                )
                ->ignore
              | false => ()
              }
            }
          }
        })

        let itemsNumber = items->Js.Array2.length

        switch (schema->untag).default {
        | Some(default) => jsonSchema.default = Some(default->(Obj.magic: unknown => Js.Json.t))
        | None => ()
        }

        // TODO: Write a breaking test with itemsNumber === 0
        if itemsNumber === 1 {
          jsonSchema->Mutable.mixin(items->Js.Array2.unsafe_get(0)->Obj.magic)
        } else if literals->Js.Array2.length === itemsNumber {
          jsonSchema.enum = Some(literals)
        } else {
          jsonSchema.anyOf = Some(items)
        }
      }
    | Object({properties, additionalItems}) =>
      switch additionalItems {
      | Schema(childSchema) => {
          jsonSchema.type_ = Some(Arrayable.single(#object))
          jsonSchema.additionalProperties = Some(
            Schema(
              internalToJSONSchema(
                childSchema,
                ~path=path->Path.concat(Path.dynamic),
                ~defs,
                ~parent=schema,
              ),
            ),
          )
        }
      | _ => {
          let required = []
          let keys = properties->Js.Dict.keys
          let jsonProperties = Js.Dict.empty()

          for idx in 0 to keys->Js.Array2.length - 1 {
            let key = keys->Js.Array2.unsafe_get(idx)
            let itemSchema = properties->Js.Dict.unsafeGet(key)
            let fieldSchema = internalToJSONSchema(
              itemSchema,
              ~path=path->Path.concat(Path.fromLocation(key)),
              ~defs,
              ~parent=schema,
            )
            if itemSchema->castToInternal->isOptional->not {
              required->Js.Array2.push(key)->ignore
            }
            jsonProperties->Js.Dict.set(key, Schema(fieldSchema))
          }

          jsonSchema.type_ = Some(Arrayable.single(#object))
          jsonSchema.properties = Some(jsonProperties)
          jsonSchema.additionalProperties = Some(
            switch additionalItems {
            | Strict => JSONSchema.Never
            | Strip
            | Schema(_) =>
              JSONSchema.Any
            },
          )
          switch required {
          | [] => ()
          | required => jsonSchema.required = Some(required)
          }
        }
      }
    | Ref({ref}) if ref === `${defsPath}${jsonName}` => ()
    | Ref({ref}) => jsonSchema.ref = Some(ref)
    | Null(_) => jsonSchema.type_ = Some(Arrayable.single(#null))
    | Never(_) => jsonSchema.not = Some(Schema({}))

    | _ =>
      X.Exn.throwAny(
        InternalError.make(
          ~code=InvalidJsonSchema(
            if (parent->castToInternal).tag->TagFlag.get->Flag.unsafeHas(TagFlag.union) {
              parent
            } else {
              schema
            },
          ),
          ~flag=Flag.none,
          ~path,
        ),
      )
    }

    switch schema->untag {
    | {description: m} => jsonSchema.description = Some(m)
    | _ => ()
    }

    switch schema->untag {
    | {title: m} => jsonSchema.title = Some(m)
    | _ => ()
    }

    switch schema->untag {
    | {deprecated} => jsonSchema.deprecated = Some(deprecated)
    | _ => ()
    }

    switch schema->untag {
    | {examples} =>
      jsonSchema.examples = Some(
        examples->(
          Obj.magic: // If a schema is Jsonable,
          // then examples are Jsonable too.
          array<unknown> => array<Js.Json.t>
        ),
      )
    | _ => ()
    }

    switch schema->untag {
    | {defs: schemaDefs} =>
      let _ = defs->X.Dict.mixin(schemaDefs)
    | _ => ()
    }

    switch schema->Metadata.get(~id=jsonSchemaMetadataId) {
    | Some(metadataRawSchema) => jsonSchema->Mutable.mixin(metadataRawSchema)
    | None => ()
    }

    jsonSchema->Mutable.toReadOnly
  }
}

let toJSONSchema = schema => {
  let target = schema->castToInternal
  let defs = Js.Dict.empty()
  let jsonSchema =
    target
    ->castToPublic
    ->RescriptJSONSchema.internalToJSONSchema(~path=Path.empty, ~parent=target->castToPublic, ~defs)
  let _ = %raw(`delete defs.JSON`)
  let defsKeys = defs->Js.Dict.keys
  if defsKeys->Js.Array2.length->X.Int.unsafeToBool {
    // Reuse the same object to prevent allocations
    // Nothing critical, just because we can
    let jsonSchemDefs = defs->(Obj.magic: dict<t<unknown>> => dict<JSONSchema.definition>)
    defsKeys->Js.Array2.forEach(key => {
      let schema = defs->Js.Dict.unsafeGet(key)
      jsonSchemDefs->Js.Dict.set(
        key,
        schema
        ->RescriptJSONSchema.internalToJSONSchema(
          ~parent=schema,
          ~path=Path.empty,
          // It's not possible to have nested recursive schema.
          // It should be grouped to a single $defs of the most top-level schema.
          ~defs=%raw(`0`),
        )
        ->Schema,
      )
    })
    (jsonSchema->JSONSchema.Mutable.fromReadOnly).defs = Some(jsonSchemDefs)
  }
  jsonSchema
}

let extendJSONSchema = (schema, jsonSchema) => {
  schema->Metadata.set(
    ~id=RescriptJSONSchema.jsonSchemaMetadataId,
    switch schema->Metadata.get(~id=RescriptJSONSchema.jsonSchemaMetadataId) {
    | Some(existingSchemaExtend) => RescriptJSONSchema.merge(existingSchemaExtend, jsonSchema)
    | None => jsonSchema
    },
  )
}

let castAnySchemaToJsonableS = (Obj.magic: schema<'any> => schema<Js.Json.t>)
let rec fromJSONSchema: RescriptJSONSchema.t => t<Js.Json.t> = {
  @inline
  let primitiveToSchema = primitive => {
    Literal.parse(primitive)->castToPublic->castAnySchemaToJsonableS
  }

  let toIntSchema = (jsonSchema: JSONSchema.t) => {
    let schema = int->castToPublic
    // TODO: Support jsonSchema.multipleOf when it's in rescript-schema
    // if (typeof jsonSchema.multipleOf === "number" && jsonSchema.multipleOf !== 1) {
    //  r += `.multipleOf(${jsonSchema.multipleOf})`;
    // }
    let schema = switch jsonSchema {
    | {minimum} => schema->intMin(minimum->Belt.Float.toInt)
    | {exclusiveMinimum} => schema->intMin((exclusiveMinimum +. 1.)->Belt.Float.toInt)
    | _ => schema
    }
    let schema = switch jsonSchema {
    | {maximum} => schema->intMax(maximum->Belt.Float.toInt)
    | {exclusiveMinimum} => schema->intMax((exclusiveMinimum -. 1.)->Belt.Float.toInt)
    | _ => schema
    }
    schema->castAnySchemaToJsonableS
  }

  let definitionToDefaultValue = (definition: JSONSchema.definition) =>
    switch definition {
    | Schema(s) => s.default
    | _ => None
    }

  (jsonSchema: JSONSchema.t) => {
    let anySchema = json->castToPublic

    let definitionToSchema = (definition: JSONSchema.definition) =>
      switch definition {
      | Schema(s) => s->fromJSONSchema
      | Any => anySchema
      | Never => never->castToAny
      }

    let schema = switch jsonSchema {
    | _ if (jsonSchema->(Obj.magic: JSONSchema.t => {..}))["nullable"] =>
      null(
        jsonSchema
        ->RescriptJSONSchema.merge({"nullable": false}->(Obj.magic: {..} => JSONSchema.t))
        ->fromJSONSchema,
      )->castAnySchemaToJsonableS
    | {type_} if type_ === JSONSchema.Arrayable.single(#object) =>
      let schema = switch jsonSchema.properties {
      | Some(properties) =>
        let schema =
          {
            let obj = Js.Dict.empty()
            properties
            ->Js.Dict.keys
            ->Js.Array2.forEach(key => {
              let property = properties->Js.Dict.unsafeGet(key)
              let propertySchema = property->definitionToSchema
              let propertySchema = switch jsonSchema.required {
              | Some(r) if r->Js.Array2.includes(key) => propertySchema
              | _ =>
                switch property->definitionToDefaultValue {
                | Some(defaultValue) =>
                  propertySchema->option->Option.getOr(defaultValue)->castAnySchemaToJsonableS
                | None => propertySchema->option->castAnySchemaToJsonableS
                }
              }
              Js.Dict.set(obj, key, propertySchema)
            })
            obj->(Obj.magic: dict<schema<JSON.t>> => unknown)
          }
          ->Schema.definitionToSchema
          ->castToPublic
        let schema = switch jsonSchema {
        | {additionalProperties} if additionalProperties === Never => schema->strict
        | _ => schema
        }
        schema->castAnySchemaToJsonableS
      | None =>
        switch jsonSchema.additionalProperties {
        | Some(additionalProperties) =>
          switch additionalProperties {
          | Any => dict(anySchema)->castAnySchemaToJsonableS
          | Never => object(_ => ())->strict->castAnySchemaToJsonableS
          | Schema(s) => dict(s->fromJSONSchema)->castAnySchemaToJsonableS
          }
        | None => Schema.factory(_ => ())->castAnySchemaToJsonableS
        }
      }

      // TODO: jsonSchema.anyOf and jsonSchema.oneOf support
      schema
    | {type_} if type_ === JSONSchema.Arrayable.single(#array) => {
        let schema = switch jsonSchema.items {
        | Some(items) =>
          switch items->JSONSchema.Arrayable.classify {
          | Single(single) => array(single->definitionToSchema)
          | Array(array) =>
            tuple(s => array->Js.Array2.mapi((d, idx) => s.item(idx, d->definitionToSchema)))
          }
        | None => array(anySchema)
        }
        let schema = switch jsonSchema.minItems {
        | Some(min) => schema->arrayMinLength(min)
        | _ => schema
        }
        let schema = switch jsonSchema.maxItems {
        | Some(max) => schema->arrayMaxLength(max)
        | _ => schema
        }
        schema->castAnySchemaToJsonableS
      }
    | {anyOf: []} => anySchema
    | {anyOf: [d]} => d->definitionToSchema
    | {anyOf: definitions} => union(definitions->Js.Array2.map(definitionToSchema))
    | {allOf: []} => anySchema
    | {allOf: [d]} => d->definitionToSchema
    | {allOf: definitions} =>
      anySchema->refine(s =>
        data => {
          definitions->Js.Array2.forEach(d => {
            try data->assertOrThrow(d->definitionToSchema) catch {
            | _ => s.fail("Should pass for all schemas of the allOf property.")
            }
          })
        }
      )
    | {oneOf: []} => anySchema
    | {oneOf: [d]} => d->definitionToSchema
    | {oneOf: definitions} =>
      anySchema->refine(s =>
        data => {
          let hasOneValidRef = ref(false)
          definitions->Js.Array2.forEach(d => {
            let passed = try {
              let _ = data->assertOrThrow(d->definitionToSchema)
              true
            } catch {
            | _ => false
            }
            if passed {
              if hasOneValidRef.contents {
                s.fail("Should pass single schema according to the oneOf property.")
              }
              hasOneValidRef.contents = true
            }
          })
          if hasOneValidRef.contents->not {
            s.fail("Should pass at least one schema according to the oneOf property.")
          }
        }
      )
    | {not} =>
      anySchema->refine(s =>
        data => {
          let passed = try {
            let _ = data->assertOrThrow(not->definitionToSchema)
            true
          } catch {
          | _ => false
          }
          if passed {
            s.fail("Should NOT be valid against schema in the not property.")
          }
        }
      )
    // needs to come before primitives
    | {enum: []} => anySchema
    | {enum: [p]} => p->primitiveToSchema
    | {enum: primitives} =>
      union(primitives->Js.Array2.map(primitiveToSchema))->castAnySchemaToJsonableS
    | {const} => const->primitiveToSchema
    | {type_} if type_->JSONSchema.Arrayable.isArray =>
      let types = type_->(Obj.magic: JSONSchema.Arrayable.t<'a> => array<'a>)
      union(
        types->Js.Array2.map(type_ => {
          jsonSchema
          ->RescriptJSONSchema.merge({type_: JSONSchema.Arrayable.single(type_)})
          ->fromJSONSchema
        }),
      )
    | {type_} if type_ === JSONSchema.Arrayable.single(#string) =>
      let schema = string->castToPublic
      let schema = switch jsonSchema {
      | {pattern: p} => schema->pattern(Js.Re.fromString(p))
      | _ => schema
      }

      let schema = switch jsonSchema {
      | {minLength} => schema->stringMinLength(minLength)
      | _ => schema
      }
      let schema = switch jsonSchema {
      | {maxLength} => schema->stringMaxLength(maxLength)
      | _ => schema
      }
      switch jsonSchema {
      | {format: "email"} => schema->email->castAnySchemaToJsonableS
      | {format: "uri"} => schema->url->castAnySchemaToJsonableS
      | {format: "uuid"} => schema->uuid->castAnySchemaToJsonableS
      | {format: "date-time"} => schema->datetime->castAnySchemaToJsonableS
      | _ => schema->castAnySchemaToJsonableS
      }

    | {type_} if type_ === JSONSchema.Arrayable.single(#integer) => jsonSchema->toIntSchema
    | {type_, format: "int64"} if type_ === JSONSchema.Arrayable.single(#number) =>
      jsonSchema->toIntSchema
    | {type_, multipleOf: 1.} if type_ === JSONSchema.Arrayable.single(#number) =>
      jsonSchema->toIntSchema
    | {type_} if type_ === JSONSchema.Arrayable.single(#number) => {
        let schema = float->castToPublic
        let schema = switch jsonSchema {
        | {minimum} => schema->floatMin(minimum)
        | {exclusiveMinimum} => schema->floatMin(exclusiveMinimum +. 1.)
        | _ => schema
        }
        let schema = switch jsonSchema {
        | {maximum} => schema->floatMax(maximum)
        | {exclusiveMinimum} => schema->floatMax(exclusiveMinimum -. 1.)
        | _ => schema
        }
        schema->castAnySchemaToJsonableS
      }
    | {type_} if type_ === JSONSchema.Arrayable.single(#boolean) =>
      bool->castToPublic->castAnySchemaToJsonableS
    | {type_} if type_ === JSONSchema.Arrayable.single(#null) =>
      literal(%raw(`null`))->castAnySchemaToJsonableS
    | {if_, then, else_} => {
        let ifSchema = if_->definitionToSchema
        let thenSchema = then->definitionToSchema
        let elseSchema = else_->definitionToSchema
        anySchema->refine(_ =>
          data => {
            let passed = try {
              let _ = data->assertOrThrow(ifSchema)
              true
            } catch {
            | _ => false
            }
            if passed {
              data->assertOrThrow(thenSchema)
            } else {
              data->assertOrThrow(elseSchema)
            }
          }
        )
      }
    | _ => anySchema
    }

    let schema = switch jsonSchema {
    | {description: _} | {deprecated: _} | {examples: _} | {title: _} =>
      schema->meta({
        title: ?jsonSchema.title,
        description: ?jsonSchema.description,
        deprecated: ?jsonSchema.deprecated,
        examples: ?jsonSchema.examples,
      })
    | _ => schema
    }

    schema
  }
}

let min = (schema, minValue, ~message as maybeMessage=?) => {
  switch schema {
  | String(_) => schema->stringMinLength(minValue, ~message=?maybeMessage)
  | Array(_) => schema->arrayMinLength(minValue, ~message=?maybeMessage)
  | Number({format: Int32 | Port}) => schema->intMin(minValue, ~message=?maybeMessage)
  | Number(_) => schema->floatMin(minValue->Obj.magic, ~message=?maybeMessage)
  | _ =>
    InternalError.panic(
      `S.min is not supported for ${schema->toExpression} schema. Coerce the schema to string, number or array using S.to first.`,
    )
  }
}

let max = (schema, maxValue, ~message as maybeMessage=?) => {
  switch schema {
  | String(_) => schema->stringMaxLength(maxValue, ~message=?maybeMessage)
  | Array(_) => schema->arrayMaxLength(maxValue, ~message=?maybeMessage)
  | Number({format: Int32 | Port}) => schema->intMax(maxValue, ~message=?maybeMessage)
  | Number(_) => schema->floatMax(maxValue->Obj.magic, ~message=?maybeMessage)
  | _ =>
    InternalError.panic(
      `S.max is not supported for ${schema->toExpression} schema. Coerce the schema to string, number or array using S.to first.`,
    )
  }
}

let length = (schema, length, ~message as maybeMessage=?) => {
  switch schema {
  | String(_) => schema->stringLength(length, ~message=?maybeMessage)
  | Array(_) => schema->arrayLength(length, ~message=?maybeMessage)
  | _ =>
    InternalError.panic(
      `S.length is not supported for ${schema->toExpression} schema. Coerce the schema to string or array using S.to first.`,
    )
  }
}

let unknown: t<unknown> = unknown->castToPublic
let json: t<Js.Json.t> = json->castToPublic
let jsonString: t<string> = jsonString->castToPublic
let uint8Array: t<Uint8Array.t> = uint8Array->castToPublic
let bool: t<bool> = bool->castToPublic
let symbol: t<Js.Types.symbol> = symbol->castToPublic
let string: t<string> = string->castToPublic
let int: t<int> = int->castToPublic
let float: t<float> = float->castToPublic
let bigint: t<bigint> = bigint->castToPublic
let unit: t<unit> = unit->castToPublic
