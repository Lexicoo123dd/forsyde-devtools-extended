# Type System: How It Works and How to Add Types

This document describes the type flow in the current compiler implementation and the steps needed when adding a new primitive type. It also explains how to write types and literals directly in compiler-generated ProceduralIR code.

The central distinction is:

- `Compiler.Types` defines the shared primitive data types and literal values.
- `Compiler.ForSyDeIR` carries those types through the signal/function representation.
- `Compiler.ProceduralIR` represents C-oriented types and expressions.
- The translation modules connect GHC Core, ForSyDeIR, ProceduralIR, and generated C.

## 1. The type system at a glance

### `Compiler.Types` — primitive types and literals

File: `src/Compiler/Types.hs`

This is the central definition of primitive types and literal values:

```haskell
data DataType
  = TInt
  | TFloat
  | TBool
  | TUnknown
  deriving (Show, Eq, Ord, Generic)

data DataLiteral
  = LInt Int
  | LFloat Float
  | LBool Bool
  deriving (Show, Eq, Generic)
```

`DataType` describes the type of a signal/token value. `DataLiteral` describes an actual literal value.

The module also defines the operations that depend on a primitive type:

| Helper | Purpose | Current examples |
|---|---|---|
| `typeOfLiteral` | Finds the `DataType` represented by a `DataLiteral` | `LInt 3` → `TInt` |
| `cTypeName` | Maps a primitive type to its C type spelling | `TInt` → `int` |
| `cUnionField` | Gives the corresponding field name in the generated token union | `TInt` → `Int` |
| `cFormatSpecifier` | Gives the `printf`/`scanf` format specifier | `TInt` → `%d` |
| `cUnionMember` | Builds a C union member declaration | `int Int;` |
| `translateLiteral` | Converts a literal value to C source text | `LBool True` → `1` |
| `prettyDataType` | Produces a readable type name for IR/debug output | `TInt` → `IntType` |
| `literalFormatSpecifier` | Gets a format specifier from a literal | `LInt 3` → `%d` |

`TUnknown` is a sentinel for a type that has not been determined. It is deliberately not a concrete C type: the C mapping helpers report an error for it.

### `ForSyDeIR` — types on signals and functions

File: `src/Compiler/ForSyDeIR.hs`

`ForSyDeIR` imports and re-exports `DataType` and `DataLiteral` from `Compiler.Types`.

The main places where the types are carried are:

```haskell
data IRSignal =
  IRSignal IRId DataType (IRId, Int) (IRId, Int)

data IRFunction =
  IRFunction IRId (Maybe CoreExpr) ([DataType], [DataType])
```

An `IRSignal` has a `DataType`. An `IRFunction` stores lists of input and output `DataType`s.

ForSyDeIR also converts supported GHC Core literals to the shared literal representation in `coreLiteralToIR`. In the current implementation, it handles integer and float Core literals:

```haskell
LitNumber LitNumInt n -> Just (LInt (fromIntegral n))
LitFloat f            -> Just (LFloat (fromRational f))
```

### `ProceduralIR` — C-oriented types and expressions

File: `src/Compiler/ProceduralIR.hs`

ProceduralIR has a broader C-oriented `Type` than `DataType`:

```haskell
data Type
  = TVoid
  | TData DataType
  | TToken
  | TIdent String
  | TPointer Type
  | TReference Type
  | TFunctionPointer Type [Type]
  | TQualifiedType [TypeQualifier] Type
```

Use `TData` to embed one of the shared primitive types in a ProceduralIR type. The other constructors describe C-level concepts such as `void`, the token type, named types, pointers, references, function pointers, and qualifiers.

Expressions use the shared literal representation:

```haskell
data Expression
  = EVar String
  | ELit DataLiteral
  | ...
```

There are no separate `EInt`, `EFloat`, or `EBool` expression constructors in the current design. Use `ELit` with the appropriate `DataLiteral` constructor.

### `ProceduralIRToC` — C source generation

File: `src/Compiler/ProceduralIRToC.hs`

The C backend uses the central helpers:

- `TData d` is rendered using `cTypeName d`.
- `ELit lit` is rendered using `translateLiteral lit`.
- The token union is assembled from the concrete input/output types recorded on function declarations/definitions, using `cUnionMember`. `TUnknown` is excluded from that union.

This means the C spelling of a primitive type should be defined in `Compiler.Types`, not duplicated in the backend.

## 2. How a Haskell type reaches generated C

For a supported primitive signal type, the intended flow is:

1. **GHC Core type inspection**
   - `Compiler.CoreIRToForSyDeIR.Function` inspects GHC types.
   - `translateCoreTypeToIR` currently recognises `Int`, `Float`, and `Bool`.
   - Lists are unwrapped recursively to their element type for the signal/function type representation.
2. **ForSyDeIR**
   - The recognised type is stored as a `DataType` on an `IRSignal` or in an `IRFunction` input/output type list.
3. **ForSyDeIR to ProceduralIR**
   - `Compiler.ForSyDeIRToProceduralIR` converts a `DataType` to a ProceduralIR `Type` using `irTypeToCType = TData`.
   - For example, `TInt` becomes `TData TInt`.
   - Literal values are converted to expressions with `irLiteralToExpression = ELit`.
4. **Core function translation**
   - `Compiler.CoreIRToProceduralIR` uses `DataLiteral` for literals (`FLit DataLiteral`, then `ELit lit`).
   - It uses `cUnionField` to select the token union member when reading input tokens and writing output tokens.
5. **C generation**
   - `Compiler.ProceduralIRToC` renders the ProceduralIR types and expressions using the helpers from `Compiler.Types`.

Not every C type has to originate from a Haskell signal type. The compiler itself also creates C variables, loop counters, return values, and other implementation details directly in ProceduralIR. Those are hardcoded at the ProceduralIR construction sites, as described below.

## 3. Adding a new primitive type

For example, suppose you want to add `Double`.

A complete end-to-end primitive type needs a type representation, literal representation (if literal values are needed), C mappings, and GHC Core recognition (if Haskell `Double` values should flow through the compiler).

### Step 1: Add the type and literal to `Compiler.Types`

Update `DataType` with `Double` as `TDouble`:

```haskell
data DataType
  = TInt
  | TFloat
  | TBool
  | TDouble
  | TUnknown
```

If the compiler must represent `Double` literal values, add a matching `DataLiteral` constructor:

```haskell
data DataLiteral
  = LInt Int
  | LFloat Float
  | LBool Bool
  | LDouble Double
```

Then update every function that pattern-matches on these types.

For `DataLiteral`:

```haskell
typeOfLiteral (LDouble _) = TDouble
```

For C generation and formatting:

```haskell
cTypeName TDouble        = "double"
cUnionField TDouble      = "Double"
cFormatSpecifier TDouble = "%lf"
cUnionMember             -- uses cTypeName and cUnionField; no extra case needed
translateLiteral (LDouble value) = show value
literalFormatSpecifier   -- uses typeOfLiteral and cFormatSpecifier
prettyDataType TDouble   = "DoubleType"
```

Also add the JSON representation:

```haskell
toJSON (LDouble value) =
  object ["type" .= ("Double" :: Text.Text), "value" .= toJSON value]
```

`ToJSON DataType` already serialises the constructor using `show`, so it does not need a new case-based mapping.

Do not add a literal constructor if the compiler never needs to represent literal values of the new type. However, `typeOfLiteral`, `translateLiteral`, JSON serialization, and any other exhaustive `DataLiteral` matches must remain complete for every constructor that does exist.

### Step 2: Detect the Haskell type from GHC Core

File: `src/Compiler/CoreIRToForSyDeIR/Function.hs`

Add the Core type constructor match to `translateCoreTypeToIR`:

```haskell
Just (tyCon, [])
  | tyCon == intTyCon    -> Just TInt
  | tyCon == floatTyCon  -> Just TFloat
  | tyCon == boolTyCon   -> Just TBool
  | tyCon == doubleTyCon -> Just TDouble
```

This step is needed when the new type should be inferred from Haskell function inputs/outputs and carried through ForSyDeIR. If the type is only used for compiler-generated C implementation variables, this GHC type-recognition step is not needed.

### Step 3: Convert Core literals, if required

File: `src/Compiler/ForSyDeIR.hs`

If GHC Core literals of the new type must become ProceduralIR literals, add the corresponding case to `coreLiteralToIR`.

### Step 4: Check downstream uses

For a primitive `DataType`, the existing generic path should carry the new type through:

- `IRSignal` and `IRFunction`, because they store `DataType`.
- `TData`, because it wraps any `DataType`.
- `irTypeToCType`, because it is currently `TData`.
- `ELit`, because it wraps any `DataLiteral`.
- The C type renderer, because it calls `cTypeName`.
- Token union generation, because it calls `cUnionMember`.
- Token field selection, because it calls `cUnionField`.

Still compile and test the project: any other exhaustive pattern matches elsewhere in the repository may need a new case.

## 4. Adding hardcoded types in compiler-generated ProceduralIR

Some types are not extracted from Haskell Core. They are needed by the compiler's own generated C code.

For example, `ForSyDeIRToProceduralIR` creates variables such as loop counters and `main`'s return type. These are explicitly represented using the ProceduralIR `Type` constructors.

To declare an `int`:

```haskell
SVarDecl (TData TInt) "i"
```

To define an `int` with an initial value:

```haskell
SVarDef (TData TInt) "i" (ELit (LInt 0))
```

To define a `float`:

```haskell
SVarDef (TData TFloat) "gain" (ELit (LFloat 0.5))
```

To define a `bool`:

```haskell
SVarDef (TData TBool) "enabled" (ELit (LBool True))
```

To use an integer literal in another expression:

```haskell
EBinOp Less (EVar "i") (ELit (LInt bufferSize))
```

To give a function an `int` return type:

```haskell
GFuncDef Nothing (TData TInt) "main" [] ([], []) body
```

The syntax is deliberately explicit:

- `TData TInt` means the ProceduralIR/C type `int`.
- `ELit (LInt 0)` means the C literal `0`.
- `SVarDef` combines a type, variable name, and initialising expression.
