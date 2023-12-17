# Haskell Proof Checker

Checks proofs about Haskell code

## Installation

At the top-level do
`stack install`

This should result in an executable called `prfchk`

## Usage

The proof-checker expects all input files to be in the **same** directory. Input files can be Theory files (`.thr`) or Haskell source (`.hs`).

It also runs in two modes:

### Interactive Mode

Giving the command `prfchk` starts up the command-line interface. You can then issue commands to load theories, list laws and check theorems.

### Batch Mode

Giving command `prfchk <filepath>` loads up 
the theory file indicated by `filepath`.

It then automatically checks every theorem present in that theory, and then exits.

### File Handling

A theory file has an extension `.thr` by default.
So `Theory` and `Theory.thr` refer to the same file.

If a file path contains one or more directory names,
e.g. `dir1/dir2/.../dirN/Theory.thr`, the those directory names are recorded internally
(as `dir1/dir2/.../dirN/`). 

If `Theory.thr` imports theory file `Subtheory`,
then the actual file loaded is `dir1/dir2/.../dirN/Subtheory.thr`.

Something similar is done when importing Haskell files (which always have the `.hs` extension).

## Syntax Guide

A theorem file is a mix of proof constructs and Haskell expressions

### Haskell Expressions

Haskell expressions can be single or many lines. 

In multi-line expressions indentation is important if the offside rule applies.

### Proof Constructs

Proof constructs can also be single or many lines. 

For proof constructs, indentation is irrelevant, and is only used for readability purposes.

####  Notation

We use angle-brackets to surround syntactic objects whose form needs further details. These can range from names for things, through Haskell expressions,
to constructs used to define proofs: strategies, calculations, assumptions, etc. 

The language is mainly line-based, with the exception of Haskell expressions, which may be multi-line and parsed as a unit.

There are two special bits of angle-bracket notation that deal with the relationship between some proof constructs and Haskell expressions:

`<br?>` is used to indicate an optional-line break, with some restrictions.

`<proof-stuff> <br?> <expr>` describes a situation where `<proof-stuff>` is associated with Haskell expression `<expr>`. What it means is that either:

* the `<expr>` can occur in its entirety on the same line as `<proof-stuff>`,
* or the `<expr>` occurs on one omore fo the following lines, terminated by a blank line.


`<br!>` is a variant of `<br>` where the line-break is mandatory.


### Theory Top-Level

A theory starts with the keyword `THEORY` and a theory name.
It is followed by zero or more lines that import other theories or Haskell file.

A `<Name>` here should start with a capital letter and be followed by zero or more letters or numbers.

A theory with name `<Name>` should be in a file called `<Name>.thr`.

A Haskell file with name `<Name>` should be in a file called `<Name>.hs` containing a module called `<Name>`.

```
THEORY <Name>
IMPORT-THEORY <Name>
IMPORT-HASKELL <Name>
```

The rest of the theory file contains proof constructs. These include laws, schemes for induction and case-based reasoning, and theorems.

### Laws

```
LAW <name> <br?> <expr>
```

A `<name>` can be any string with no whitespace

An `<expr>` is a valid Haskell expression.


### Induction Schemes

An induction scheme provideds the key information needed to do induction,
namely underlying type, base, step cases, and the relevant injectivity law. Such schemes have a name that indicates the data-type over which the induction is being done.

A `<Type>` should start with a capital letter and follow the rules for a Haskell type name.

Currently, one simple induction with one base and one step case is supported, and so the scheme consists of the following four items, in order:

```
INDUCTION-SCHEME <Type>
BASE <expr>
STEP <var> --> <expr>
INJ <br?> <expr> == <expr>
```

### Case-Based Reasoning Schemes

A case-based reasoning scheme provides two or more cases, each defined as a Haskell expression of type `Bool`, numbered from 1 upwards. It also states the law that requires the cases to be exhaustive (covers all possibilities), and a law for each pair of cases that requires them to be mutually exclusive (at most one can be true). The ordering of terms within the exhaustive law is the same order as the cases are listed (1..), and the exclusive law case-pairs are ordered as follows: (1,2); (1,3) ; ... ; (1,N); (2,3), ... ,(2,N); ... ; (N-2,N-1) ; (N-2,N) ; (N-1,N).

The ordering is important in order to simplify checking.

```
CASE-SCHEME <name>
CASE 1 <br?> <expr1>
...
CASE N <br?> <exprN>
EXHAUSTIVE <br?> <expr1> || ... || <exprN>
EXCLUSIVE 1 2 <br?> not(<expr1> && <expr2>)
...
-- use order 1 3 ; 1 4 ; ... ; N-2 N-1 ; N-2 N 
...
EXCLUSIVE (N-1) N <br?> not(<exprN-1> && <exprN>)
```


### Theorems

A theorem starts with the `THEOREM` keyword, which has an associated name and the expression to be proven. It contains a strategy construct, and then terminated by a line starting with `QED` which refers to the theorem name.

```
THEOREM <name> <br?> <expr>
<strategy>
QED <name>
```

### Theory sub-Level

A `<strategy>` starts with the `STRATEGY` keyword and an indicator of the strategy type. It then contains a `strategy-body` that corresponds to the strategy type. It is terminated by the `END` keyword referring to the strategy type.

```
STRATEGY <strategy-type>
<strategy-body>
END <strategy-type>
```

Currently there are six `strategy-type`s:

```
ReduceAll
ReduceLHS
ReduceRHS
ReduceBoth
CaseSplit <case-name>
Induction <ind-var> :: <type>
```

### Strategy Bodies

Each `<strategy-type>` has a specific `<strategy-body>`:


#### Reduce(All | LHS | RHS)

Here `<strategy-body>` is just a single calculation:

```
<calculation>
```

#### ReduceBoth

Here `<strategy-body>` is two calculations, the first introduced by the `LHS` keyword, on its own line, the second by the `RHS` keyword, also on its own line.

```
LHS
<calculation>
RHS
<calculation>
```

#### CaseSplit

Here `<strategy-body>` is a sequence of cases, in order 1..N,
each of which starts with the `CASE` keyword, followed by the sequence number, and a statement of the particular case predicate. This is followed by a `<case-body>`, and ended by a line starting with `END CASE` and followed by the relevant case number.

```
CASE 1 <br!> <expr1> 
<case-body>
END CASE 1
...
CASE N <br!> <exprN>
<case-body>
END CASE N
```

A `<case-body>` starts with the `SHOW` keyword, followed by a Haskell expression that is the thing being proven here. Note that this is the same for all cases,  but is repeated here for clarity, and checking.
It is then followed by a strategy construct that contains the proof of this case

```
SHOW <br?> <expr>
<strategy>
```

#### Induction

Here, the `<strategy-body>` contains two sub-parts: one for the base-case, the other for the induction-step.

The base-case starts with keyword `BASE`, followed by the value the induction variable takes for this case, and theny by an expression which is the theorem being proven with all instances of the induction variable replaced by the base value. This is followed by a proof strategy, and then terminated with the line `END BASE`.

The step-case starts with keyword `STEP` followed by an expression that denotes what the induction variable is replaced by in the step. This is followed by a line starting with `ASSUME` that gives an expression that states the induction hypothesis. Next is a line starting with `SHOW` that gives the expression obtained by taking the hypothesis (`hyp` in `ASSUME hyp`) and replacing all occurences of the induction variable by the step expression (STEP `stepexpr`). This is followed by a proof strategy, and then terminated with the line `END STEP`.

```
BASE <val> <br!> <expr>
<strategy>
END BASE
STEP <expr>
ASSUME <br!> <expr>
SHOW <br!> <expr>
<strategy>
END STEP
```

### Calculations

A `calculation` is a sequence of expressions, each separated by a line starting with an equals-sign, that contains a `<justification>`.

```
<expr>
= <justification>
...
= <justification>
<expr>
```

### Justifications

A `<justification>` has one compulsory component (`<law>`), and two optional components (`<usage>`,`<focus>`). If the latter two are ommitted, then default values are supplied, as all three are needed to properly specify a justification.

```
 = <law> [<usage>] [<focus>]
```


#### law (mandatory):

The `<law>` component identifies the logical/mathematical justification for the change being mave between the expression before this justification and the one after it. It can be an explicit reference to an actual law (`LAW`), or to a Haskell definition of a value or function (`DEF`), which effectively define laws).
In addition, the meaning of conditionals (`if`,`case`) induce laws (`IF`, `GRDIF`) that describe their outcomes. Here it suffices to indicate which condition branch has evaluated to `True`. For case-based reasoning and the induction step-case, we need to be able to use the case expression (`CASEP`) and inductive hypothesis (`INDHYP`) as laws. We support a deep-recursive simplfier (`SIMP`) for simple arithmetic and boolean simplifications, and a normaliser (`NORM`) that takes nested additions or multiplications and flattens them out and sorts them (saves **lots** of use of associativity and symmetry laws!)

Currently we handle the Haskell if-expression, and a variant of the Haskell case-expression that corresponds to a single pattern with multiple guards.

```
  LAW <name>        -- name of law
  DEF <name>        -- defn of name
  DEF <name>.<i>    -- defn of name, identifying clause
  IF <i>            -- if-expression,  identifying true branch
  GRDIF <i>         -- guarded-if, identifying true branch
  CASEP <name> <i>  -- case assumption, 
                    -- identifying casesplit scheme, and case number
  INDHYP            -- inductive hypothesis
  SIMP              -- simplifier
  NORM <op>         -- flattens and sorts nested <op> in { +, * }
```

Here `<i>` denotes a number from 1 upwards

The ordering used by `NORM` is **not** generally based 
on the numeric value of an expression.
It is based roughly on the "simplicity" of the expression:

From "smallest" (1) to "largest" (9):

  1. Booleans: `False < True` 
  2. Ints: `... < -2 < -1 < 0 < 1 < 2 < ...` 
  3. Chars: unicode ordering
  4. Variable and function names ordered using string ordering
  5. Constructors ordered using string ordering
  6. Function applications:  `f1 x1 < f2 x2` if `f1 < f2` or `f1=f2` and `x1 < x2`
  7. If-expressions: `if c then t else e` - compare `c`s, then `t`s, then `e`s.
  8. Case-expressions.
  9. Let-expressions.

#### usage (optional):

The `<usage>` component defines **how** a law is matched. There are three possibilities:

* Match the whole law to replace with `True` (not currently possible for `DEF` laws)
* For laws of the form `lhs = rhs` only: 

  * Match the LHS of the law to replace with the RHS
  * Match the RHS of the law to replace with the LHS

  Note that DEF laws do have the `lhs = rhs` form (sometimes with a little under-the-hood translation).

Here, `l2r` and `r2l` are keywords.

```
  l2r -- left-to-right (for laws of form lhs = rhs)
  r2l -- right-to-left (for laws of form lhs = rhs)
```

If `<usage>` is ommited, then it is assumed to be as follows:

* for a `DEF` law, it is taken as `l2r` ;
* in all other cases it means matching the **whole** law (the only way to specify the whole law)

#### focus (optional):

The `<focus>` component identifies **where** in the expression the law is applied. It specifies an occurence of a `<name>`. If the name is that of a non-function value, then that name is the focus. If the name is that of a function, then it applies to the expression that includes all the arguments to which that particular instance of the function is applied.

Here `@` is a keysymbol.

```
  @ <name>      -- the first occurrence of that name
  @ <name> <i>  -- ith in-order occurrence of name
```

If `<focus>` is ommited, then it is assumed to be as follows:

* for a `DEF` law, it taken as the first occurence of the name being defined;
* in all other cases it means the top-level of the expression.




