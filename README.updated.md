# ap — "apropos++" for the quick SBCL REPL

SLIME's apropos tools are fantastic — *when you are inside Emacs*.  
But sometimes you are not: you are SSH'ed into a box, inside a Docker container, or just running a plain SBCL REPL while tinkering.

`ap` exists for exactly those REPL-only sessions: a tiny, quick-to-load "apropos++" that you can use **immediately while experimenting**, without starting Emacs. It optimizes the classic loop:

1) search for a concept,  
2) skim docstrings,  
3) jump to the right symbol.

---

## What `ap` gives you

- **Search by name *or* docstring** (or both)
- **Regex search** powered by **cl-ppcre** (scanner compiled once per query → fast)
- **Short, Clojure-like command**: `(ap:ap ...)`
- **Kind filtering**: functions, macros, variables, classes, constants, special operators
- **Package selectors** that do what you mean:
  - `"."` = current package
  - `NIL` or `""` = all packages
  - `"CL"` / `:cl` / `#<PACKAGE ...>` = that package
  - `("CL" "SB-EXT")` = multiple packages
- **Exported-only by default**, optionally include internal symbols
- **Readable output**: description first, then name (because docstrings are what your eyes want)
- **Optional thematic ordering** (no LLMs): cluster-ish ordering by token similarity in name+docstring

---

## Quick start

### Install (Quicklisp / ASDF)

Put this repo somewhere ASDF can see it (e.g. `~/common-lisp/ap/`) and then:

```lisp
(ql:quickload :ap)
;; or
(asdf:load-system :ap)
```

After loading:

```lisp
(ap:ap "." "hash")
```

---

## File layout

```
ap/
  ap.asd
  package.lisp
  ap.lisp
  README.md
```

---

## The command: `ap:ap`

`ap` is REPL-first and intentionally permissive: you can call it with 0–2 positional arguments *plus* keywords.

### Calling forms

```lisp
(ap)                       ; pkg=".", q=nil
(ap q)                     ; treat single arg as query (default)
(ap pkg q)                 ; explicit pkg selector + query
(ap :pkg pkg :q q ...)     ; keyword-only style

;; Keywords: :k :exp :case :lim :min :tgt :u :s   (also :pkg and :q)
```

Positional convenience rules:
- 0 args → `pkg = "."`, `q = NIL`
- 1 arg  → **query by default**; you can still force package selection via `:pkg`
- 2 args → `(pkg q)`

You can always override with `:pkg` and/or `:q`.

> Introspection tip: `(describe 'ap::%ap)` shows the full `&key` interface with defaults.

---

## Package selectors (do-what-I-mean)

### One-arg ambiguity note

If you write `(ap "CL")`, do you mean the package **CL** or the query string **"CL"**?

`ap` chooses the simplest rule for fast REPL usage:
- with **one positional argument**, it is treated as **query**
- if you want a single positional package selector, use `:pkg`:

```lisp
(ap :pkg "CL")           ; list exported API of CL (description-first)
(ap :pkg nil :q "hash")  ; all packages, query
```

(And of course, `(ap "CL" "hash")` is unambiguous.)


### Current package: `"."`

```lisp
(ap:ap "." "hash")
```

### All packages: `NIL` or `""`

```lisp
(ap:ap nil "hash")
(ap:ap ""  "hash")
```

### A specific package

Any of these work:

```lisp
(ap:ap "CL" "hash")
(ap:ap :cl  "hash")
(ap:ap (find-package "CL") "hash")
```

### Multiple packages

```lisp
(ap:ap '("CL" "SB-EXT" "SB-THREAD") "thread")
```

---

## Query rules

### Query = NIL / "" → no filtering

Great for "list the API" cases.

```lisp
(ap:ap "MY-PKG" nil)
(ap:ap "MY-PKG" "")
```

### Regex search is the default

`ap` treats any non-empty query (except `=...`) as a **CL-PPCRE regex** and compiles it once.

```lisp
(ap:ap "CL" "hash")          ; behaves like substring match
(ap:ap "CL" "hash|table")    ; regex OR
(ap:ap "CL" "make-.*table")  ; regex
```

> ⚠️ Note: regex semantics mean `.` matches "anything". That's intentional: `ap` is regex-first.

### Exact match: prefix with `=`

Exact match uses `string=` (with case control).

```lisp
(ap:ap "CL" "=MAKE-HASH-TABLE")
(ap:ap "CL" "=car" :case t)     ; case-insensitive exact
(ap:ap "CL" "=car" :case nil)   ; case-sensitive exact
```

---

## Search targets: name vs doc vs both (`:tgt`)

By default, `ap` matches against **either symbol name or docstring**:

```lisp
(ap:ap "CL" "hash")       ; matches name OR doc
```

### Match name only

```lisp
(ap:ap "CL" "hash" :tgt :name)
```

### Match docstrings only

```lisp
(ap:ap "CL" "hash" :tgt :doc)
```

---

## Kind filtering (`:k`)

Default kinds are:
- `:function`
- `:macro`
- `:variable`

This is deliberate: these are what you most often browse at the REPL.

### Functions only

```lisp
(ap:ap "CL" "hash" :k '(function))
```

### Functions + macros

```lisp
(ap:ap "CL" "hash" :k '(function macro))
```

### Everything

```lisp
(ap:ap "CL" "hash" :k :all)
```

### Include classes, constants, special operators

```lisp
(ap:ap "CL" "condition" :k '(class))
(ap:ap "CL" "pi"        :k '(constant))
(ap:ap "CL" "let"       :k '(special-operator))
```

> Internally, `ap` can tag a symbol with multiple kinds (e.g., a name might be both a function and a variable in some packages).

---

## Exported-only vs internal symbols (`:exp`)

By default, `ap` shows **exported symbols only**:

```lisp
(ap:ap "SB-EXT" "thread")
```

To include **internal** symbols as well:

```lisp
(ap:ap "SB-EXT" "thread" :exp nil)
```

This is particularly useful for implementation packages like `SB-INT`, `SB-KERNEL`, etc.

---

## Show undocumented results (`:u`)

By default, `ap` filters out symbols with no docstring (because they're usually noise).

To include undocumented items:

```lisp
(ap:ap "MY-PKG" nil :u t)            ; list everything, including undocumented
(ap:ap "SB-EXT" "thread" :u t :exp nil)
```

---

## Limit output (`:lim`)

```lisp
(ap:ap nil "hash" :lim 50)
```

---

## Thematic ordering (`:min`)

You wanted "thematically close things close" **without LLMs**.
`ap` does this using a lightweight similarity heuristic:
- tokenize `(name + docstring)`
- use Jaccard similarity
- do greedy nearest-neighbor ordering

Enable it by passing a number to `:min`:

```lisp
(ap:ap "CL" "hash" :min 0.08)
(ap:ap "SB-EXT" "thread" :min 0.10 :exp nil)
```

### How to tune `:min`

- `:min 0.05` → looser grouping (longer "chains")
- `:min 0.08` → good default for most APIs
- `:min 0.12` → only very close items stay adjacent

If similarity drops below `:min`, the algorithm "restarts" from a new dense seed.
That gives you a cluster-ish effect without explicit clustering code.

> This is intentionally simple, fast, and dependency-free (no embeddings, no LLMs).

---

## Where this shines (common REPL workflows)

### 1) "What functions in my package mention CSV?"

```lisp
(ap:ap "." "csv" :k '(function))
```

### 2) "Search all packages for anything about mutex/locks"

```lisp
(ap:ap nil "mutex|lock" :k '(function macro) :lim 80)
```

### 3) "Find me the symbol whose doc says 'timeout' in SB-EXT"

```lisp
(ap:ap "SB-EXT" "timeout" :tgt :doc)
```

### 4) "List my package API in a readable way"

```lisp
(ap:ap "." nil)
```

### 5) "Internal browsing: show me ALL thread-related internals"

```lisp
(ap:ap "SB-THREAD" "thread" :exp nil :u t :k :all)
```

### 6) "Exact match because I already know the name"

```lisp
(ap:ap "CL" "=MAKE-HASH-TABLE")
```

### 7) "Regex power move: show all symbols ending in -TABLE"

```lisp
(ap:ap "CL" ".*-TABLE$" :tgt :name)
```

### 8) "Thematic tour of a concept"

```lisp
(ap:ap "CL" "stream" :min 0.08)
```

---

## Output format

`ap` prints a table:

- **Description** (docstring, one line)
- **Name**
- **Kind**
- **Package**

Docstrings are truncated to keep the display skim-friendly.

---

## Tips

### Regex is your superpower

Because CL-PPCRE is fast and we compile the scanner once, regex queries are cheap:

- `foo|bar`
- `^make-`
- `table$`
- `\bword\b` (word boundary)
- `[A-Z].*` etc.

### Keep `:tgt :doc` in mind

If you want "concept search", docstrings are often better than names:

```lisp
(ap:ap nil "authentication|oauth|token" :tgt :doc :lim 100)
```

---

## API reference (keywords)

- `:k` — kinds filter  
  - list like `'(function macro)` or `'(class)` or `:all`
- `:exp` — exported-only boolean (default `t`)
- `:case` — case-insensitive boolean (default `t`)
- `:lim` — limit number or NIL
- `:min` — number enables thematic ordering; NIL disables
- `:tgt` — `:name` / `:doc` / `:both` (default `:both`)
- `:u` — include undocumented symbols
- `:s` — output stream (default `*standard-output*`)

---

## Dependencies

- **cl-ppcre** (via Quicklisp / ASDF)

---

## License

MIT (or choose your preferred license in `ap.asd` / repository).

---

## FAQ

### "Why not use SLIME apropos?"

SLIME's apropos is great — **inside Emacs**.

This is for:
- SSH sessions
- Docker containers
- quick SBCL REPLs
- "I just want answers now" workflows

### "Does thematic ordering replace real embeddings?"

No. It's a pragmatic middle ground:
- no external services
- no heavy dependencies
- surprisingly useful on Lisp APIs because names and docs share meaningful tokens

If you ever want better grouping without LLMs, the next step is TF-IDF + cosine similarity (still local).

### "Can I make it even shorter?"

Import into `CL-USER`:

```lisp
(use-package :ap)
(ap "." "hash")
```

---

## Contributing ideas

If you want to extend `ap`, common next steps are:
- cluster headings ("Groups") instead of a single chain
- hide the package column when browsing a single package
- add definition locations via `sb-introspect`
- add a return mode to return symbols directly (not just printed rows)

PRs welcome.
