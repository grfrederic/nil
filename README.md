## nil : Naturwissenschaftlicher Idealistischer Logikprozessor


### description
nil is a statically typed language following the logic programming paradigm  
it's main inspiration is the Gödel programming language  
nil also has a syntax highlighting plugin for vim, [nil.vim](https://github.com/grfrederic/nil.vim)


### examples

#### program 1, addition
```
   1|    succ : Peano -> Peano  -- constructor
   2|    zero : Peano           -- constant
   3|
   4|    -- predicate, add ~x ~y ~z checks if ~x + ~y = ~z
   5|    add ? Peano Peano Peano
   6|    add zero ~x ~x
   7|    add (succ ~x) ~y (succ ~z) <- add ~x ~y ~z
   8|
   9|    -- get solutions to 1 + ~x = 3
  10|    add (succ zero) ~x (succ (succ (succ zero)))?
```
reply:
```
    |    ~x -> succ (succ zero)
```

#### program 2, generic lists
```
   1|    cons : ~A (List ~A) -> List ~A  -- list of ~A's
   2|    nil : List                      -- empty list
   3|    
   4|    head ? ~A (List ~A)
   5|    head ~a (cons ~a ~t)
   6|    
   7|    append ? List List List
   8|    append nil ~y ~y
   9|    append (cons ~h ~x) ~y (cons ~h ~z) <- append ~x ~y ~z 
  10|
  11|    a : A
  12|    head a ~l?
```
reply:
```
    |    ~l -> cons a -era_0_~t
```
the variable _-era_0_~t_ is a variable generated during SLD resolution


### syntax

#### general
constants and constructors are start with a lower case character  
variables start with a tilde, then a lower case character  
types start with a upper case character  
variable types start with a tilde, then a upper case character

#### constructors
```
    |    -- constant (constructor of arity 0) z, has type P
    |    z : P
    |
    |    -- constructor s takes one arg of type P, and creates type P
    |    s : P -> P
    |
    |    -- constructor cons takes two args:
    |    --     * one of type ~A
    |    --     * one of type List ~A
    |    -- and creates a new List ~A
    |    cons : ~A (List ~A) -> ~A
    |
    |    -- record book has two fields:
    |    --     * one of type Integer, called isbn
    |    --     * one of type String, called author
    |    book : {isbn: Integer, author: String} -> Book
```

#### relations
```
    |    -- relation p takes one arg of type A, and one of type B
    |    -- it never succeeds
    |    p ? A B
    |
    |    -- relation id takes two args of type ~A
    |    -- it succeeds if they are equal
    |    id ? ~A ~A
    |    id ~a ~a
    |
    |    -- relation sameType takes two args of type ~A
    |    -- it always succeeds, but can be used to force typing
    |    sameType ? ~A ~A
    |    sameType ~a1 ~a2
    |
    |    -- relation grandma holds, if there exists a ~m,
    |    -- so that ~m is a daughter of ~gm, and a mother of ~gd
    |    grandma ~gm ~gd <- mom ~gm ~m, mom ~m ~gd
```

#### asking questions
```
    |    -- find all children of eva, v1
    |    <- child eva ~c
    |
    |    -- find all children of eva, v2
    |    child eva ~c?
    |
    |    -- find all children of eva AND adam
    |    child eva ~c, child adam ~c?
```


### features

#### polymorphism
```
    |    id : ~A ~A
    |    id ~a ~a
```

#### generic types
```
    |    cons : ~A (List ~A) -> List ~A
```
this will also automatically generate:
```
    |    fromCons ? (List ~A) ~A (List ~A)
    |    fromCons (cons ~a ~l) ~a ~l
```

#### records
```
    |    book : {isbn: Integer, author: String} -> Book

```
this will also automatically generate:
```
    |    getIsbn ? Book Integer
    |    getIsbn (book {isbn: ~i, author: ~a} ~i
    |
    |    getAuthor ? Book String
    |    getAuthor (book {isbn: ~i, author: ~a} ~a
```

#### algebraic types
```
    |    just : ~A -> Maybe ~A 
    |    null : Maybe ~A 
```
this will also automatically generate:
```
    |    fromJust ? (Maybe ~A) ~A
    |    fromJust (just ~a) ~a
    |
    |    fromNull ? (Maybe ~A)
    |    fromNull null
```

