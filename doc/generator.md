# *Protocol* GENERATOR 

## *Class* GENERATOR-MIXIN

The "abstract base class" for generator objects. By inheriting from GENERATOR-MIXIN, subclasses gain access to a minimal protocol.

## *Class* GENERATOR

A "live" generator object that represents a function with multiple entry points that "yields" intermittent to its caller.

## *Class* EXHAUSTED-GENERATOR

A "dead" generator object that has reached the end of its execution and has no other values to yield.

## *Function* MAKE-GENERATOR

### Syntax:

**make-generator** *closure* => *generator*

### Arguments and Values:

*closure* -- A closure formed within a **cl-cont:with-call/cc** environment whose argument represents the continuation to yield values to when called.

*generator* -- a **generator** object

### Description:

The function **make-generator** constructs a **generator** object from a continuation.

### Example:

````common-lisp
(make-generator 
  (lambda/cc (yield-continuation)
    (funcall yield-continuation 'hello)
    (funcall yield-continuation 'world)))
=> <GENERATOR>
````

### *Generic Function* NEXT

### Syntax:

**next** *generator* => *value* *generator*

### Arguments and Values

*generator* -- A **generator** object whose next value is to be yielded.

*value* -- The value yielded by the **generator** object.

*generator* -- The same **generator** object. 

### Description:

The default method of the generic function *next* on *generator* objects returns the yielded value as well as a reference to the **generator** provided.

If previous call to **next** has exhausted the **generator**, then the method will return **nil** and the object will change its class to **exhausted-generator**.

### Example:

````common-lisp
(defvar *gen*
  (make-generator 
    (lambda/cc (yield-continuation)
      (funcall yield-continuation 'hello)
      (funcall yield-continuation 'world))))

(next *gen*)
=> 'HELLO
=> <GENERATOR>

(next *gen*)
=> 'WORLD
=> <GENERATOR>

(next *gen*)
=> NIL
=> <EXHAUSTED-GENERATOR>
```` 

## *Generic Function* EXHAUSTEDP

### Syntax:

**exhaustedp** *generator* => *boolean*

### Arguments and Values:

*generator* -- A **generator** object

*boolean* -- **t** or **nil**

### Description:

The default method of **exhaustedp** on subclasses of **generator-mixin** returns *t* if its argument is an **exhausted-generator** and **nil** otherwise.


````common-lisp
(defvar *gen*
  (make-generator 
    (lambda/cc (yield-continuation)
      (funcall yield-continuation 'hello)
      (funcall yield-continuation 'world))))

(next *gen*)
=> 'HELLO
=> <GENERATOR>

(exhaustedp *gen*)
=> NIL

(next *gen*)
=> 'WORLD
=> <GENERATOR>

(exhaustedp *gen*)
=> NIL

(next *gen*)
=> NIL
=> <EXHAUSTED-GENERATOR>

(exhaustedp *gen*)
=> T

```` 

## *Macro* WITH-GENERATOR

### Syntax:

**with-generator** *body* => *generator*

### Arguments and Values:

*body* -- An implicit **progn**

*generator* -- A **generator** object

### Description:

A macro which provides an implicit **cl-cont:with-call/cc** environment that the provided body code is closed over. 

The **with-generator** environment provides the local macro **yield** which yields a value to the caller.

### Example:


````common-lisp
(defvar *gen*
  (with-generator
    (yield 'hello)
    (yield 'world)))

(next *gen*)
=> 'HELLO
=> <GENERATOR>

(next *gen*)
=> 'WORLD
=> <GENERATOR>

(next *gen*)
=> NIL
=> <EXHAUSTED-GENERATOR>
```` 
