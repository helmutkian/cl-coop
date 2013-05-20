# CL-COOP

A cooperative multitasking library for Common Lisp that provides generators and coroutines. 

# Packages

## COM.HELMUTKIAN.CL-COOP.GENERATORS

**CL-COOP** allows for both low-level and high-level means of constructing generators.  They are advanced by passing them to the function **next**, and can be checked for the end of their execution with the predicate **deadp**.

The low-level route involves passing a continuation (i.e. **cl-cont::funcallable/cc** object--a closure over a **cl-cont:with-call/cc** environment) to the function **make-generator**.  The continuation takes a single argument representing the calling continuation, and calling **funcall** on this argument passes a value to the caller and suspends the execution of the generator until resumed by the caller.

````common-lisp
(defvar *g*
  (make-generator 
    (cl-cont:lambda/cc (calling-continuation)
      (funcall calling-continuation 1)
      (funcall calling-continuation 2)
      (funcall calling-continuation 3)))

(deadp *g*)
=> nil

(next *g*)
=> 1
(deadp *g*)
=> nil

(next *g*)
=> 2
(deadp *g*)
=> nil

(next *g*)
=> 3

(deadp *g*)
=> t

`````

The high-level route uses the macro **with-generator**.  This macro wraps its body in an implicit **cl-cont:with-call/cc** environment and provides the local macro **yield**.  Calls to **yield** pass the given value to the caller and suspends the execution of the generator until resumed by the caller.


````common-lisp
(defvar *g*
  (with-generator
    (yield 1)
    (yield 2)
    (yield 3)))

(next *g*)
=> 1

(next *g*)
=> 2

(next *g*)
=> 3
`````	

Because they rely on execution within a **cl-cont:with-call/cc** environment, **generator**s are subject to the same limitations as other code within the same environment.  Yielding calls, i.e. calls to the calling continuation in the low-level **make-generator** constructor and calls to **yield** in the high-level **with-generator** constructor, cannot occur within closures passed to functions not withinthe same environment such as **reduce** or **mapcar**.  The list of operators that yielding calls can occur within is [here](http://www.lispworks.com/documentation/HyperSpec/Body/03_ababa.htm), with the exceptions of **catch**, **throw**, **progv**, and **unwind-protect**.

## COM.HELMUTKIAN.CL-COOP.COROUTINES

Provides an implementation of coroutines.

## License

CL-COOP is released under the LLGPL. That is the [LGPL](http://www.gnu.org/copyleft/lgpl.html) with the [Franz preamble](http://opensource.franz.com/preamble.html).
