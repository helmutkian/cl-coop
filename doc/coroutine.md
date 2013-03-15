# *Protocol* Coroutine

## *Class* COROUTINE-QUEUE

Dispatch queue for coroutines. Coroutines may yield a pointer to another coroutine in the queue to directly dispatch to or yield to the next coroutine within the queue.

## *Class* COROUTINE-MIXIN

Abstract base class that gives its subclasses minimal access to the **coroutine** protocol.

## *Class* COROUTINE

A "live" coroutine that can continue to execute statements.

## *Class* EXHAUSTED-COROUTINE

A "dead" coroutine whose execution has ceased.

## *Function* MAKE-COROUTINE-QUEUE

A constructor for an empty **coroutine-queue**.

## *Function* MAKE-COROUTINE

### Description:

The function **make-coroutine** constructs a "raw" **coroutine** object that can either be run on its own as a **generator** or added to a **coroutine-queue**.

## *Function* PUSH-COROUTINE

### Description:

Pushes a **coroutine** object onto a **coroutine-queue**.

## *Function* START-COROUTINES

### Description

Begins the exection of the **coroutine**s within a  **coroutine-queue**.