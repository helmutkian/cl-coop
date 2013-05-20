
# CL-COOP

## What is cooperative multitasking?

Cooperative multitasking is contrasted with preemptive multitasking.  With preemptive multitasking, usually encountered in the traditional threading approach to multitasking, either the operating system, virtual machine, or language runtime determines the execution of tasks with little-to-no input from the code itself.  With cooperative multitasking, each task must manually suspend its own execution in order to permit the execution of other tasks.  In preemptive multitasking, individual tasks are usually called threads.  In cooperative multitasking, individual tasks are usually called coroutines.  CL-COOP provides a coroutine implementation and an interface to make the use of cooperative multitasking simpler.

Tradition functions or "routines," are a subset of coroutines that simply suspend their execution as their last statement.  CL-COOP treats Common Lisp functions as coroutines and allows for them to interoperate.

## How do coroutines work?

Coroutines work by the programmer specifying in the body of the coroutine itself where execution may be suspended and resumed. There are three ways of suspending the currently executing coroutine.  

### Yielding

The first is by suspending the current coroutine and yielding program flow to the function or coroutine which had immediately resumed its execution:

````common-lisp
(defvar *coro*
  (with-coroutine ()
    (print "hi")
    (yield)
    (print "bye")))

(defun f ()
  (print "say hi")
  (next *coro*)
  (print "say bye")
  (next *coro*))


(f)
=> say hi
=> hi
=> say bye
=> bye
````

### Swapping

Secondly, the current coroutine can be explicitly swapped for another coroutine without regard to the calling task.  This can done with what is called a "trampoline" function:

````common-lisp

(defun trampoline (coro &rest arg)
  (multiple-value-bind (swap-coro arg) (apply #'next coro arg)
    (trampoline swap-coro arg)))

(defun make-consumer (producer)
  (with-coroutine ()
    (print "CONSUMING")
    (print (yield producer))
    (print "CONSUMING")	
    (print (yield producer))))

(defun make-producer (consumer)
  (with-coroutine
    (print "PRODUCING")
    (yield consumer 'a)
    (print "PRODUCING")	
    (yield consumer 'b)))

(defvar *the-producer* nil)

(defvar *the-consumer* 
  (make-consumer *the-producer*))

(setf *the-producer* 
      (make-producer *the-consumer*))

(trampoline *the-consumer*)

=> CONSUMING
=> PRODUCING
=> A
=> CONSUMING
=> PRODUCING
=> B

````

Obviously, without tail-call optimization, using a trampoline function could readily lead to stack overflow if there was enough "swapping" of coroutines.

### Pipes

Another common use for cooperative multitasking is building queues of processes, similar to application pipes in Unix-like systems.  This can be down by having a scheduling function that maintains a queue of coroutines and ceases execution when the first coroutine in the sequence has been exhausted.

````common-lisp

(defvar *pipe* (make-queue))

(defun start-pipe (pipe &rest initarg)
  (let ((head (queue-front pipe)))
    (loop until (deadp head)
          for value 
              = (apply #'next (queue-pop pipe) initarg)
              then (apply #'next (queue-pop pipe) value))))

(defvar *number-generator*
  (with-coroutine ()
    (dotimes (i 3)
      (yield i))))

(queue-push *pipe* *number-generator*)
(queue-push *pipe* #'1+)
(queue-push *pipe* #'print)

(start-pipe *pipe*)

=> 1
=> 2
=> 3
````