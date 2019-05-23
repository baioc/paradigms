# Better Code
By Sean Parent

- Write all code as a library
    - Don't Repeat Yourself (or others)
- Regular Types
    - Goal: No Incomplete Types
- Algorithms
    - Goal: No Raw Loops (??)
- Data Structures
    - Goal: No Incidental Data Structures
- Runtime Polymorphism
    - Goal: No Inheritance
- Concurrency
    - Goal: No Raw Synchronization Primitives


## Resource Acquisition Is Initialization

Better worded as *Resource Deacquisition is Destruction*.

`new` is bad, `delete` is also bad unless when being used to undo a `new`.
Use smart pointers instead.

**A shared pointer is as good as a global variable**, but a shared pointer to an immutable (const) object has value semantics.


## Polymorphism

Polymorphism is an implementation detail that should not complicate the client code.

### Inheritance

Inheritance is a mechanism to implement runtime polymorphism where one class is derived from another and overrides some part of it's parent's implementation.
But since classes with a common inheritance may have different sizes in memory, we can only use them as "polymorphic types" by pointers to that common parent, adding heap allocations and an extra layer of indirection over the already existing virtual calls.
In this we either use raw pointers and are forced to have to deal with all their problems, or we encapsulate them in smart pointers and end up losing some standard semantics (`unique_ptr`s can't be copied, `shared_ptr`s perform shallow copying, etc),  and transform our types into *non-regular* ones.

However, there is no such thing as "polymorphic types", but polymorphic use of different types that share particular attributes.
Thus, knowing that polymorphism is only useful when it's used, we can take advantage of C++'s override mechanism and templates to automatically create polymorphic wrappers around a given type and then simply provide the attributes this interface requires.
This method is called the [**Runtime-Concept idiom**](https://www.youtube.com/watch?v=QGcVXgEVMJg).


## Data Structures

[Data structures](https://www.youtube.com/watch?v=sWgDk-o-6ZE) consist of the semantic relationships between our data.
These relationships and their properties must be thought of and never incidental (as in a shared variable) so as to provide us the ability to reason locally about the code, a fundamental [element of programming](http://elementsofprogramming.com/book.html).
An architecture is the set of all these data structures and their relationships.


## Concurrency

Avoid raw synchronization primitives.
Even with [concurrent programming](https://www.youtube.com/watch?v=zULU6Hhp42w) tools, one must always take care so as to not serialize code.
