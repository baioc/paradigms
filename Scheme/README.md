# SICP - The Wizard Book

MIT 6.001 - Structure and Interpretation of Computer Programs

## Computer Science

Not really a science (according to Richard Feynman).
Not really about computers, in the same way that physics is not really about particle accelerators and biology is not really about microscopes.
> "Computer science is no more about computers than astronomy is about telescopes." — Edsger Dijkstra / Michael Fellows

It is in fact about two things:

1. How we define "what is true" - declarative knowledge
2. How we define "how to" do something - imperative knowledge

> Underlying our approach to this subject is our conviction that "computer science" is not a science and that its significance has little to do with computers.
> The computer revolution is a revolution in the way we think and in the way we express what we think.
> The essence of this change is the emergence of what might best be called procedural epistemology -- the study of the structure of knowledge from an imperative point of view, as opposed to the more declarative point of view taken by classical mathematical subjects.
> Mathematics provides a framework for dealing precisely with notions of "what is." Computation provides a framework for dealing precisely with notions of "how to."

Computer science brings us the **formalized knowledge about processes**, that is, to **convey with precision the imperative knowledge of how to do things**.

The pattern of rules that direct a process is called a procedure.
**And in the same way a wizard uses spells to control magic, computer scientists use procedures to control processes.**
Instead of using ancient hebrew, sumerian or arcadian to cast these spells, the arcaic and esoteric languages we use are such as LISP to express the magic behaviour of our desired programs.
A computer language, however, is not just a way of getting a computer to perform operations but rather that it is a novel formal medium for expressing ideas about methodology.
Thus, **"programs must be written for people to read, and only incidentally for machines to execute."**

Among the programs we write, some (but never enough) perform a precise mathematical function such as sorting or finding the maximum of a sequence of numbers, determining primality, or finding the square root.
We call such programs algorithms.

### Managing Complexity

As programmers fit data and program pieces to construct large systems, complexity must be controlled.
Here, computer science behaves as an abstract form of engineering: the real-world aspects of physical systems are left behind and the only constraints imposed in building sotfware are the limitations of our minds.
> "If art interprets our dreams, the computer executes them in the guise of programs!"

The most fundamental of such techniques is **the Black-box abstraction**.
Being able to define and name new pieces of knowledge and expose them as a black box entity in the language which can be used without knowing the inner details is crucial thing in managing complexity.
That way, we create levels of abstractions and minimize the number of items that we have to deal with on a certain level.

## Programming

> "The acts of the mind, wherein it exerts its power over simple ideas, are chiefly these three: 1. Combining several simple ideas into one compound one, and thus all complex ideas are made. 2. The second is bringing two ideas, whether simple or complex, together, and setting them by one another so as to take a view of them at once, without uniting them into one, by which it gets all its ideas of relations. 3. The third is separating them from all other ideas that accompany them in their real existence: this is called abstraction, and thus all its general ideas are made." - John Locke, An Essay Concerning Human Understanding (1690)

Every powerful language has three mechanisms for accomplishing this:

- primitive expressions, which represent the simplest entities the language is concerned with,
- means of combination, by which compound elements are built from simpler ones, and
- means of abstraction, by which compound elements can be named and manipulated as units.

In programming, we deal with two kinds of elements: procedures and data (later we will discover that they are really not so distinct).
Informally, data is "stuff" that we want to manipulate, and procedures are descriptions of the rules for manipulating the data.
Thus, any powerful programming language should be able to describe primitive data and primitive procedures and should have methods for combining and abstracting procedures and data.

### Definitions

> In the defining statement “A lake is a naturally occurring stretch of water”:
> “naturally occurring stretch of water” is the *definiens* and
> "lake" is the *definiendum*.

### Actions and Identity

We say that an action A had an effect on an object X (or equivalently, that X was changed by A) if some property P which was true of X *before A* became false of X *after A*.

We say that two objects X and Y are the same if any action which has an effect on X has the same effect on Y.

> "As assignment and thus change actions are introduced into our programming, we have opened ourselves up to all the horrible questions that have been plaguing philosophers for thousands of years." - Gerald Jay Sussman, MIT 6.001 (1968)

## LISP

According to [wikipedia](https://en.wikipedia.org/wiki/Lisp_programming_language): Originally specified in 1958, Lisp ("LISt Processor") is the second-oldest high-level programming language in widespread use today (Fortran is one year older).

As a *general-purpose* programming language, **LISP isn't fit to solve any particular problem.
Instead, it is useful for constructing within it the language that solves the problems one wants to solve**.
Lisp is particularly well suited to this task, because of its ability to represent and manipulate symbolic expressions.
This makes it "general-purpose" in an unexpected sort of way.

### Metalinguistic Abstraction

Metalinguistic abstraction - talking about and establishing new languages - plays an important role in all branches of engineering design.
It is particularly important to computer programming, because in programming not only can we formulate new languages but we can also implement these languages by constructing evaluators.

An evaluator (or interpreter) for a programming language is a procedure that, when applied to an expression (a sequence of symbols) of the language, performs the actions required to evaluate that expression.
An evaluator that is written in the same language that it evaluates is said to be *metacircular*.

**It is no exaggeration to regard this as the most fundamental idea in programming**:
The evaluator, which determines the meaning of expressions in a programming language, is just another program.
To appreciate this point is to change our images of ourselves as programmers.
We come to see ourselves as designers of languages, rather than only users of languages designed by others.

> An interpreter raises the machine to the level of the user program; a compiler lowers the user program to the level of the machine language.
> We can regard the Scheme language (or any programming language) as a coherent family of abstractions erected on the machine language.
