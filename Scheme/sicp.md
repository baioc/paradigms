# MIT 6.001 Structure and Interpretation of Computer Programs

## Computer Science

Not really a science (according to Richard Feynman).
Not really about computers, in the same way that physics is not really about particle accelerators and biology is not really about microscopes.
Computer science brings us the **formalized knowledge about processes**, that is, to **convey with precision the imperative knowledge of how to do things**.
> "Computer science is no more about computers than astronomy is about telescopes." — Edsger Dijkstra / Michael Fellows

The pattern of rules that direct a process is called a procedure.
**And in the same way a wizard uses spells to control magic, computer scientists use procedures to control processes.**
Instead of using ancient hebrew, sumerian or arcadian to cast these spells, the arcaic and esoteric languages we use are such as LISP to express the magic behaviour of our desired programs.
A computer language, however, is not just a way of getting a computer to perform operations but rather that it is a novel formal medium for expressing ideas about methodology.
Thus, **"programs must be written for people to read, and only incidentally for machines to execute."**

Among the programs we write, some (but never enough) perform a precise mathematical function such as sorting or finding the maximum of a sequence of numbers, determining primality, or finding the square root.
We call such programs algorithms

As programmers fit data and program pieces to construct large systems, complexity must be controlled.
Here, computer science behaves as an abstract form of engineering: the real-world aspects of physical systems are left behind and the only constraints imposed in building sotfware are the limitations of our minds.
> "If art interprets our dreams, the computer executes them in the guise of programs!"

## Programming

> "The acts of the mind, wherein it exerts its power over simple ideas, are chiefly these three: 1. Combining several simple ideas into one compound one, and thus all complex ideas are made. 2. The second is bringing two ideas, whether simple or complex, together, and setting them by one another so as to take a view of them at once, without uniting them into one, by which it gets all its ideas of relations. 3. The third is separating them from all other ideas that accompany them in their real existence: this is called abstraction, and thus all its general ideas are made." - John Locke, An Essay Concerning Human Understanding (1690)

Every powerful language has three mechanisms for accomplishing this:

- primitive expressions, which represent the simplest entities the language is concerned with,
- means of combination, by which compound elements are built from simpler ones, and
- means of abstraction, by which compound elements can be named and manipulated as units.

In programming, we deal with two kinds of elements: procedures and data (later we will discover that they are really not so distinct.).
Informally, data is "stuff" that we want to manipulate, and procedures are descriptions of the rules for manipulating the data.
Thus, any powerful programming language should be able to describe primitive data and primitive procedures and should have methods for combining and abstracting procedures and data.

### Definitions

> In the defining statement “A lake is a naturally occurring stretch of water”:
> “naturally occurring stretch of water” is the *definiens* and
> "lake" is the *definiendum*.
