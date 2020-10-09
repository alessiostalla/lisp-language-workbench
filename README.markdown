# Treep

Treep is the Tree-Processing language built on top of Common Lisp. It aims to be the core of a "language workbench" type of application, to aid in the implementation of: 
 * Domain-Specific Languages (DSLs)
 * Static Analysis Tools
 * Editors (traditional and projectional)
 * Translators/Generators
 * Everything else revolving around Programming Languages.

## Goals

 * Provide a set of object-oriented building blocks for programming languages
 * Make it easy to represent them as text (in S-expression notation, XML, etc.)
 * Provide a framework for tree-to-tree transformations
 * Provide a useful runtime environment for the above, including:
   * A self-hosting (metacircular) interpreter
   * A compiler (using the underlying Common Lisp compiler)
   * A REPL
   * A debugger
 * Provide useful examples such as:
   * A type system with inference and some support from the compiler
   * A web-based code (model) browser
   * Graphical transformations, visualizations, and such
   * Integrations with existing tools (e.g. MPS, web-based projectional editors, etc.)
   * Etc.
 * Write thorough documentation for all the above
 * Generate interest in the project, spread the word
   
## Non-goals

 * To write a full IDE or projectional editor
 * To write an industrial-grade optimizing compiler
 
## Design Principles

 * Code is made of *forms* (expressions) which are CLOS objects.
 * Forms may *contain* other forms (e.g. a function declaration will contain parameter declarations and a body),
   thus they're organized in a tree.
 * Names are symbols, not strings.
 * Some forms may use symbols as references to other forms/definitions, so the tree is actually a graph with some preferential edges (the parent-child relationships).
 * Symbols are hierarchical (foo:bar:baz), so they can easily map to languages with multiple levels of namespacing
   (e.g., package.class.member).
 * Environments (also known as symbol tables) are first-class objects.
 * Loading code has no side effects. The load function returns a module which consists of:
   * A symbol with all the newly read symbols interned;
   * An environment with the new definitions added.
 * The system keeps track of who-calls-who so that, when installing new definitions into an environment, dependant
   functions can be recompiled (e.g., if the signature of a function changed).
 * Everything can be inspected. Functions retain information about their arguments (including types).

## Usage

At the present time, the language is not yet usable in practice, too many pieces are missing, in particular, a REPL. Stay tuned. 

## Installation

Treep is developed and tested on ABCL and SBCL.
 * Install a Lisp implementation, preferably ABCL or SBCL.
 * Install Quicklisp.
 * Load treep with ASDF.