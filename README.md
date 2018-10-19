<img src="http://tpleyer.de/Sisyphus_Logo.png" alt="logo" width="250px"/>

# Sisyphus - The simple finite state machine generator

## What is Sisyphus?

Sisyphus is meant to be a simple to use command line program to generates
finite state machines, also known as finite automatons, into other target
languages. Sisyphus is written in Haskell.

## Where does the name come from?

Writing state machines is a monotonic, tedious and repetitive work. Yet you
have to be careful to get the details right. This kind of tasks are nowadays
called Sisyphean tasks:

    In Greek mythology Sisyphus or Sisyphos was the king of Ephyra (now known
    as Corinth). He was punished for his self-aggrandizing craftiness and
    deceitfulness by being forced to roll an immense boulder up a hill only for
    it to roll down when it nears the top, repeating this action for eternity.
    Through the classical influence on modern culture, tasks that are both
    laborious and futile are therefore described as Sisyphean.

Having my own experience with writing state machine code I thought that was a
very funny metaphor, thus I stuck to the name Sisyphus.

## Installation

Sisyphus auto-generates its lexer and parser code. For this it uses the yacc
style Haskell programs [Alex] and [Happy], thus these are dependencies for
Sisyphus.

Actually compiling the program can be done either with [Cabal] or the stack
tool from the [Haskell Tool Stack]. Conveniently all of the above mentioned
dependencies are included in the cross-platform [Haskell Platform]. Further
you will need git or download a .zip if you want the sources.

So the recommended way to install Sisyphus is to:

1. Install the Haskell Platform
2. Install git and clone the project or download the zipped package
3. Type the following commands in the terminal in the projects root:

```bash
    alex -o src/Sisyphus/Lexer.hs specs/Lexer.x
    happy -o src/Sisyphus/Parser.hs specs/Parser.y
    stack build
    stack install
```

After that the *sisyphus* program should be available. Type `sisyphus --help`
for a test. You should see Sisyphus' help text printed.

[Alex]: (https://www.haskell.org/alex/)
[Happy]: (https://www.haskell.org/happy/)
[Cabal]: (https://www.haskell.org/cabal/)
[Haskell Tool Stack]: (https://docs.haskellstack.org/en/stable/README/)
[Haskell Platform]: (https://docs.haskellstack.org/en/stable/README/)

## Goals

    - Simple
    - Fast
    - Readable
    - Maximum target language support
    - Aims to be the pandoc of state machines

## Target Group

Sisyphus is meant to be the right choice for the common use case: A simple state
machine with less than 30 states. This is exactly the niche where hand written
code starts to become unreadable und hard to maintain and commercial solutions
are overkill, or don't justify their price respectively.

## Motivation

State machines are omnipresent in every day programming. Sometimes they
appear in so simple forms that people do not notice them, sometimes they hide
deep inside so called "spaghetti code", but the concept of states and
transitioning between them is our means to describe the world.

Yet there are no good tools that manage the balancing act of

    - Being easy and fast to use
    - Free of charge
    - Expressive

Usually programmers choose one of the following options

    1. Hand write a state machine in their programming language of choice
    2. Use a sophisticated UML designer program and auto-generate code from it

The first options tends to become un-maintanable very quickly, but is ususally
the fastest option and free of charge.

The second option allows to create very complex state machines that are still
maintainable and comprehensible, because visualization greatly helps to
understand the states and transitions. But these tools have definitely a higher
learning curve and are commercial products.

Sisyphus tries to fit inbetween both options. It is based on a very simple
description language which allows to write down logic quickly. From this simple
text file code will be generated, alleviating the programmer from writing the
code by hand. Because of its simplicity Sisyphus cannot fully compete with
full featured commercial products, but is meant to provide simple solutions to
common problems.

## Currently supported target languages

    - Graphviz
    - C

## What Sisyphus is not

    - Not the next Simulink, Enterprise Architect or VisualState
    - Not meant as a graphical tool (no GUI planned)

## Future work

    - Full conformity to the UML standard (e.g. hierarchical state machines)
    - Ability to read commercial file formats and generate code from them

## Milestones

    - [x] Sisyphus grammar specification
    - [x] Lexer/Parser for grammar
    - [x] Plausibility checks for grammar input
    - [x] Code generation mechanism
    - [x] Produce code in C
    - [ ] Produce code in C++
    - [ ] Produce code in Python
    - [ ] Support for internal variables
    - [ ] Support for guards
    - [ ] Hierarchical state machines
    - [ ] Support for 3rd party state machines

## Contributing

Even though Sisyphus is written in Haskell literally everyone that knows a
programming language can contribute! Why? Because ultimately Sisyphus wants
to generate code for every possible target language. If you see your favorite
programming language missing, please feel free to contribute. Even with no
programming knowledge at all, contributions to the documentation, better
comments and typo corrections are always welcome.
