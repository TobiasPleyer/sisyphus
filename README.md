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
style Haskell programs [Alex](https://www.haskell.org/alex/) and
[Happy](https://www.haskell.org/happy/), thus these are dependencies for
Sisyphus.

Actually compiling the program can be done either with
[Cabal](https://www.haskell.org/cabal/) or the stack tool from the
[Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/).
Conveniently all of the above mentioned dependencies are included in the
cross-platform
[Haskell Platform](https://www.haskell.org/platform/). Further you will need
git or download a .zip if you want the sources.

So the recommended way to install Sisyphus is to:

1. Install the Stack Tool
2. Install git and clone the project or download the zipped package
3. Type the following commands in the terminal in the projects root:

```bash
    make
    make install
```

After that the *sisyphus* program should be available in your PATH.
Type `sisyphus --help` for a test. You should see Sisyphus' help text printed.

## Usage

Sisyphus generates its output from so called grammar files. Grammar files are simply
text files with a syntax understood by Sisyphus.

The list of supported input formats can be found below. Currently the only supported
syntax is Sisyphus' own domain specific language, called
Sisyphus Grammar Format (SGF), which is very closely related to the
[PlantUML state diagram specification](http://plantuml.com/state-diagram).
PlantUML was chosen as the basis of the language because
it is already a well known specification standard with a good support for advanced state chart
features, such has hierarchical and parallel state machines.

Note however that PlantUML and SGF are not a 100% interchangeable! The details about
the commonalities and differences deserves its own section below.

### Command Line Interface

Sisyphus is meant to be used in the command line. Here is its own help text:

```
~$ sisyphus --help
Sisyphus - Finite state machine generator v1.0.0.0

sisyphus [OPTIONS] [PATH]

Common flags:
  -W --no-warnings         Don't print warnings
  -E --warn-is-error       Warnings are treated as errors
  -p --print-statemachine  Print the parsed state machine
  -d --outputdir=DIR       Output will go in this directory
  -T --template-root=ITEM  The root directory of the sisyphus templates
  -n --fsm-name=NAME       The name used for file names and variable prefixes
  -i --input-format=INPUT  The input file format
  -t --targets=TARGET      The target language of the generated FSM
  -? --help                Display help message
  -V --version             Print version information
     --numeric-version     Print just the version number
```

The options and arguments are listed in more detail in the table below:

| Long name          | Short name | Description                                                                                          |
|--------------------|------------|------------------------------------------------------------------------------------------------------|
| no-warnings        | W          | Warnings, if any, are silently ignored                                                               |
| warn-is-error      | E          | Warnings, if any, will be treated like errors                                                        |
| print-statemachine | p          | The state machine parsed from the input file is printed on stdout                                    |
| outputdir          | d          | The root directory of the output. All generated files will go there                                  |
| template-root      | T          | The root directory of the sisyphus templates. The are required for the generation process            |
| fsm-name           | n          | This string is used to prefix the state machine output files as well as variables within these files |
| input-format       | i          | The grammar format of the input grammar file                                                         |
| targets            | t          | This option can be repeated. Every specified target language will be generated                       |
| help               | ?          | Display help message                                                                                 |
| version            | V          | Print version information                                                                            |

A typical usage could look like this (taken from the GarageDoor example):

```bash
~$ cd path/to/sisyphus_git_root/examples/GarageDoor
~$ sisyphus -pE -T../../templates -nGarageDoorFSM -d. -tC GarageDoor.sgf
```

This will result in the files `GarageDoorFSM.h` and `GarageDoorFSM.c` which you can
compile into your application.

If you want a quick visualization of your state machine code you can copy/paste the
grammar file content into the [PlantText](https://www.planttext.com/) online tool.
Please not that the PlantText tool expects the input to be PlantUML, so even
though most Sisyphus files will meet these constraints it might well be that the
tool's parser will reject the input with a syntax error.

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

## Currently supported input formats

    - SGF: Sisyphus Grammar Format

## Currently supported target languages

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
    - [ ] Support SCXML as input format
    - [ ] Support for internal variables
    - [ ] Support for guards
    - [ ] Hierarchical state machines
    - [ ] Support for 3rd party state machines

## Sisyphus Grammar Format

The theory behind Sisyphus and thus its grammar is that of statecharts.
The term statechart were first coined by David Harel in his 1984 paper
'Statecharts: A visual formalism for complex systems' and has inspired
many systems and applications in the years that followed. The concept
of state charts has been further formalised in the UML standard.
Sisyphus tries to stick to the UML specifications as closely as possible,
specifically to the chapter 14 of the UML2.5 standard.

Sisyphus' own grammar lends itself a lot from
[PlantUML state diagram specification](http://plantuml.com/state-diagram). 
Even though closely related to each other these language specifications are
not fully compatible. In the following the main differences are enlisted:

    - In Sisyphus states have to be declared before usage
    - In Sisyphus different states can have the same name as long as their
      scope is different. If you want to refer to a state that does not
      have a unique name the name must be (partially) qualified by its scope
      until it is unambiguously clear which state is referred at
    - As a consequence of the prior two bulletin it is possbile to define
      transitions from and to states anywhere in the grammar file as long
      as you scope the states appropriately
    - PlantUML and PlantText are purely about the textual and visual
      depiction of the state chart. Sisyphus however gives a semantic meaning
      to the contents. As a consequence PlantUML allows any text to be placed
      in a state. Sisyphus on the other hand interprets the text as behavior:
      entry, exit and internal reactions
    - Entry, exit and internal reactions are introduced via the <entry>,
      <exit> and <internal> specifiers

The following things are identical:

    - PlantUML notes syntax, but Sisyphus will simply ignore them
    - A simple state (no inner elements) is simply introduced with the
      declaration `state STATENAME`
    - A nested state (contains other states etc.) is written as
      `state STATENAME {...}`, where the dots stand for the body. If the body
      is empty Sisyphus will consider the state to be a simple state
    - Parallel state machines, a.k.a. regions are created by either two or
      more dashses `-` or two or more pipes `|`
    - A transition with the special star state (`[*]`) as source marks the
      initial state of a region
    - A transition with the special star state (`[*]`) as target marks a
      final state of a region
    - The trigger of a transition is given after a colon `:`

In general a state machine has to react on events, possibly triggering a
complex cascade of inner (private) events, also called signals.
The following behavior elements exist: entry reactions, exit reactions,
internal reactions and transitions. Internal reactions and transitions are
made up of a trigger, an optional boolean guard function and an optional
behavior side which describes the behavior of the element towards stimulation.
An internal reaction never leaves the state it belongs to and the state is
never considered as exited or entered. A transition on the other hand, even
if the source state equals the destination state, always causes the source
state to be exited and the destination state to be entered.

The textual representation of the full action specification for an internal
reaction or a transition follows the following syntax:

```
{trigger}* ['['<guard>']'] ['/'<behavior-expression>]
e.g. evButtonPress [!Switched_Off] / ^evPressed @open
```

Currently guards are not supported in Sisyphus. Events (signals) emitted by
a transition are prefixed by a `^`, actions to be executed are prefixed
by a `@`. By this convention it is possible that an event and an action share
the same name without any ambiguities.

It is not easy to fully formalize these rules. Sisyphus' grammar is best
explained by examples, so please have a look at the files in the examples
directory.

## Contributing

Even though Sisyphus is written in Haskell literally everyone that knows a
programming language can contribute! Why? Because ultimately Sisyphus wants
to generate code for every possible target language. If you see your favorite
programming language missing, please feel free to contribute. Even with no
programming knowledge at all, contributions to the documentation, better
comments and typo corrections are always welcome.
