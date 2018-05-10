# Sisyphus - The simple finite state machine generator

## What is Sisyphus?

Sisyphus is meant to be a simple to use program that generates finite state
machines, also known as finite automatons, out of a simple to read and write
definition language.

## Motivation

State machines are a omnipresent in every day programming. Sometimes they
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
code by hand. Because of its simplicity Sisyphus cannot fully compede with
full featured commercial products, but is meant to provide simple solutions to
common problems.

## Where does the name come from?

Originally I tried to form some acronym with the words *simple*, *finite*,
*state*, *machine* and *generator*, somewhat like sfsmg. I tried to read that out
loud and the name Sisyphus popped up in my head. I looked what wikipedia had to
say about him and found:

    In Greek mythology Sisyphus or Sisyphos was the king of Ephyra (now known
    as Corinth). He was punished for his self-aggrandizing craftiness and
    deceitfulness by being forced to roll an immense boulder up a hill only for
    it to roll down when it nears the top, repeating this action for eternity.
    Through the classical influence on modern culture, tasks that are both
    laborious and futile are therefore described as Sisyphean.

Having my own experience with writing state machine code I thought that was a
very funny metaphor, thus I stuck to the name Sisyphus.
