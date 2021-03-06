# WorkSheet

## Introduction

WorkSheet (insert better name) is a higly collaborative online IDE. It's an experimental project for me to test out some of my hypothesis relating to the field of IDE development.

At this point my focus is entirely on creating a compelling experience for one or more developers looking to test out some algorithm, perform an online interview, etc. I believe this is the use case that stands to gain the most from the advantage of a web app, ie no overhead, no configuration, no installation.

## Motivation

Although, I am focusing on the use case described above, WorkSheet remains a bit of a kitchen sink of different ideas. I have consciensly decided not to focus on a single idea and attempt to create something genuinly practical in the foreseable future. This doesn't exclude the possibility of focusing at a later point in time and attempting to produce a practical tool.

Here is a short summary of things I want to test with this project:

- results are continously printed out for the user to see
- print out the result for each line
- realtime collaboration
- Optionnaly typed language
- Integrated versionning
- no more text files

### Continous results

Why bother with a "run" button? Let's just use websockets to interpret the code as soon as the user changes the code. This probably works fairly well for most use cases and should only get better with better network connectivity and computer performance.

For an example of a project pursuing this idea, see LightTable.

### Print out the results for each line

Printing out each line of code is generally very useful. In particular it often allows to avoid print statements entirely for the target audience.

### Realtime collaboration

Sometimes, it would be usefull for two people to work on the same "code base" at the same time. This requires a flexible dynamic language in order to work acceptably.

### Optionnaly typed language

For a language to truly be scalable, it needs to have optionnal typing in my opinion. When writing a few lines of simple code (like api linking), it often totally useless to have types. However, types can be extremely usefull to avoid test cases and to validate complex logic. I believe that duck typing should be the default typing paradigm with the possibility for a programmer to document in the code the way he expectes his API to be used. This can then later be validated by a static analysis pass.

Even declared types should not stop a programmer from being able to call a function with a different type. It should only be there so that the static analysis pass can warn about declared type mismatchs.

### Integrated versionning

Versionning is a very useful tool for a software engineer. It should be as easy as possible to version one's code. It would also be posssible to levarage a knowledge of the languages AST in order to produce better diffs.

### No more text files

I believe that text files are a horrible way to persist a code base. I believe we could achieve more interesting languages by storing them in a format such as xml. No only would this allow to decouple style from logic. It would also more easily allow for inovative ways for IDE to present code to programmers (think UML diagrams) and allow programming language designers to worry less about the semantics of a language to respect the limited expressive power of a raw text file.

## Inspiration

### Scalakata

Scalakata is a simple web app that allows you to test out code in the Scala programming language. I use Scalakata on a daily basis and it has a lot to make me realize the power of low overhead development environments.

### Scala Worksheet

Scala WorkSheet is an Eclipse plugin that showed me the power of printing the result for each line of code.

### Gilad Bracha

I have learned alot about programming language design from Gilad Bracha. I first learnt about Optional typing from him and the possibilities it offers from him.

He is currently working on Dart, an optionnally typed language for the web developed at Google. I would very much like to write my client side code for this project in Dart.

## Roadmap

As part of a school project. I will attempt to bring this project to version 0.1 as defined in the milestones in the issue tracker.

In order to attain that goal, I will be performing weekly sprints. This amounts to 10 milestones defined as 0.0.1 .. 0.0.9 in the issue tracker.
