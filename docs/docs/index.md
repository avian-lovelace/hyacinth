# Introduction

## About Hyacinth

Welcome to the Hyacinth docs! Hyacinth is a hybrid imperative-functional programming language with a static, nominal type system. Hyacinth is compiled to a custom bytecode format, and run by the Hyacinth bytecode VM. The design of Hyacinth is inspired by several languages including Typescipt, Haskell, and C#.

Hyacinth is an in-progress personal project of Robin Gieseking. Hyacinth is not ready or intended for any production use, and may have backwards-incompatible changes at any time. This project is mostly a vehicle for me to learn more about compilers and programming language theory. But, if you are interested in Hyacinth, feel free to check it out and play around with it a little.

The Hyacinth repository, which includes the compiler, VM, docs, and examples is located at [https://github.com/avian-lovelace/hyacinth](https://github.com/avian-lovelace/hyacinth)

## About Hyacinth Docs

This documentation explains how to install and run the Hyacinth compiler and features of the Hyacinth language. It is primarily written for my own future reference, but should be generally understandable for most people familiar with at least one other programming language.

## Acknowledgments

The following resources were very helpful to me when working on this project. A big thanks to their authors for making them available for free online!

 - [Crafting Interpreters](https://craftinginterpreters.com/) by Robert Nystrom - This book was instrumental when starting to implementing Hyacinth. *Crafting Interpreters* taught me how to structure a compiler, how a stack VM works, how to write a garbage collector, and was generally influential on most parts of this project.
 - [Bidirectional Typing Rules: A Tutorial](https://davidchristiansen.dk/tutorials/bidirectional.pdf) by David Raymond Christiansen - This paper and the accompanying talk were what really helped me understand bidirectional type checking.
 - [Parser Combinators: a Walkthrough](https://hasura.io/blog/parser-combinators-walkthrough) by Antoine Leblanc - This article was a great resource in understanding how parser combinators work and how to use them effectively.