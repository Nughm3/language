# Toy programming language

I've been interested in programming languages and compilers for a while now, and have been tinkering with making one as a hobby project. During this process, I've learnt a lot about compilers, including syntax and semantic analysis, type systems, code generation, interning, and more.

## Features

- Primitive types: `int` and `bool`
- Pratt parsing
- (Almost) "everything is an expression"
- Higher order functions
- Type checking
- AST-walking interpreter

## Next steps

However, exploring so many topics at once has led to a lot of code churn, so I've decided to stop developing this particular implementation and start again. I hope to eventually produce a new compiler with the following features:

- Interned data structures
- Incremental compilation
- Hybrid memory management strategy
- Lossless syntax trees
- Proper error handling (this particular interpreter panics on just about anything)
- Code generation to native code with LLVM and Cranelift

I also have a few more ambitious ideas to implement such as algebraic effects or a concurrency model, but I'm not too sure how that'll go.

## License

Licensed under the [Unlicense](/LICENSE)
