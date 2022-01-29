Sham: A DSL for runtime code generation.

see the paper [Sham: A DSL for Fast DSLs](https://arxiv.org/pdf/2005.09028.pdf)

Rajan Walia , Chung-chieh Shan , and Sam Tobin-Hochstadt

Indiana University, Bloomington, United States

> **Abstract** Domain-specific languages (DSLs) are touted as both easy to embed in programs and easy to optimize. Yet these goals are often in tension. Embedded or internal DSLs fit naturally with a host language, while inheriting the host’s performance characteristics. External DSLs can use external optimizers and languages but sit apart from the host.
> 
> We present Sham, a toolkit designed to enable internal DSLs with high performance. Sham provides a domain-specific language (embedded in Racket) for implementing other high-performance DSLs, with transparent compilation to assembly code at runtime. Sham is well suited as both a compilation target for other embedded DSLs and for transparently replacing DSL support code with faster versions. Sham provides seamless inter-operation with its host language without requiring any additional effort from its users. Sham also provides a framework for defining language syntax which implements Sham’s own language interface as well.
> 
> We validate Sham’s design on a series of case studies, ranging from Krishnamurthi’s classic automata DSL to a sound synthesis DSL and a probabilistic programming language. All of these are existing DSLs where we replaced the backend using Sham, resulting in major performance gains. We present an example-driven description of how Sham can smoothly enhance an existing DSL into a high-performance one.
> 
> When compared to existing approaches for implementing high-performance DSLs, Sham’s design aims for both simplicity and programmer control. This makes it easier to port our techniques to other languages and frameworks, or borrow Sham’s innovations “à la carte” without adopting the whole approach. Sham builds a sophisticated and powerful DSL construction toolkit atop fundamental language features including higher-order functions, data structures, and a foreign-function interface (FFI), all readily available in other languages. Furthermore, Sham’s approach allows DSL developers to simply write functions, either using Sham or generating Sham, without needing to work through complex staging or partial evaluation systems.

