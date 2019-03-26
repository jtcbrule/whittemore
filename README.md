# Whittemore

Causal programming in Clojure.


## Getting started

The easiest way to get started is with [Leiningen](https://leiningen.org) (requires [Java](https://openjdk.java.net/install/), OpenJDK 8 recommended). Once Leiningen is installed, create a new project:

    lein new whittemore demo
    cd demo

A REPL can be started from within the project directory:

    lein repl

Rich output (HTML, images, LaTeX) is available in [Jupyter](https://jupyter.org/install) notebooks. Once Jupyter installed:

    lein jupyter install-kernel # first time only
    lein jupyter notebook

[Graphviz](https://graphviz.org/download/) is recommended for rendering causal diagrams. Whittemore will automatically fallback to [viz.cljc](https://github.com/jebberjeb/viz.cljc) if Graphviz is not installed, but this is much slower.


## Examples

The `notebooks` directory has several examples.

## License

Copyright © 2018 Joshua Brule

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

