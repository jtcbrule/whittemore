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


## References

[Whittemore paper](https://arxiv.org/abs/1812.11918), presented at the 2019 [AAAI-WHY](https://why19.causalai.net/papers.html) Symposium

Whittemore is based on the structural causal model / graphical causal model / "Pearlian" causality approach. A good survey is [On Pearl's Hierarchy and the Foundations of Causal Inference](https://causalai.net/r60.pdf) (Bareinboim et al 2020)


## License

Copyright © 2018 Joshua Brule

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

