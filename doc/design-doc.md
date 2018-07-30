
# Design Doc

## Causal programming

minimize g(M, Q, D)
subject to (M, D, Q) -> F
and M in M', Q in Q', D in D'


## acausal.core

Everything should be referentially transparent.

Key functions:

- (model ...)
- (data ...)
- (query ...)
- (identify ...)

(model ...) constructs a (class of) structural causal model. For the thesis, we're going with recursive, non-parametric. Characterized by V, the set of endogenous variables, a DAG, and a confounding set. Note that this also implies conditional independence constraints.

Other extensions include PAG, linear (SEM), monotonic models.

(data ...) constructs the available *population* probabilities. Initially, the goal is to support anything of the form P(y | do\*(z)) (any subset of z), where y and z are arbitrary sets. Would like to extend to arbitrary sets of P(y | do(z), w); this would include selection bias as a special case -- but not the transportability stuff. Another possible extension is to just allow P(y | do\*(z)), but also put in a 'macro expander' frontend to support counterfactual queries.

(data ...) Takes an optional imap argument. This raises the question of what the default should be. If a model is available, I think the default imap should be 'compatible', i.e. the assumped imap should be exactly what the model suggests. If no model is available, then probably the default should be 'no independencies'. Look into making the imap a *function*: takes a query, and asks, either; "what d-seperates these", or, "are these d-seperated, given this".

(query ...) constructs a query object; permit anything of the form P(y | do(x)) at a minimum. More advanced queries would permit P(y | w, do(x)), i.e. conditional causal identification (would require a novel zIDC algorithm), or even just arbitrary queries.

(identify model query data) does the identification. Current goal: zID. By default, data is assumed to be P(V).

To support the full causal programming, we'll need additional functions. (identify ...) supports the identification task. We'll also need to support:

- Causal discovery (enumerate models, want 'least complex'; unclear how to calculate)
- (Optimal) experimental design (enumrate data sets; want minimal/least expensive)
- Query generation (enumerate queries; want 'maximial'; unclear how to calculate)

During development, also keep in some helpful visualization functions.

## acausal.viz

Visualization functions; looking at graphviz-based, but also possible support for javascript/forced-directed graph based.

## acausal.jupyter

For 'live' usage. Helper namespace? (Needs to include the protocols for rendering in the notebook.)
