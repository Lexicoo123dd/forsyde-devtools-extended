# ForSyDe IO Documentation
ForSyDe IO can be generated in the `.fiodl` format using the `--output-forsyde-io` flag.

This `.fiodl` file can be used by various other ForSyDe tools, examples being:
- IDeSyDe - For design space identification and exploration.
- ForSyDe IO - Querying and manipulating models.

**_NOTE:_** If IDeSyDe is run using a system with the only signals being the global inputs and outputs (e.g. a single actor with no loops), it crashes. This seems to be an error on IDeSyDe's side, as it crashes reading a system with no edges.

The generation of the ForSyDe IO output can be found in `src/Compiler/ForSyDeIO.hs`. The output will be a main `systemgraph` block containing the following information:

## Actors
An actor is represented by a vertex with the trait `SDFActor`.

An actor will always have the following traits: `[moc::sdf::SDFActor, moc::sdf::SDFComb, visualization::Visualizable]`. If the actor has a function, it will also have the trait `[impl::InstrumentedExecutable]`.

An actor will always have the ports `(combFunctions)` (which is required by `SDFComb`) and any consumption and production signals.

An actor will always implement `consumption` and `production` which give detail on what and how much it consumes/produces of each signal, and is required by the `SDFActor` trait. It will additionally implement `operationRequirements` if it has a function, which is required by the `InstrumentedExecutable` trait.

`operationRequirements` will give information on what operations are being done in the function and how many. These can currently be:
```
integer add
integer sub
integer mul
integer div
```

## Signals
A signal is represented by a vertex with the trait `SDFChannel`. 

A signal will always have the following traits: `[moc::sdf::SDFChannel, visualization::Visualizable]`.

A signal will always have the ports `(producer, consumer, initialTokenValues)`, producer and consumer being required by the `SDFChannel` trait.

A signal will always implement `numOfInitialTokens` (required by the `SDFChannel` trait) and `tokenSizeInBits`. `numOfInitialTokens` gives information on how many delays there are in the signal, while `tokenSizeInBits` gives information on the bit-size of the signal type, which can also be dependent on the system used. This will e.g. be `64_l` for an Int on a 64-bit computer.

## Edges
Information on how actors and signals connect is represented by edges. Each signal has two edges, one from actor a to the signal, and one from the signal to actor b. There are no edges for global inputs and outputs.

An edge will always have the following traits: `[moc::sdf::SDFDataEdge, visualization::VisualConnection]`. 

The first edge will have info on the actor vertex it consumes from, from which output port of that vertex, to the signal/channel vertex it connects to, and its consumption port being `"producer"`.

The second edge will have info on the signal/channel vertex it comes from, its production port being `"consumer"`, to the actor vertex it connects to, and to which input port of that vertex.
