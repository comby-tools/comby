# README

This library contains the core parallel and shared memory components used in [Hack](https://github.com/facebook/hhvm/tree/master/hphp/hack), [Flow](https://github.com/facebook/flow), and [Pyre](https://github.com/facebook/pyre-check). 
The internals of these components are little known outside of these projects, yet stand to benefit the OCaml community more generally, both
for practical use and educational purposes.

This library extracts the core components, wraps them in a tidier interface, and builds with dune. It is a personal best effort
and offered 'as-is': there is no promise of maintenance, and no official support or affiliation with the projects and/or companies 
supporting the projects above.

The code contains potentially leftover and irrelevant functions (again, best effort), and do not necessarily have the most recent upstream changes of corresponding modules. This is especially the case for
modules in [procs](https://github.com/rvantonder/hack-parallel/tree/master/src/procs) and [heap](https://github.com/rvantonder/hack-parallel/tree/master/src/heap) (cf. respective [procs](https://github.com/facebook/hhvm/tree/master/hphp/hack/src/procs) and [heap](https://github.com/facebook/hhvm/tree/master/hphp/hack/src/heap) in Hack). Pull requests for
upstream changes to these files are welcome. The files in the current library are current as of around mid-2018.

# Install and Example

Hack_parallel is available on opam:

- Note: this is only available for opam 2. See [how to install opam 2](https://opam.ocaml.org/doc/Install.html).

```
opam install hack_parallel
```

Please see the example project here to get a feel for the interface: https://github.com/rvantonder/hack-parallel-example

# Some more details

The design decisions behind the parallel architecture and shared memory are best explained by this [video](https://www.youtube.com/watch?v=uXuYVUdFY48&t=0s&list=WL&index=28)
and this part of the [documentation](https://github.com/rvantonder/hack-parallel/blob/master/src/heap/hh_shared.c#L10-L76).
You can ignore the scary `THIS CODE ONLY WORKS WITH HACK`--the code does work generally, but you have to keep in mind the
restrictions on memory operations as explained in the rest of the file. The motivation behind the shared memory implementation is similar to [ancient](http://git.annexia.org/?p=ocaml-ancient.git;a=blob;f=README.txt;h=e2d9103d5f1820f89e5fd9e18f245cc330e8b29d;hb=HEAD). 

