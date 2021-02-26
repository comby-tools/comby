===================================================
MParser, a simple monadic parser combinator library
===================================================

This library implements a rather complete and efficient monadic parser
combinator library similar to the Parsec library for Haskell by Daan Leijen
and the FParsec library for FSharp by Stephan Tolksdorf.

See the file LICENSE.txt for copying conditions.

Home page: https://github.com/cakeplus/mparser

MParser used to be a part of ocaml-base, a collection of useful OCaml
libraries by Holger Arnold [1]_.

The monadic interface of MParser is compatible with pa_monad [2]_.


Dependencies
------------

In order to compile this package, you will need:

* ocaml (>= 3.11).
* findlib [3]_.
* optional dependency: pcre-ocaml [4]_ for ``mparser.pcre``.
* optional dependency: re [5]_ for ``mparser.re``.


Installing
----------

1. Uncompress the source archive and go to the root of the package.
2. Run ``ocaml setup.ml -configure``. Optional flags:

   - ``--enable-pcre`` -- support for PCRE-based regular expressions
     (``MParser_PCRE`` module, ``mparser.pcre`` findlib package).
   - ``--enable-re`` -- support for RE-based regular expressions
     (``MParser_RE`` module, ``mparser.re`` findlib package).

3. Run ``ocaml setup.ml -build``.
4. Run ``ocaml setup.ml -install``.
5. Optionally, run ``ocaml setup.ml -doc`` to produce an HTML API reference.


Uninstalling
------------

1. Go to the root of the package
2. Run ``ocaml setup.ml -uninstall``


Usage example
-------------

Let's implement a simple expression evaluator.

To save the typing effort, it is often handy to open the ``MParser`` module:

.. sourcecode:: ocaml

  open MParser


First, we define a parsing combinator ``expr``, which handles expression
parsing, taking care of the operator precedence issues:

.. sourcecode:: ocaml

  let infix p op =
    Infix (p |>> (fun _ a b -> (`Binop (op, a, b))), Assoc_left)

  let operators =
    [
      [
        infix (char '*') `Mul;
        infix (char '/') `Div;
      ];
      [
        infix (char '+') `Add;
        infix (char '-') `Sub;
      ];
    ]

  let decimal =
    many1_chars digit |>> int_of_string

  let expr =
    expression operators (decimal |>> fun i -> `Int i)


Next, we implement an interpreter for our expression tree:

.. sourcecode:: ocaml

  let rec calc = function
    | `Int i -> i
    | `Binop (op, a, b) ->
        match op with
          | `Add -> calc a + calc b
          | `Sub -> calc a - calc b
          | `Mul -> calc a * calc b
          | `Div -> calc a / calc b


The evaluator function:

.. sourcecode:: ocaml

  let eval (s: string) : int =
    match MParser.parse_string expr s () with
      | Success e ->
          calc e
      | Failed (msg, e) ->
          failwith msg


Using it:

.. sourcecode:: ocaml

  eval "4*4+10/2"  ->  21


Have fun!


References
----------

.. [1] http://www.holgerarnold.net/software
.. [2] http://www.cas.mcmaster.ca/~carette/pa_monad
.. [3] http://projects.camlcity.org/projects/findlib.html
.. [4] https://bitbucket.org/mmottl/pcre-ocaml
.. [5] https://github.com/ocaml/ocaml-re
