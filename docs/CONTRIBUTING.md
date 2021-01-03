# Contributing

Contributions to this project are welcome and should be submitted via GitHub
pull requests. If you plan to embark on implementing a significant feature
(anything besides trivial bug fixes, or small code cleanups), please first
[create an issue](https://github.com/comby-tools/comby/issues/new/choose) to
discuss your change.

Here are some specific implementation guidelines, and also the rationale and
background behind Comby's current feature set and behavior. Please keep these
in mind if you plan to implement or suggest a change to the tool behavior, and
be patient to discuss such changes.

### PRs

- Essentially every PR (corresponding to a new feature) should include one or
  more test cases.

- Your PR build may fail if there is a significant performance regression (> 20% slower).

- Your PR build may fail if there is a reduction in code coverage. That's usually OK for small reductions.

### Comby syntax rationale and proposal guidelines

*If you are proposing a change to do with Comby metasyntax, like `:[hole]`,
please read this section. Otherwise, feel free to ignore it.*

This section discusses choices in the metasyntax of Comby (like `:[hole]`,
i.e., syntax that does not refer to concrete source code), and things to
consider if you are thinking of proposing a change.

**Template syntax.** Comby implements hole syntax like `:[hole]` that is
different from familiar syntaxes found as in, for example, regular expressions.
This syntax has been gradually expanded from `:[hole]` to [four other
forms](https://comby.dev/#match-syntax) over the course of months. These forms
roughly correspond to basic [POSIX character
classes](https://en.wikibooks.org/wiki/Regular_Expressions/POSIX_Basic_Regular_Expressions).
The design intends to keep the feature set of metasyntax small. This simplifies
the complexity of understanding and writing declarative templates. It can be
tempting to grasp for familiar syntax as in regular expressions, and it might
sometimes feel like similar behavior is missing from Comby. However, it's not a
light consideration to support similar regex syntax, as this can interact in
undesirable ways with Comby's current features. For example, suppose Comby
supported a way to match arbitrary regex within templates. Would patterns like
`(` then potentially break the ability to parse balanced parentheses later? How
would it be avoided?  It's not so simple to blacklist the `(` from this
hypothetical regex support within Comby, because the set and significance of
delimiters like `(` can change depending on language. In Bash, we might have to
blacklist `case` and `esac` from hypothetical regex support, which becomes
awkward to blacklist in regex because these are alphanumeric words.

Thus, introducing new syntax is tempting, but needs careful consideration: it
is very easy to add syntax to a language, but difficult to take it away (or
change the underlying behavior) once it is supported. In general, increasing
the amount of syntactic forms induces a higher learning curve, higher potential
cause for confusion for users, and more documentation. Lowering syntax
variations lead to more obvious and comprehensible templates, where the idea
strives to be minimal and useful, rather than comprehensive and
over-engineered.  Thus, being conservative and thoughtful about declarative
syntax, and thinking through the implications, can help to avoid problems down
the line. Thus, to set expectations: be patient and understanding with
proposals and feedback related to changing syntax (both yours and others).

**Rules.** Comby implements rules that are separate, optional inputs. One rule
is associated with a match and rewrite template. Rules impose constraints on
matches and can extend rewriting. It is an explicit decision to separate rules
(and their constraints) from templates. This avoids conflating constraints and
concrete syntax. Compare to regular expressions which mix these concerns,
leading to the possibility of very complex expressions (e.g., with match groups
and `|` clauses), and the need to escape these metacharacters (or concrete
characters).  Separate rules in Comby achieve a more declarative way for
introducing constraints, and deliver readable and escape-free match and rewrite
templates.  Thus, proposing syntax changes related to constraint should usually
be considered in the context of rules.
