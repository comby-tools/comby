# Roadmap

The purpose of this file is to give a high-level overview of planned features. The intent is to communicate the relative priority of these features. See this as a hopeful wishlist, rather than an exact timeline or promise, since it's difficult to predict how long some features may take to implement.

### In the next month (Oct 2019)
- [x] [Interactive editing mode](https://github.com/comby-tools/comby/pull/104), a la codemod.
- [ ] Official support for match rules that contain hole syntax. This needs planning to document and test the desired semantics.
- [ ] Matching support for arbitrary tagged delimiters, like HTML `<p>foo</p>`.
- [ ] Switching horizontal/vertical layout in comby.live.
- [ ] Work out matching semantics for "first argument" in common functon syntax.

### In the coming 3 to 6 months (Oct 2019 - Feb 2020)
- [ ] Indentation-sensitive support.
- [ ] Nested rewrite rules and a semantics defining the interaction with match rules.
- [ ] A more efficient rewrite of the parser engine
