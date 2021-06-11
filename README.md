# Cri, an implementation of IRC in OCaml for MirageOS

`cri` is a little library which implements the core of the IRC client
according to [RFC1459][RFC1459] and [RFC2812][RFC2812]. It's a Proof-of-Concept
to show the usual MirageOS development process. Specially the abstraction
required to be compatible with MirageOS.

However, a simple example exists into the `bin` directory which is a
specialisation of `cri` with `unix`.

## A simple bot logger

To help the development of `cri` and iterate on it, the distribution comes with
a simple logger which call periodically a function with saved messages. The
implementation of it is available into `lib-logger`.

Then, an unikernel (into `unikernel`) extends it to save these messages into a
Git repository (via [`ocaml-git`][ocaml-git]/[`irmin`][irmin]). It permits to
show a small unikernel as an example of what we can do with MirageOS.

## Experimental status

Many parts of the IRC protocol are not implemented yet and we still continue to
work on them. But the most common commands/responses are implemented.

[RFC1459]: https://datatracker.ietf.org/doc/html/rfc1459
[RFC2812]: https://datatracker.ietf.org/doc/html/rfc2812
[ocaml-git]: https://github.com/mirage/ocaml-git
[irmin]: https://github.com/mirage/irmin
