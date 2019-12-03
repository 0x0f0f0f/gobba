(lang dune 1.10)
(name minicaml)
(using menhir 2.0)
(source (github 0x0f0f0f/minicaml))
(license MIT)
(authors "Alessandro Cheli")
(maintainers "sudo-woodo3@protonmail.com")

(package
    (name minicaml)
    (synopsis "A simple, didactical, purely functional programming language")
    (description "A simple, didactical, purely functional programming language written for the programming 2 course at the University of Pisa, extended with a simple parser made with Menhir and ocamllex")
    (depends
        (menhir (> 20190924))
        (ANSITerminal (> 0.8.1))))
