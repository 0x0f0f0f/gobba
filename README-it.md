# minicaml

**minicaml** è un piccolo linguaggio di programmazione interpretato puramente
funzionale con uno scopo didattico. Ho scritto **minicaml** per il corso
**Programmazione 2** dell'Università di Pisa, insegnata dai professori Gianluigi
Ferrari e Francesca Levi. Si basa sul progetto [minicaml]
(http://pages.di.unipi.it/levi/codice-18/evalFunEnvFull.ml) degli insegnanti,
ovvero un esempio di valutazione per mostrare come funzionano gli interpreti. È
un sottoinsieme interpretato di Caml, con valutazione eager e solo dichiarazioni
locali (`let-in`). Ho aggiunto un semplice parser e lexer realizzato con menhir
e ocamllex ([impara di più]
(https://v1.realworldocaml.org/v1/en/html/parsing-with-ocamllex-and-menhir.html)).
Ho anche aggiunto un semplice REPL (shell) che mostra ogni passaggio di
riduzione che viene eseguito nel valutare un'espressione. Vorrei, in un prossimo
futuro, implementare un semplice compilatore e una macchina astratta per questo
progetto.

**minicaml** implementa solo tipi di dati basilari (interi e booleani) e non
sarà mai un linguaggio di programmazione completo per da utilizzare nel mondo
reale. **L'unico scopo di minicaml è aiutare gli studenti a comprendere come
funzionano gli interpreti e i linguaggi di programmazione**

## Installazione
Rilascerò un file binario (non è necessario compilarlo) nel prossimo futuro. Per
installare, devi avere `opam` (gestore dei pacchetti di OCaml) e una
distribuzione di OCaml recente installata sul tuo sistema.
[rlwrap](https://github.com/hanslub42/rlwrap) è suggerito per un'esperienza con
scorciatoie da tastiera come su bash.

```bash
# clona il repository
git clone https://github.com/0x0f0f0f/minicaml
# cd nella cartella
cd minicaml
# installa le dipendenze
opam install dune menhir ANSITerminal
# compila
make
# esegui
make run
# rlwrap è suggerito
rlwrap make run
# puoi installare minicaml eseguendo
make install
# esegui minicaml installato
rlwrap minicaml
```

## Utilizzo

Esegui `make run` per eseguire un REPL. Il REPL mostra l'AST equivalente ad ogni
espressione inserita, e viene mostrato ogni passaggio di riduzione nella
valutazione delle espressioni. Segnala anche errori semantici e di sintassi.

