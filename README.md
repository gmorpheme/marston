# marston

A functional Clojure implementation of the
[ICWS94 Draft](http://corewar.co.uk/icws94.txt) CoreWar standard.

Interpreter only, no redcode assembler yet.

This is a straight functional translation of the code in the spec.
Currently *untested* save in the REPL - there will be horrendous bugs
in here. Should be some some good test cases out there already
somewhere...

No deps so should be trivially ClojureScript-able.

## Usage

Don't.

Not fast yet but well under a second for battles with 80000-cycle
limit and parallelises cleanly. Of course, not all battles go the
distance so this is shoddy benchmarking...

```clojure
marston.core> (time (frequencies (map status (take 100 (repeatedly (fn [] (battle IMP DWARF)))))))
"Elapsed time: 28856.231407 msecs"
{[:drawn] 57, [:won "dwarf"] 43}
marston.core> (time (frequencies (map status (pmap (fn [x] (battle IMP DWARF)) (range 100)))))
"Elapsed time: 8359.896126 msecs"
{[:drawn] 68, [:won "dwarf"] 32}
```

## License

Copyright © 2014 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
