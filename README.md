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
marston.core>  (time (frequencies (map status (take 100 (repeatedly (fn [] (battle IMP DWARF)))))))
"Elapsed time: 43235.078962 msecs"
{[:drawn] 76, [:won "dwarf"] 24}
marston.core> (time (frequencies (map status (pmap (fn [x] (battle IMP DWARF)) (range 100)))))
"Elapsed time: 11706.437191 msecs"
{[:drawn] 79, [:won "dwarf"] 21}
marston.core> 
```

## License

Copyright © 2014 Greg Hawkins

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
