#! /bin/sh
#| -*- scheme -*-
exec foment $0 "$@"
|#


(import (scheme base)
        (scheme write)
        (scheme eval)
        (scheme repl)
        (rename (only (srfi 27) random-integer
                      default-random-source random-source-randomize!)
                (random-integer random))
        (srfi 78)
        (prefix (weinholt struct der) der:)
        (weinholt struct pack)
        (only (weinholt struct pack-aux) roundb)
        (weinholt r6rs-compatibility))
(include "test-common.scm")
(display (main-program))
(newline)
