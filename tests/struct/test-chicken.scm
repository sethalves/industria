#! /bin/sh
#| -*- scheme -*-
exec csi -s $0 "$@"
|#

(use r7rs)
(import-for-syntax r7rs)
(import (scheme base)
        (scheme write)
        (scheme eval)
        (scheme repl))
(include "srfi/26.sld")
;; (include "srfi/27.sld")
(include "srfi/42.sld")
(include "srfi/60.sld")
(include "srfi/78.sld")
(include "weinholt/r6rs-compatibility.sld")
(include "weinholt/bytevectors.sld")
(include "weinholt/struct/der.sld")
(include "weinholt/struct/pack-aux.sld")
(include "weinholt/struct/pack.sld")
(import (rename (only (srfi 27) random-integer
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
