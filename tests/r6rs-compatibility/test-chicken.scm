#! /bin/sh
#| -*- scheme -*-
exec csi -s $0 "$@"
|#

(use r7rs)
(import-for-syntax r7rs)
(import (scheme base)
        (scheme write))
(include "srfi/60.sld")
(include "weinholt/r6rs-compatibility.sld")
(import (weinholt r6rs-compatibility))
(include "test-common.scm")
(display (main-program))
(newline)
