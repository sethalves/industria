#! /bin/sh
#| -*- scheme -*-
exec foment $0 "$@"
|#

(import (scheme base)
        (scheme write)
        (weinholt r6rs-compatibility))
(include "test-common.scm")
(display (main-program))
(newline)
