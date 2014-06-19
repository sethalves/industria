#! /bin/sh
#| -*- scheme -*-
exec sash -A . -F .sld $0 "$@"
|#

(import (scheme base)
        (scheme write)
        (weinholt archive tar))
(include "test-common.scm")
(display (main-program))
(newline)
