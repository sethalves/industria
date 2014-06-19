#! /bin/sh
#| -*- scheme -*-
exec foment $0 "$@"
|#

(import (scheme base)
        (scheme write)
        (weinholt archive tar))
(include "test-common.scm")
(display (main-program))
(newline)
