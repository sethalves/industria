#! /bin/sh
#| -*- scheme -*-
exec foment $0 "$@"
|#

(import (scheme base)
        (scheme write)
        (weinholt bytevectors))
(include "test-common.scm")
(display (main-program))
(newline)
