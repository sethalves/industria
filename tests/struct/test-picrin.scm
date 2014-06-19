#! /bin/sh
#| -*- scheme -*-
exec picrin $0 "$@"
|#

(import (scheme base)
        (scheme write)
        (srfi 78)
        (prefix (weinholt struct der) der:))
(include "test-common.scm")
(display (main-program))
(newline)
