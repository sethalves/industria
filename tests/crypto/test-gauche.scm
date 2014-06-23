#! /bin/sh
#| -*- scheme -*-
exec gosh \
-e '(append! *load-suffixes* (list ".sld"))' \
-e '(append! *load-path* (list "."))' \
-ftest -r7 $0 "$@"
|#

(import (scheme base)
        (scheme char)
        (scheme write)
        (srfi 69)
        (srfi 78)
        (weinholt bytevectors)
        (weinholt r6rs-compatibility)
        (weinholt crypto entropy)
        (weinholt crypto md5)
        (weinholt crypto sha-1)
        (weinholt crypto uuid)
        (weinholt crypto crc)
        )
(include "test-common.scm")
(display (main-program))
(newline)
