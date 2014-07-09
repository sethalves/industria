#! /bin/sh
#| -*- scheme -*-
CHIBI_MODULE_PATH="" exec chibi-scheme -A . -s $0 "$@"
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
        (weinholt crypto aes)
        (weinholt crypto arcfour)
        (weinholt crypto blowfish)
        )
(include "test-common.scm")
(display (main-program))
(newline)
