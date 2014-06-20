#! /bin/sh
#| -*- scheme -*-
exec csi -s $0 "$@"
|#

(use r7rs)
(import-for-syntax r7rs)
(import (scheme base)
        (scheme char)
        (scheme write))
(include "srfi/26.sld")
(include "srfi/42.sld")
(include "srfi/60.sld")
(include "srfi/78.sld")
(import (srfi 69)
        (srfi 78))
(include "weinholt/r6rs-compatibility.sld")
(include "weinholt/bytevectors.sld")
(include "weinholt/struct/pack-aux.sld")
(include "weinholt/struct/pack.sld")
(include "weinholt/crypto/entropy.sld")
(include "weinholt/crypto/md5.sld")
(include "weinholt/crypto/sha-1.sld")
(include "weinholt/crypto/uuid.sld")
(import (weinholt bytevectors)
        (weinholt r6rs-compatibility)
        (weinholt crypto entropy)
        (weinholt crypto md5)
        (weinholt crypto sha-1)
        (weinholt crypto uuid)
        )
(include "test-common.scm")
(display (main-program))
(newline)
