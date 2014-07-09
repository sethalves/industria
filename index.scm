(repository
  (url "http://snow2.s3-website-us-east-1.amazonaws.com/industria/index.scm")
  (name "r7rs industria")
  (sibling
    (name "Snow Repository")
    (url "http://snow-repository.s3-website-us-east-1.amazonaws.com/index.scm")
    (trust 1.0))
  (package
    (name ())
    (version "1.0")
    (url "http://snow2.s3-website-us-east-1.amazonaws.com/industria/crypto.tgz")
    (size 88064)
    (checksum (md5 "3a0c8c2d962a7f3651cec371c4dcc808"))
    (library
      (name (weinholt crypto entropy))
      (path "weinholt/crypto/entropy.sld")
      (version "1.0")
      (homepage "https://weinholt.se/industria/")
      (maintainers "Seth Alves <seth@hungry.com>")
      (authors "Göran Weinholt <goran@weinholt.se>")
      (description "Entropic helpers")
      (license mit)
      (depends (srfi 13) (srfi 26) (srfi 27)))
    (library
      (name (weinholt crypto md5))
      (path "weinholt/crypto/md5.sld")
      (version "1.0")
      (homepage "https://weinholt.se/industria/")
      (maintainers "Seth Alves <seth@hungry.com>")
      (authors "Göran Weinholt <goran@weinholt.se>")
      (description "The MD5 Message-Digest Algorithm. RFC 1321")
      (license mit)
      (depends
        (srfi 1)
        (srfi 60)
        (weinholt bytevectors)
        (weinholt r6rs-compatibility)))
    (library
      (name (weinholt crypto sha-1))
      (path "weinholt/crypto/sha-1.sld")
      (version "1.0")
      (homepage "https://weinholt.se/industria/")
      (maintainers "Seth Alves <seth@hungry.com>")
      (authors "Göran Weinholt <goran@weinholt.se>")
      (description "Byte-oriented SHA-1 from FIPS 180-3 and RFC 3174")
      (license mit)
      (depends (srfi 60) (weinholt bytevectors) (weinholt r6rs-compatibility)))
    (library
      (name (weinholt crypto uuid))
      (path "weinholt/crypto/uuid.sld")
      (version "1.0")
      (homepage "https://weinholt.se/industria/")
      (maintainers "Seth Alves <seth@hungry.com>")
      (authors "Göran Weinholt <goran@weinholt.se>")
      (description "Universally Unique IDentifiers (RFC 4122)")
      (license mit)
      (depends
        (srfi 19)
        (srfi 60)
        (weinholt r6rs-compatibility)
        (weinholt bytevectors)
        (weinholt crypto entropy)
        (weinholt crypto md5)
        (weinholt crypto sha-1)
        (weinholt struct pack)))
    (library
      (name (weinholt crypto crc))
      (path "weinholt/crypto/crc.sld")
      (version "1.0")
      (homepage "https://weinholt.se/industria/")
      (maintainers "Seth Alves <seth@hungry.com>")
      (authors "Göran Weinholt <goran@weinholt.se>")
      (description "Procedures that calculate Cyclic Redundancy Codes")
      (license mit)
      (depends
        (srfi 1)
        (srfi 60)
        (weinholt bytevectors)
        (weinholt r6rs-compatibility)))
    (library
      (name (weinholt crypto aes))
      (path "weinholt/crypto/aes.sld")
      (version "1.0")
      (homepage "https://weinholt.se/industria/")
      (maintainers "Seth Alves <seth@hungry.com>")
      (authors "Göran Weinholt <goran@weinholt.se>")
      (description "Rijndael cipher as parameterized by AES")
      (license mit)
      (depends
        (weinholt crypto aes private)
        (srfi 1)
        (srfi 60)
        (weinholt r6rs-compatibility)))
    (library
      (name (weinholt crypto aes private))
      (path "weinholt/crypto/aes/private.sld")
      (version "1.0")
      (homepage "https://weinholt.se/industria/")
      (maintainers "Seth Alves <seth@hungry.com>")
      (authors
        "Göran Weinholt <goran@weinholt.se>"
        "Erik De Win"
        "Antoon Bosselaers"
        "Servaas Vanderberghe"
        "Peter De Gersem"
        "Joos Vandewalle")
      (description "GF(2⁸)")
      (license mit)
      (depends (srfi 60) (weinholt r6rs-compatibility)))
    (library
      (name (weinholt crypto arcfour))
      (path "weinholt/crypto/arcfour.sld")
      (version "1.0")
      (homepage "https://weinholt.se/industria/")
      (maintainers "Seth Alves <seth@hungry.com>")
      (authors "Göran Weinholt <goran@weinholt.se>")
      (description "ARCFOUR encryption")
      (license mit)
      (depends
        (srfi 1)
        (srfi 60)
        (weinholt r6rs-compatibility)
        (weinholt bytevectors))))
  (package
    (name ())
    (version "1.0")
    (url "http://snow2.s3-website-us-east-1.amazonaws.com/industria/archive.tgz")
    (size 12288)
    (checksum (md5 "aeda5146e828036e7bb3c7daccff314e"))
    (library
      (name (weinholt archive tar))
      (path "weinholt/archive/tar.sld")
      (version "1.0")
      (homepage "https://weinholt.se/industria/")
      (maintainers "Seth Alves <seth@hungry.com>")
      (authors "Göran Weinholt <goran@weinholt.se>")
      (description "Procedures that read Tape ARchives")
      (license mit)
      (depends (srfi 13) (srfi 19) (srfi 60) (weinholt r6rs-compatibility))))
  (package
    (name ())
    (version "1.0")
    (url "http://snow2.s3-website-us-east-1.amazonaws.com/industria/bytevectors.tgz")
    (size 9216)
    (checksum (md5 "3f448f158835c0ba762a5743bf70d5bc"))
    (library
      (name (weinholt bytevectors))
      (path "weinholt/bytevectors.sld")
      (version "1.0")
      (homepage "https://weinholt.se/industria/")
      (maintainers "Seth Alves <seth@hungry.com>")
      (authors "Göran Weinholt <goran@weinholt.se>")
      (description "Bytevector utilities")
      (license mit)
      (depends (srfi 60) (weinholt r6rs-compatibility))))
  (package
    (name ())
    (version "1.0")
    (url "http://snow2.s3-website-us-east-1.amazonaws.com/industria/r6rs-compatibility.tgz")
    (size 25600)
    (checksum (md5 "acd2e7167b82bf614e35a9e5fe87a669"))
    (library
      (name (weinholt r6rs-compatibility))
      (path "weinholt/r6rs-compatibility.sld")
      (version "1.0")
      (homepage "https://weinholt.se/industria/")
      (maintainers "Seth Alves <seth@hungry.com>")
      (authors "Seth Alves <seth@hungry.com>")
      (description "Bytevector utilities")
      (license mit)
      (depends (srfi 60))))
  (package
    (name ())
    (version "1.0")
    (url "http://snow2.s3-website-us-east-1.amazonaws.com/industria/struct.tgz")
    (size 47616)
    (checksum (md5 "910939871a4430ec9715f9cb9f6e5ffa"))
    (library
      (name (weinholt struct der))
      (path "weinholt/struct/der.sld")
      (version "1.0")
      (homepage "https://weinholt.se/industria/")
      (maintainers "Seth Alves <seth@hungry.com>")
      (authors "Göran Weinholt <goran@weinholt.se>")
      (description "Distinguished Encoding Rules (DER)")
      (license mit)
      (depends
        (srfi 1)
        (srfi 19)
        (srfi 26)
        (srfi 60)
        (weinholt r6rs-compatibility)
        (weinholt bytevectors)))
    (library
      (name (weinholt struct pack))
      (path "weinholt/struct/pack.sld")
      (version "1.0")
      (homepage "https://weinholt.se/industria/")
      (maintainers "Seth Alves <seth@hungry.com>")
      (authors "Göran Weinholt <goran@weinholt.se>")
      (description
        "Syntax for packing and unpacking C structs using bytevectors")
      (license mit)
      (depends (weinholt r6rs-compatibility) (weinholt struct pack-aux)))
    (library
      (name (weinholt struct pack-aux))
      (path "weinholt/struct/pack-aux.sld")
      (version "1.0")
      (homepage "https://weinholt.se/industria/")
      (maintainers "Seth Alves <seth@hungry.com>")
      (authors "Göran Weinholt <goran@weinholt.se>")
      (description
        "Auxiliary library for (weinholt struct). Please don't use this library directly.")
      (license mit)
      (depends (srfi 60)))))
