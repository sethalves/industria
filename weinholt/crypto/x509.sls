;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2009 Göran Weinholt <goran@weinholt.se>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
#!r6rs

;; X.509 certificates as per RFC 5280.

;; The decoding routines are implemented manually, but should maybe be
;; automatically generated from the ASN.1 types in the RFC.

;; Here is an example of a parsed and annotated certificate:

;; ((tbsCertificate (version v3)
;;                  (serialNumber 256)
;;                  (signature (algorithm (1 2 840 113549 1 1 5))
;;                             (parameters ()))
;;                  (issuer (((type (2 5 4 6)) (value "US")))
;;                          (((type (2 5 4 11)) (value "gov")))
;;                          (((type (2 5 4 10)) (value "NIST"))))
;;                  (validity (utcTime #[date 0 26 58 9 21 5 1996 0])
;;                            (utcTime #[date 0 26 58 9 21 5 1997 0]))
;;                  (subject (((type (2 5 4 6)) (value "US")))
;;                           (((type (2 5 4 11)) (value "gov")))
;;                           (((type (2 5 4 10)) (value "NIST")))
;;                           (((type (2 5 4 3)) (value "Tim Polk"))))
;;                  (subjectPublicKeyInfo
;;                   (algorithm (algorithm (1 2 840 113549 1 1 1))
;;                              (parameters ()))
;;                   (subjectPublicKey
;;                    #vu8(48 129 137 2 129 129 0 225 106 228 3 48 151 2 60
;;                            244 16 243 181 30 77 127 20 123 246 245 208 120
;;                            233 164 138 240 163 117 236 237 182 86 150 127
;;                            136 153 133 154 242 62 104 119 135 235 158 209
;;                            159 192 180 23 220 171 137 35 164 29 126 22 35
;;                            76 79 168 77 245 49 184 124 170 227 26 73 9 244
;;                            75 38 219 39 103 48 130 18 1 74 233 26 182 193
;;                            12 83 139 108 252 47 122 67 236 51 54 126 50 178
;;                            123 213 170 207 1 20 198 18 236 19 242 45 20 122
;;                            139 33 88 20 19 76 70 163 154 242 22 149 255 35
;;                            2 3 1 0 1)))
;;                  (issuerUniqueID #f)
;;                  (subjectUniqueID #f)
;;                  (extensions
;;                   ((extnID (2 5 29 17)) (critical #f)
;;                    (extnValue
;;                     #vu8(48 54 134 52 104 116 116 112 58 47 47 119 119
;;                             119 46 105 116 108 46 110 105 115 116 46 103
;;                             111 118 47 100 105 118 56 57 51 47 115 116 97
;;                             102 102 47 112 111 108 107 47 105 110 100 101
;;                             120 46 104 116 109 108)))
;;                   ((extnID (2 5 29 18)) (critical #f)
;;                    (extnValue #vu8(48 22 134 20 104 116 116 112 58 47 47 119 119
;;                                       119 46 110 105 115 116 46 103 111 118 47)))
;;                   ((extnID (2 5 29 35)) (critical #f)
;;                    (extnValue #vu8(48 22 128 20 8 104 175 133 51 200 57 74 122
;;                                       248 130 147 142 112 106 74 32 132 44 50)))
;;                   ((extnID (2 5 29 32)) (critical #f)
;;                    (extnValue #vu8(48 14 48 12 6 10 96 134 72 1 101 3 2 1 48 9)))
;;                   ((extnID (2 5 29 15)) (critical #t)
;;                    (extnValue #vu8(3 2 7 128)))))
;;  (signatureAlgorithm (algorithm (1 2 840 113549 1 1 5))
;;                      (parameters ()))
;;  (signatureValue
;;   #vu8(142 142 54 86 120 139 191 161 57 117 23 46 227 16
;;            220 131 43 104 52 82 28 246 108 29 82 94 84 32 16
;;            94 76 169 64 249 75 114 158 130 185 97 220 235 50
;;            165 189 177 177 72 249 155 1 187 235 175 155 131
;;            246 82 140 176 109 124 208 154 57 84 62 109 32 111
;;            205 208 222 190 39 95 32 79 182 171 13 245 183 225
;;            186 180 223 223 61 212 246 237 1 251 110 203 152 89
;;            172 65 251 72 156 31 246 91 70 224 41 226 118 236
;;            196 58 10 252 146 197 192 210 169 201 211 41 82 135
;;            101 51)))


(library (weinholt crypto x509)
  (export certificate<-bytevector
          public-key<-certificate
          decipher-certificate-signature
          verify-certificate-chain

          certificate-tbs-data

          print-certificate)
  (import (rnrs)
          (only (srfi :13 strings) string-join)
          (srfi :19 time)
          (weinholt crypto rsa)
          (weinholt crypto sha-1)
          (prefix (weinholt struct der (1)) der:))

  (define (print . x) (for-each display x) (newline))

;;; The Certificate ASN.1 type from the RFC (parsed by hand).
  (define (RelativeDistinguishedName)
    `(set-of 1 +inf.0 ,(AttributeTypeAndValue)))

  (define (RDNSequence)
    `(sequence-of 0 +inf.0 ,(RelativeDistinguishedName)))

  (define (AttributeType) 'object-identifier)

  (define (AttributeValue) 'ANY)

  (define (AttributeTypeAndValue)
    `(sequence (type ,(AttributeType))
               (value ,(AttributeValue))))

  (define (Time)
    `(choice (utcTime utc-time)
             (generalTime generalized-time)))

  (define (Validity)
    `(sequence (notBefore ,(Time))
               (notAfter ,(Time))))

  (define (AlgorithmIdentifier)
    `(sequence (algorithm object-identifier)
               (parameters ANY (default #f))))

  (define (Version)
    `(integer ((v1 . 0) (v2 . 1) (v3 . 2))))

  (define (SubjectPublicKeyInfo)
    `(sequence (algorithm ,(AlgorithmIdentifier))
               (subjectPublicKey bit-string)))

  (define (Extensions)
    `(sequence-of 1 +inf.0 ,(Extension)))

  (define (Extension)
    `(sequence (extnID object-identifier)
               (critical boolean (default #f))
               (extnValue octet-string)))

  (define (UniqueIdentifier)
    'bit-string)

  (define (Name)
    (RDNSequence))

  (define (Certificate)
    `(sequence (tbsCertificate ,(TBSCertificate))
               (signatureAlgorithm ,(AlgorithmIdentifier))
               (signatureValue bit-string)))

  (define (TBSCertificate)
    `(sequence (version (explicit context 0 ,(Version)) (default v1))
               (serialNumber integer)
               (signature ,(AlgorithmIdentifier))
               (issuer ,(Name))
               (validity ,(Validity))
               (subject ,(Name))
               (subjectPublicKeyInfo ,(SubjectPublicKeyInfo))
               (issuerUniqueID (implicit context 1 ,(UniqueIdentifier)) (default #vu8()))
               (subjectUniqueID (implicit context 2 ,(UniqueIdentifier)) (default #vu8()))
               (extensions (explicit context 3 ,(Extensions)) (default '()))))

;;;

  (define equal-dn? equal?) ;compare distinguished names. FIXME: sec 7.1

  (define (symbol<-oid oid)
    (define oids
      '(((0 9 2342 19200300 100 1 25) . domainComponent)
        ((1 2 840 10040 4 1) . dsa)
        ((1 2 840 10040 4 3) . dsaWithSha1)
        ((1 2 840 113549 1 1 1) . rsaEncryption)
        ((1 2 840 113549 1 1 4) . md5withRSAEncryption)
        ((1 2 840 113549 1 1 5) . sha1WithRSAEncryption)
        ((1 2 840 113549 1 9 1) . emailAddress)

        ((1 3 6 1 5 5 7 1 1) . authorityInfoAccess)
        ((1 3 14 3 2 26) . sha1)
        ((2 5 4 3) . commonName)
        ((2 5 4 6) . countryName)
        ((2 5 4 7) . localityName)
        ((2 5 4 8) . stateOrProvinceName)
        ((2 5 4 9) . streetAddress)
        ((2 5 4 10) . organizationName)
        ((2 5 4 11) . organizationalUnitName)
        ((2 5 4 16) . postalAddress)
        ((2 5 4 17) . postalCode)

        ((2 5 29 14) . subjectKeyIdentifier)
        ((2 5 29 15) . keyUsage)
        ((2 5 29 17) . subjectAltName)
        ((2 5 29 18) . issuerAltName)
        ((2 5 29 19) . basicConstraints)
        ((2 5 29 31) . cRLDistributionPoints)
        ((2 5 29 32) . certificatePolicies)
        ((2 5 29 35) . authorityKeyIdentifier)))
    (cond ((assoc oid oids) => cdr)
          (else (string->symbol (string-append
                                 "oid-"
                                 (string-join (map number->string oid) "."))))))

  (define-record-type certificate
    (fields tbs-data                    ;Bytevector with the tbs-certificate
            tbs-certificate             ;"To Be Signed"
            signature-algorithm
            signature-value))

  (define-record-type tbs-certificate
    (fields version
            serial-number
            signature-algorithm
            issuer
            validity
            subject
            subject-public-key-info
            issuer-unique-id            ;unused
            subject-unique-id           ;unused
            extensions))

  (define certificate<-bytevector
    (case-lambda
      ((bv)
       (certificate<-bytevector bv 0 (bytevector-length bv)))
      ((bv start end)
       (let* ((parse-tree (der:decode bv start end))
              (cert-data (der:translate parse-tree (Certificate) asn1translate)))
         ;; (write (der:translate parse-tree (Certificate)
         ;;                       (lambda (x y z) (list x z))))
         (let* ((tbs (car (der:data-value parse-tree)))
                (tbs-data (make-bytevector (der:data-length tbs))))
           ;; Copy the To Be Signed certificate, so the signature can
           ;; be verified.
           (bytevector-copy! bv (der:data-start-index tbs)
                             tbs-data 0
                             (bytevector-length tbs-data))
           (apply make-certificate tbs-data cert-data))))))

  (define (translate-algorithm value)
    (list (symbol<-oid (car value))   ;algorithm
          #f #;(cadr value)))         ;parameters

  (define (asn1translate name type value)
    ;; This uses the sequence field names to interpret the ASN.1
    ;; certificate data.
    (case name
      ((signatureValue)
       (bytevector-uint-ref value 0 (endianness big) (bytevector-length value)))
      ((issuer subject)
       (map (lambda (e)
              ;; FIXME: This is actually defined by the attribute type
              ;; in some maze-like way.
              (cons (symbol<-oid (car e))
                    (der:translate (cadr e) '(choice (teletexString teletex-string)
                                                     (printableString printable-string)
                                                     (universalString universal-string)
                                                     (utf8String utf8-string)
                                                     (bmpString bmp-string)
                                                     (IA5String ia5-string)
                                                     (T61String t61-string)))))
            (map car value)))
      ((signature signatureAlgorithm) (translate-algorithm value))
      ((tbsCertificate) (apply make-tbs-certificate value))
      ((subjectPublicKeyInfo)
       (let ((algo (translate-algorithm (car value))))
         (case (car algo)
           ((rsaEncryption) (rsa-public-key<-bytevector (cadr value)))
           ;; TODO: dsa
           (else
            (error 'certificate-bytevector
                   "unimplemented signature algorithm" (car algo))))))
      (else value)))

  (define (public-key<-certificate cert)
    (tbs-certificate-subject-public-key-info (certificate-tbs-certificate cert)))

;;; Certificate signatures etc

  (define (decipher-certificate-signature signed-cert signer-cert)
    (unless (equal-dn? (tbs-certificate-subject (certificate-tbs-certificate signer-cert))
                       (tbs-certificate-issuer  (certificate-tbs-certificate signed-cert)))
      (error 'decipher-certificate-signature
             "issuer on signed certificate does not match subject on signer"
             (tbs-certificate-subject (certificate-tbs-certificate signer-cert))
             (tbs-certificate-issuer  (certificate-tbs-certificate signed-cert))))
    (let ((key (public-key<-certificate signer-cert)))
      (cond ((rsa-public-key? key)
             (let ((digest (rsa-pkcs1-decrypt-digest
                            (certificate-signature-value signed-cert) key)))
               (list (translate-algorithm (car digest))
                     (cadr digest))))
            (else
             (error 'decipher-certificate-signature
                    "unimplemented crypto?")))))

  (define (verify-certificate-chain l)
    ;; TODO: check subject/issuer, all that jazz. CRLs. Time fields.
    ;; Same signature algorithms. That critical CA extension. List of
    ;; trusted root certificates. Maximum depth.
    (let ((cert (certificate-tbs-certificate (car l))))
      (cond ((and (equal-dn? (tbs-certificate-subject cert)
                             (tbs-certificate-issuer cert))
                  (null? (cdr l)))
             (let* ((signature (decipher-certificate-signature (car l) (car l)))
                    (algo (car signature)))
               (case (car algo)
                 ((sha1)
                  (if (bytevector=? (sha-1->bytevector
                                     (sha-1 (certificate-tbs-data (car l))))
                                    (cadr signature))
                      'self-signed
                      'bad-signature))
                 (else
                  'unimplemented-signing-algorithm))))

            ((null? (cdr l))
             ;; TODO: parse e.g. the CAfile /etc/ssl/certs/ca-certificates.crt
             (print "Would need to find root certificate for:")
             (write (tbs-certificate-issuer cert))
             (newline)
             'no-root-certificate)
            ((equal-dn? (tbs-certificate-issuer cert)
                        (tbs-certificate-subject (certificate-tbs-certificate (cadr l))))
             (let ((signature (decipher-certificate-signature (car l) (cadr l))))
               ;; TODO: verify the digest. this only proves that there
               ;; is _a_ signature on the certificate. :)
               (print "DIGEST: " signature)
               (verify-certificate-chain (cdr l))))
            (else
             'bad-certificate-chain))))

  (define (print-certificate c)
    (let ((cert (certificate-tbs-certificate c)))
      (print "---------------")
      (print "X.509 certificate version " (tbs-certificate-version cert))
      (print "- Serial number: " (tbs-certificate-serial-number cert))
      (print "- Signature algorithm: " (tbs-certificate-signature-algorithm cert))
      (print "- Subject Public Key: ")
      (print (public-key<-certificate c))

      (display "- Issuer: ") (write (tbs-certificate-issuer cert)) (newline)
      (let ((v (tbs-certificate-validity cert)))
        (print "- Not valid before: " (date->string (car v)))
        (print "- Not valid after: " (date->string (cadr v))))
      (display "- Subject: ") (write (tbs-certificate-subject cert)) (newline)

      (print "- Extensions: ")
      (for-each (lambda (x) (write x) (newline))
                (tbs-certificate-extensions cert))

      (print "Signature algorithm: " (certificate-signature-algorithm c))

      #;(print "Signature: " (bit-string-value (certificate-signature-value c)))

      ))



  )
