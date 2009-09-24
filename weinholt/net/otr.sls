#!r6rs
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

;; Off-the-Record Messaging Protocol version 2

;; http://www.cypherpunks.ca/otr/Protocol-v2-3.1.0.html

;; TODO: S-M-P.
;; TODO: fix the state transitions (most are probably missing)
;; TODO: better interface for the outgoing messages etc
;; TODO: Petite Chez has trouble with the AKE (bad sigs).
;; TODO: Ypsilon is very slow on D-H key generation.
;; TODO: should it be possible to establish a session with someone
;; using our own DSA key?
;; TODO: reveal old MAC keys. Forget old D-H keys.
;; TODO: finishing sessions.

(library (weinholt net otr (0 0 20090924))
  (export otr-message?
          otr-update!
          otr-send-encrypted!
          make-otr-state
          otr-state-their-dsa-key
          otr-state-our-dsa-key
          otr-state-secure-session-id
          otr-hash-public-key
          otr-format-session-id)
  (import (rnrs)
          (only (srfi :1 lists) iota)
          (only (srfi :13 strings) string-contains string-join
                string-index string-index-right string-pad)
          (srfi :26 cut)
          (srfi :27 random-bits)
          (srfi :39 parameters)
          (weinholt bytevectors)
          (weinholt crypto aes)
          (weinholt crypto dsa)
          (weinholt crypto entropy)
          (weinholt crypto math)
          (weinholt crypto sha-1)
          (weinholt crypto sha-2)
          (weinholt struct pack)
          (weinholt text base64)
          (weinholt text strings))

;;; Helpers

  (define (print . x)
    ;; (for-each display x) (newline)
    (values))
  (define (hex x)
    (string-append "#x" (number->string x 16)))

  (define (make-secret g n bits tries) ;also in net/irc/fish, should be in crypto/dh maybe?
    (unless (< tries 1000)
      (error 'make-secret "unable to make a secret"))
    (let* ((y (bytevector->uint (make-random-bytevector (div (+ bits 7) 8))))
           (Y (expt-mod g y n)))
      ;; See RFC 2631. Probably not clever enough.
      (if (and (<= 2 Y (- n 1))
               (= 1 (expt-mod Y (/ (- n 1) 2) n)))
          (values y Y)
          (make-secret g n bits (+ tries 1)))))

  (define (get-bytevector p n)
    (let ((ret (get-bytevector-n p n)))
      (unless (eqv? (bytevector-length ret) n)
        (error 'get-bytevector "short read" n (bytevector-length ret)))
      ret))

  (define (mpi->uint bv)
    (bytevector-uint-ref bv 4 (endianness big) (bytevector-u32-ref bv 0 (endianness big))))

  (define (uint->mpi int)
    (let* ((len (div (bitwise-and -8 (+ 7 (bitwise-length int))) 8))
           (ret (make-bytevector (+ 4 len))))
      (bytevector-u32-set! ret 0 len (endianness big))
      (bytevector-uint-set! ret 4 int (endianness big) len)
      ret))

  (define (dsa-public-key->bytevector key)
    (bytevector-append
     (pack "!S" key-type-dsa)
     (uint->mpi (dsa-public-key-p key))
     (uint->mpi (dsa-public-key-q key))
     (uint->mpi (dsa-public-key-g key))
     (uint->mpi (dsa-public-key-y key))))

  (define (get-public-dsa-key port)
    ;; Read a public DSA key from the X in reveal-signature or
    ;; signature messages.
    (let ((p (bytevector->uint (get-bytevector port (get-unpack port "!L"))))
          (q (bytevector->uint (get-bytevector port (get-unpack port "!L"))))
          (g (bytevector->uint (get-bytevector port (get-unpack port "!L"))))
          (y (bytevector->uint (get-bytevector port (get-unpack port "!L")))))
      (make-dsa-public-key p q g y)))

  (define (get-public-key p)
    ;; Read the DSA public key from pub[A] in: X[A] = pub[A],
    ;; keyid[A], sig[A](M[A]).
    (let ((keytype (get-unpack p "!S")))
      (if (= keytype key-type-dsa)
          (get-public-dsa-key p)
          (error 'auth-state-awaiting-reveal-sig "Bad keytype"))))

  (define (sign-public-key key secret keyid Y X)
    ;; Signs the public part of the private key and returns pub[B],
    ;; keyid[B], sig[B](M[B]).
    (let*-values (((pub) (dsa-public-key->bytevector
                          (dsa-private->public key)))
                  ((r s) (dsa-create-signature
                          (sha-256->bytevector
                           (hmac-sha-256 secret
                                         (uint->mpi Y) (uint->mpi X)
                                         pub
                                         (pack "!L" keyid)))
                          key)))
      (bytevector-append pub
                         (pack "!L" keyid)
                         (bytevector-pad (uint->bytevector r) q-len 0)
                         (bytevector-pad (uint->bytevector s) q-len 0))))

  (define (verify-public-key key secret keyid X Y r s)
    (dsa-verify-signature (sha-256->bytevector
                           (hmac-sha-256 secret
                                         (uint->mpi X) (uint->mpi Y)
                                         (dsa-public-key->bytevector key)
                                         (pack "!L" keyid)))
                          key r s))

  (define (bytevector-pad bv outlen padding)
    (let ((inlen (bytevector-length bv))
          (ret (make-bytevector outlen padding)))
      (bytevector-copy! bv (if (< outlen inlen) (- inlen outlen) 0)
                        ret (max 0 (- outlen inlen))
                        (min outlen inlen))
      ret))

  (define (MAC secret . data)
    (subbytevector (sha-256->bytevector
                    (apply hmac-sha-256 secret data))
                   0 160/8))


  (define (h1 b secbytes)
    (let ((s (make-sha-1)))
      (sha-1-update! s (make-bytevector 1 b))
      (sha-1-update! s secbytes)
      (sha-1-finish! s)
      (let ((ret (sha-1->bytevector s)))
        (sha-1-clear! s)
        ret)))

  (define (h2 b secbytes)
    (let ((s (make-sha-256)))
      (sha-256-update! s (make-bytevector 1 b))
      (sha-256-update! s secbytes)
      (sha-256-finish! s)
      (let ((ret (sha-256->bytevector s)))
        (sha-256-clear! s)
        ret)))

  (define (make-keys X y)
    (let* ((secbytes (uint->mpi (expt-mod X y n)))
           (ssid (bytevector-u64-ref (h2 0 secbytes) 0
                                     (endianness big)))
           (key-material (h2 1 secbytes))
           (aeskey (subbytevector key-material 0 16))
           (aeskey* (subbytevector key-material 16 32))
           (m1 (h2 2 secbytes))
           (m2 (h2 3 secbytes))
           (m1* (h2 4 secbytes))
           (m2* (h2 5 secbytes)))
      ;; XXX: can't actually erase the result of expt-mod and so on...
      (bytevector-fill! secbytes 0)
      (bytevector-fill! key-material 0)
      (let ((c (expand-aes-key aeskey))
            (c* (expand-aes-key aeskey*)))
        (bytevector-fill! aeskey 0)
        (bytevector-fill! aeskey* 0)
        (values ssid c c* m1 m2 m1* m2*))))

;;;

  (define otr-version #x0002)

  (define whitespace-prefix
    (string-append "\x20;\x09;\x20;\x20;\x09;\x09;\x09;\x09;"
                   "\x20;\x09;\x20;\x09;\x20;\x09;\x20;\x20;"))

  (define v2-tag "\x20;\x20;\x09;\x09;\x20;\x20;\x09;\x20;")

  ;; Diffie-Hellman modulus and generator
  (define n
    (string->number
     "FFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD1\
      29024E088A67CC74020BBEA63B139B22514A08798E3404DD\
      EF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245\
      E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7ED\
      EE386BFB5A899FA5AE9F24117C4B1FE649286651ECE45B3D\
      C2007CB8A163BF0598DA48361C55D39A69163FA8FD24CF5F\
      83655D23DCA3AD961C62F356208552BB9ED529077096966D\
      670C354E4ABC9804F1746C08CA237327FFFFFFFFFFFFFFFF"
     16))
  (define g 2)

  (define dh-length 640)              ;length for the private D-H keys

  (define q-len 160/8)                  ;1024-bit DSA keys, 160-bit q

  ;; Message types
  (define msg-diffie-hellman-commit #x02)
  (define msg-diffie-hellman-key #x0a)
  (define msg-reveal-signature #x11)
  (define msg-signature #x12)
  (define msg-data #x03)

  ;; Data message flags
  (define flag-ignore-unreadable #b00000001)

  ;; Key types
  (define key-type-dsa #x0000)

  ;; TLV types
  (define tlv-null #x0000)
  (define tlv-finish #x0001)

  (define-record-type otr-state
    (fields (immutable our-dsa-key)
            (mutable their-dsa-key)
            (mutable secure-session-id)
            ;; Maximum segment size
            (mutable mss)
            ;; De-fragmentation buffer
            (mutable frag-n)
            (mutable frag-k)
            (mutable frags)
            ;; Result queue
            (mutable queue)
            ;; Continuations
            (mutable k)
            (mutable c)
            ;; Diffie-Hellman keys
            (mutable our-keys)
            (mutable their-pubkeys)
            (mutable our-latest-acked)
            (mutable our-pubkeys)
            ;; Last used top half of the AES counter
            (mutable their-ctr)
            (mutable our-ctr))
    (protocol
     (lambda (p)
       (lambda (dsa-key mss)
         (assert (dsa-private-key? dsa-key))
         ;; It is not documented in the specification, but the DSA
         ;; keys have to have a 160-bit q-parameter, or the reference
         ;; implementation will reject the signatures. A 1024-bit DSA
         ;; key will probably be OK.
         (assert (= 160/8 (bytevector-length
                           (uint->bytevector
                            (dsa-private-key-q dsa-key)))))
         (p dsa-key #f 0 mss
            0 0 '()
            '()
            plaintext-state #f
            '() '() 0 '()
            0 0)))))

  ;; (define-enumeration policy
  ;;   (allow-v1 allow-v2 require-encryption send-whitespace-tag
  ;;             whitespace-start-ake error-start-ake)
  ;;   otr-policy)

  (define (set-established! state ssid our-keyid y Y their-keyid X their-dsa-key)
    (otr-state-our-keys-set! state (list (cons our-keyid y)))
    (otr-state-our-pubkeys-set! state (list (cons our-keyid Y)))
    (otr-state-their-pubkeys-set! state (list (cons their-keyid X)))
    (otr-state-their-dsa-key-set! state their-dsa-key)
    (otr-state-secure-session-id-set! state ssid)
    (otr-state-their-ctr-set! state 0)
    (otr-state-our-ctr-set! state 0)
    (otr-state-our-latest-acked-set! state our-keyid))

  (define (forget-session! state)
    (otr-state-our-keys-set! state '())
    (otr-state-our-pubkeys-set! state '())
    (otr-state-their-pubkeys-set! state '())
    (otr-state-their-dsa-key-set! state #f)
    (otr-state-secure-session-id-set! state 0)
    (otr-state-their-ctr-set! state 0)
    (otr-state-our-ctr-set! state 0)
    (otr-state-our-latest-acked-set! state 0))

  (define (otr-message? msg)
    (cond ((string-contains msg "?OTR"))
          ((string-contains msg whitespace-prefix) =>
           ;; Tagged plaintext
           (lambda (i)
             ;; They offer OTRv2?
             (string-contains msg v2-tag (+ i (string-length whitespace-prefix)))))
          (else #f)))

  (define (fragment outmsg mss)
    ;; Splits an outgoing message into pieces that fit in the maximum
    ;; message size
    (if (< (string-length outmsg) mss)
        (list outmsg)
        (let ((mss (- mss (string-length "?OTR,12345,12345,,"))))
          (let lp ((pieces '())
                   (outmsg outmsg))
            (if (<= (string-length outmsg) mss)
                (let* ((pieces (cons outmsg pieces))
                       (total (length pieces)))
                  (map (lambda (p i)
                         (string-append "?OTR," (number->string i)
                                        "," (number->string total)
                                        "," p ","))
                       (reverse pieces) (iota total 1)))
                (lp (cons (substring outmsg 0 mss) pieces)
                    (substring outmsg mss (string-length outmsg))))))))

  (define (otr-hash-public-key pubkey)
    ;; Returns the SHA-1 hash of a key, formatted for the user.
    (let ((pub (dsa-public-key->bytevector pubkey))
          (m (make-sha-1)))
      (sha-1-update! m pub
                     (if (zero? (unpack "!S" pub))
                         2
                         0)
                     (bytevector-length pub))
      (sha-1-finish! m)
      (string-upcase
       (string-join (map (lambda (i) (string-pad (number->string i 16) 8 #\0))
                         (bytevector->uint-list (sha-1->bytevector m)
                                                (endianness big) 4))
                    " "))))

  (define (otr-format-session-id id)
    ;; Formats a secure session ID for the user.
    (string-upcase
     (string-append (string-pad (number->string (bitwise-bit-field id 32 64) 16) 8 #\0)
                    " "
                    (string-pad (number->string (bitwise-bit-field id 0 32) 16) 8 #\0))))

;;; Protocol state machine building blocks

  (define *state* (make-parameter 'no-state))

  (define (send msg)
    (assert (bytevector? msg))
    (for-each (cut queue-data 'outgoing <>)
              (fragment (string-append "?OTR:" (base64-encode msg) ".")
                        (otr-state-mss (*state*)))))

  ;; Send an error message to the correspondent. It will probably be
  ;; shown verbatim, or perhaps it will be translated if it matches
  ;; one of the messages libotr uses.
  (define (send-error msg)
    (assert (string? msg))
    (queue-data 'outgoing (string-append "?OTR Error: " msg)))

  (define (queue-data type data)
    (otr-state-queue-set!
     (*state*)
     (append (otr-state-queue (*state*))
             (list (cons type data)))))

  (define (empty-queue! state)
    (let ((queue (otr-state-queue state)))
      (otr-state-queue-set! state '())
      queue))

  (define (recv)
    ;; Return and wait for the next incoming message
    (call/cc
      (lambda (k)
        (otr-state-k-set! (*state*) k)
        ((otr-state-c (*state*))))))

  (define (return state p)
    (parameterize ((*state* state))
      (call/cc
        (lambda (c)
          (otr-state-c-set! (*state*) c)
          (guard (con
                  (else
                   (when (message-condition? con)
                     (print "Error: " (condition-message con)))
                   ;; TODO: what whould be appropriate here?
                   (queue-data 'local-error con)))
            ((otr-state-k (*state*)) p))))))

;;; Everything below here deals with decoding and encoding messages

  (define (tlv-decode bv)
    ;; Decodes all tlvs in the bytevector
    (let ((p (open-bytevector-input-port bv)))
      (let lp ((ret '()))
        (if (port-eof? p)
            (reverse ret)
            (let-values (((type len) (get-unpack p "!SS")))
              (lp (cons (cons type (get-bytevector p len))
                        ret)))))))

  (define (tlv-encode type bv)
    ;; Encode one tlv
    (bytevector-append (pack "!SS" type (bytevector-length bv)) bv))

  ;; "Bob" starts the Authenticated Key Exchange.
  (define (start-ake _)
    (let-values (((x X) (make-secret g n dh-length 100))
                 ((r) (make-random-bytevector 128/8)))
      (let* ((Xbv (uint->mpi X))
             (X-hash (sha-256->bytevector (sha-256 Xbv))))
        ;; Encrypt our public D-H key
        (aes-ctr! Xbv 0 Xbv 0 (bytevector-length Xbv) (expand-aes-key r) 0)
        ;; Send the public D-H key X encrypted with the key r and the
        ;; hash of the unencrypted X.
        (send (bytevector-append (pack "!SC" otr-version msg-diffie-hellman-commit)
                                 (pack "!L" (bytevector-length Xbv)) Xbv
                                 (pack "!L" (bytevector-length X-hash)) X-hash))
        (auth-state-awaiting-dhkey (recv) Xbv X-hash x X r))))

  ;; "Bob" gets Alice's public D-H key.
  (define (auth-state-awaiting-dhkey p Xbv X-hash x X r)
    (let ((type (get-u8 p)))
      (cond ((= type msg-diffie-hellman-key)
             (let ((Y (bytevector->uint (get-bytevector p (get-unpack p "!L")))))
               (unless (and (<= 2 Y (- n 2)) (not (= X Y)))
                 (error 'auth-state-awaiting-dhkey "Received bad Y" Y))
               (let-values (((ssid c c* m1 m2 m1* m2*) (make-keys Y x)))
                 (let* ((keyid-bob 1)
                        (X-bob (sign-public-key (otr-state-our-dsa-key (*state*))
                                                m1 keyid-bob X Y)))
                   ;; Encrypt our public DSA key
                   (aes-ctr! X-bob 0 X-bob 0 (bytevector-length X-bob) c 0)
                   (clear-aes-schedule! c)
                   (send (bytevector-append
                          (pack "!SC" otr-version msg-reveal-signature)
                          (pack "!L" (bytevector-length r)) r
                          (pack "!L" (bytevector-length X-bob)) X-bob
                          (MAC m2
                               (pack "!L" (bytevector-length X-bob))
                               X-bob)))
                   (auth-state-awaiting-signature (recv) x X Y keyid-bob
                                                  ssid c* m1* m2*)))))
            ((= type msg-diffie-hellman-commit)
             ;; Both sides started the AKE.
             ;; TODO: test this.
             (get-bytevector p (get-unpack p "!L"))
             (let ((their-mac (bytevector->uint (get-bytevector p (get-unpack p "!L"))))
                   (our-mac (bytevector->uint X-hash)))
               (cond ((> our-mac their-mac)
                      ;; Resend our D-H Commit message and ignore theirs.
                      (send (bytevector-append
                             (pack "!SC" otr-version msg-diffie-hellman-commit)
                             (pack "!L" (bytevector-length Xbv)) Xbv
                             (pack "!L" (bytevector-length X-hash)) X-hash))
                      (auth-state-awaiting-dhkey (recv) Xbv X-hash x X r))
                     (else
                      ;; Ignore the D-H Commit message we sent.
                      (set-port-position! p 2)
                      (auth-state-none p)))))
            (else
             (auth-state-awaiting-dhkey (recv) Xbv X-hash x X r)))))


  ;; "Bob" get's Alice's public DSA key
  (define (auth-state-awaiting-signature p x X Y keyid-bob ssid c* m1* m2*)
    (let ((type (get-u8 p)))
      (cond ((= type msg-signature)
             (let ((X-alice (get-bytevector p (get-unpack p "!L")))
                   (mac (get-bytevector p 160/8)))
               (unless (bytevector=? mac (MAC m2* (pack "!L" (bytevector-length X-alice))
                                              X-alice))
                 (error 'auth-state-awaiting-signature "Bad message MAC"))
               (aes-ctr! X-alice 0 X-alice 0 (bytevector-length X-alice) c* 0)
               (let* ((X-alice (open-bytevector-input-port X-alice))
                      (key-alice (get-public-key X-alice))
                      (keyid-alice (get-unpack X-alice "!L"))
                      (r (bytevector->uint (get-bytevector X-alice q-len)))
                      (s (bytevector->uint (get-bytevector X-alice q-len))))
                 (unless (verify-public-key key-alice m1* keyid-alice Y X r s)
                   (error 'auth-state-awaiting-signature "Bad message signature"))
                 (clear-aes-schedule! c*)
                 (set-established! (*state*) ssid keyid-bob x X keyid-alice Y key-alice)
                 (queue-data 'session-established 'from-here)
                 (msg-state-encrypted (recv)))))
            (else
             (auth-state-awaiting-signature (recv) x X Y keyid-bob ssid c* m1* m2*)))))

  (define (plaintext-state p)
    (let ((type (get-u8 p)))
      (cond ((= type msg-diffie-hellman-commit)
             (set-port-position! p 2)   ;before the type
             (auth-state-none p))
            (else
             (send-error "I can't read your pernicious secret writing right now")
             (queue-data 'undecipherable-message #f)
             (plaintext-state (recv))))))

  (define (auth-state-none p)
    (unless (eqv? msg-diffie-hellman-commit (get-u8 p))
      (auth-state-none (recv)))
    ;; X-encrypted is "Bob"'s g^x encrypted with a key he reveals in
    ;; the next message.
    (let* ((X-encrypted (get-bytevector p (get-unpack p "!L")))
           (X-hash (get-bytevector p (get-unpack p "!L"))))
      (let-values (((y Y) (make-secret g n dh-length 100)))
        (print (list 'our-dh-privkey (hex y)))
        (print (list 'our-dh-pubkey (hex Y)))
        (send (bytevector-append (pack "!SC" otr-version
                                       msg-diffie-hellman-key)
                                 (uint->mpi Y)))
        (auth-state-awaiting-reveal-sig (recv) X-encrypted X-hash y Y))))

  (define (auth-state-awaiting-reveal-sig p X-encrypted X-hash y Y)
    (unless (eqv? msg-reveal-signature (get-u8 p))
      (error 'auth-state-awaiting-reveal-sig "wrong message type"))
    (let* ((rkey (get-bytevector p (get-unpack p "!L")))
           (X-bob (get-bytevector p (get-unpack p "!L")))
           (mac (get-bytevector p 160/8))
           (X (make-bytevector (bytevector-length X-encrypted))))
      ;; Decrypt "Bob"'s g^x
      (aes-ctr! X-encrypted 0 X 0 (bytevector-length X) (expand-aes-key rkey) 0)
      (unless (bytevector=? X-hash (sha-256->bytevector (sha-256 X)))
        (error 'auth-state-awaiting-reveal-sig "Bad message M(X)"))
      (let ((X (mpi->uint X)))
        (unless (and (<= 2 X (- n 2)) (not (= X Y)))
          (error 'auth-state-awaiting-reveal-sig "Bad message g^x"))
        (print (list 'their-dh-pubkey (number->string X 16)))
        (let-values (((ssid c c* m1 m2 m1* m2*) (make-keys X y)))
          (unless (bytevector=? mac (MAC m2 (pack "!L" (bytevector-length X-bob))
                                         X-bob))
            (error 'auth-state-awaiting-reveal-sig "Bad message MAC"))
          ;; Decrypt "Bob"'s public key
          (aes-ctr! X-bob 0 X-bob 0 (bytevector-length X-bob) c 0)
          (clear-aes-schedule! c)
          (let* ((X-bob (open-bytevector-input-port X-bob))
                 (key-bob (get-public-key X-bob))
                 (keyid-bob (get-unpack X-bob "!L"))
                 (keyid-alice 1)        ;ID for the D-H key
                 (X-alice (sign-public-key (otr-state-our-dsa-key (*state*))
                                           m1* keyid-alice Y X))
                 (r (bytevector->uint (get-bytevector X-bob q-len)))
                 (s (bytevector->uint (get-bytevector X-bob q-len))))
            (unless (verify-public-key key-bob m1 keyid-bob X Y r s)
              (error 'auth-state-awaiting-reveal-sig "Bad message signature"))
            ;; Encrypt our public key
            (aes-ctr! X-alice 0 X-alice 0 (bytevector-length X-alice) c* 0)
            (clear-aes-schedule! c*)
            (send (bytevector-append
                   (pack "!SC" otr-version msg-signature)
                   (pack "!L" (bytevector-length X-alice))
                   X-alice
                   (MAC m2* (pack "!L" (bytevector-length X-alice)) X-alice)))
            (set-established! (*state*) ssid keyid-alice y Y keyid-bob X key-bob)
            (queue-data 'session-established 'from-there)
            (msg-state-encrypted (recv)))))))

  ;; "Bob"'s part of the data exchange phase
  (define (msg-state-encrypted p)
    (let ((type (get-unpack p "C")))
      (cond ((= type msg-data)
             (let*-values (((flags skeyid rkeyid) (get-unpack p "!uCLL"))
                           ((next-key) (get-bytevector p (get-unpack p "!L")))
                           ((ctr) (bitwise-arithmetic-shift-left (get-unpack p "!Q") 64))
                           ((msg) (get-bytevector p (get-unpack p "!L")))
                           ((pos) (port-position p))
                           ((mac) (get-bytevector p 160/8))
                           ((old-keys) (get-bytevector p (get-unpack p "!L"))))
               (print "Revealed MAC keys: " old-keys)
               ;;(assert (port-eof? p))
               ;; TODO: handle flag-ignore-unreadable
               (assert (and (not (zero? ctr))))
               ;; TODO: manage their CTR
               #;
               (unless (> ctr (otr-state-their-ctr (*state*)))
                 (send-error "You transmitted an unreadable encrypted message (CTR).")
                 (error 'msg-state-encrypted "Bad CTR"))
               (print (list 'flags flags 'skeyid skeyid 'rkeyid rkeyid
                            'ctr (number->string ctr 16)
                            'old-keys old-keys))
               (let* ((X (cdr (assv skeyid (otr-state-their-pubkeys (*state*)))))
                      (Y (cdr (assv rkeyid (otr-state-our-pubkeys (*state*)))))
                      (y (cdr (assv rkeyid (otr-state-our-keys (*state*)))))
                      (secbytes (uint->mpi (expt-mod X y n)))
                      ;;(sendbyte (if (> Y X) 1 2))
                      (recvbyte (if (> Y X) 2 1))
                      (enckey (subbytevector (h1 recvbyte secbytes) 0 16))
                      (mackey (sha-1->bytevector (sha-1 enckey))))
                 (print "X: " (hex X))
                 (print "y: " (hex y))

                 (set-port-position! p 0)
                 (unless (bytevector=? mac (sha-1->bytevector
                                            (hmac-sha-1 mackey (get-bytevector p pos))))
                   (send-error "You transmitted an unreadable encrypted message (MAC).")
                   (error 'msg-state-encrypted "Bad MAC"))
                 (otr-state-their-ctr-set! (*state*) ctr)
                 (otr-state-our-latest-acked-set! (*state*) rkeyid)
                 (unless (assv (+ skeyid 1) (otr-state-their-pubkeys (*state*)))
                   ;; Add their next key
                   (print "Added key: " (+ skeyid 1) " "
                          (hex (bytevector->uint next-key)))
                   (otr-state-their-pubkeys-set! (*state*)
                                                 (cons (cons (+ skeyid 1)
                                                             (bytevector->uint next-key))
                                                       (otr-state-their-pubkeys (*state*))))
                   (print "Their keys: " (otr-state-their-pubkeys (*state*))))
                 ;; Decrypt the message
                 (aes-ctr! msg 0 msg 0 (bytevector-length msg)
                           (expand-aes-key enckey) ctr)
                 (cond ((bytevector-u8-index msg 0) =>
                        (lambda (nulpos)
                          (let ((msgpart (subbytevector msg 0 nulpos))
                                (tlvpart (subbytevector msg (+ nulpos 1)
                                                        (bytevector-length msg))))
                            (unless (bytevector=? msgpart #vu8())
                              (queue-data 'encrypted (utf8->string msgpart)))
                            ;; XXX: The library user should probably
                            ;; send heartbeats... it's not possible to
                            ;; know here if a reply to this encrypted
                            ;; message will be generated anyway.
                            (let ((tlvs (tlv-decode tlvpart)))
                              (cond ((assv tlv-finish tlvs) =>
                                     (lambda (_)
                                       (forget-session! (*state*))
                                       (queue-data 'session-finished 'by-them)
                                       (plaintext-state (recv))))
                                    (else
                                     (print "TLVs: " tlvs)))))))
                       (else
                        (unless (bytevector=? msg #vu8())
                          (queue-data 'encrypted (utf8->string msg))))))))
            (else
             (send-error "That was unexpected of you.")))
      (msg-state-encrypted (recv))))


  ;; Alice's part of the data exchange phase. Used to send an
  ;; encrypted message to the correspondent.
  (define (otr-send-encrypted! state msg)
    (define (make-next-key! state)
      (let ((latest-id (caar (otr-state-our-keys state))))
        (when (= (otr-state-our-latest-acked state) latest-id)
          (print "Making a new DH key")
          (let-values (((y Y) (make-secret g n dh-length 100)))
            ;; (print "Next public key: " (+ latest-id 1) " -- " (hex Y))
            ;; (print "Next private key: " (+ latest-id 1) " -- " (hex y))
            (otr-state-our-keys-set! state (cons (cons (+ latest-id 1) y)
                                                 (otr-state-our-keys state)))
            (otr-state-our-pubkeys-set! state (cons (cons (+ latest-id 1) Y)
                                                    (otr-state-our-pubkeys state)))))))
    ;; This will go in the encrypted message part
    (define (encode-message msg tlvs)
      (let ((msg (string->utf8 msg)))
        (bytevector-append (cond ((bytevector-u8-index-right msg 0) =>
                                  (lambda (i) (subbytevector msg 0 i)))
                                 (else msg))
                           #vu8(0)
                           ;; Slightly random padding
                           (tlv-encode tlv-null (make-bytevector
                                                 (random-integer 7)
                                                 0)))))
    (parameterize ((*state* state))
      (make-next-key! state)
      (otr-state-our-ctr-set! state (+ 1 (otr-state-our-ctr state)))
      (let ((X (car (otr-state-their-pubkeys state)))
            (next-Y (car (otr-state-our-pubkeys state)))
            (Y (assv (otr-state-our-latest-acked state) (otr-state-our-pubkeys state)))
            (y (assv (otr-state-our-latest-acked state) (otr-state-our-keys state)))
            (ctr (otr-state-our-ctr state))
            (msg (encode-message msg '())))
        (let* ((secbytes (uint->mpi (expt-mod (cdr X) (cdr y) n)))
               (sendbyte (if (> (cdr Y) (cdr X)) 1 2))
               ;;(recvbyte (if (> (cdr Y) (cdr X)) 2 1))
               (enckey (subbytevector (h1 sendbyte secbytes) 0 16))
               (mackey (sha-1->bytevector (sha-1 enckey)))
               ;; TODO: put in old MAC keys here
               (old-keys #vu8()))
          ;; Encrypt the message
          (aes-ctr! msg 0 msg 0 (bytevector-length msg) (expand-aes-key enckey)
                    (bitwise-arithmetic-shift-left ctr 64))
          (let ((data (bytevector-append
                       (pack "!SC" otr-version msg-data)
                       (pack "!uCLL" 0 (car Y) (car X))
                       (uint->mpi (cdr next-Y))
                       (pack "!Q" ctr)
                       (pack "!L" (bytevector-length msg)) msg)))

            (send (bytevector-append data
                                     (sha-1->bytevector
                                      (hmac-sha-1 mackey data))
                                     (pack "!L" (bytevector-length old-keys))
                                     old-keys))

            ;; TODO: This is not a good interface
            (empty-queue! state))))))


  ;; Updates the OTR state with the given message and returns a list
  ;; of strings to send to the correspondent, incoming encrypted
  ;; messages, etc.
  (define (otr-update! state msg)
    (cond ((string-contains msg "?OTR,") =>
           ;; Fragmented message
           (lambda (i)
             (let ((parts (string-split msg #\, 3 i (string-index-right msg #\,))))
               (let ((k (string->number (cadr parts) 10))
                     (n (string->number (caddr parts) 10))
                     (piece (cadddr parts)))
                 ;; TODO: sanity checks.
                 (otr-state-frags-set! state (cons piece (otr-state-frags state)))
                 (if (= k n)
                     (let ((msg (apply string-append (reverse (otr-state-frags state)))))
                       (otr-state-frags-set! state '())
                       (otr-update! state msg))
                     '())))))
          ((string-contains msg "?OTR:") =>
           (lambda (i)
             (let ((p (open-bytevector-input-port
                       (base64-decode (substring msg (+ i (string-length "?OTR:"))
                                                 (string-index-right msg #\.))))))
               (cond ((= (get-unpack p "!S") otr-version)
                      (return state p)
                      (empty-queue! state))
                     (else '())))))
          ((string-contains msg "?OTR Error:") =>
           (lambda (i)
             (otr-state-k-set! state auth-state-none)
             `((remote-error . ,(substring msg (+ i (string-length "?OTR Error:"))
                                           (string-length msg)))
               #;(outgoing . "?OTRv2?")))) ;TODO: initiate AKE depending on policy
          ;; TODO: strip the tag and pass through the plaintext
          ((string-contains msg whitespace-prefix) =>
           ;; Tagged plaintext
           (lambda (i)
             (cond ((string-contains msg v2-tag (+ i (string-length whitespace-prefix)))
                    ;; They offer OTRv2
                    (otr-state-k-set! state start-ake)
                    (return state #f)
                    (empty-queue! state))
                   (else
                    '()))))             ;offer not taken
          ((or (string-contains msg "?OTR?")
               (string-contains msg "?OTRv"))
           ;; TODO: handle the other combinations of versions
           (otr-state-k-set! state start-ake)
           (return state #f)
           (empty-queue! state))

          (else '()))))
