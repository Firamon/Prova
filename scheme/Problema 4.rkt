;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Problema 4|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;la procedura principale, il cui obiettivo è descritto sopra
;(define btr-sum
  ;(lambda (dvd dvs)    ;input: dvd = dividendo , dvs = divisore
    ;(lsd dvs)
    ;(btr-carry-sum dvd (string-ref dvs (-(string-length dvs)1)))
    ;(btr-sum (substring dvd 0 (-(string-length dvd)1) (substring dvs 0 (-(string-length dvs)1))))
;date le rappresentazioni BTR di due interi (stringhe) e il riporto in entrata (carattere), restituisce la rappresentazione BTR della somma inclusiva del riporto

(define fun
  (lambda(dvd dvs)
    (if (> (string-length dvd) 0)
    (string-append (string
    (btr-digit-sum (string-ref dvd (-(string-length dvd)1)) dvs (btr-digit-carry (string-ref dvd (-(string-length dvd)1)) dvs #\.)))
    (fun (substring dvd 0 (-(string-length dvd)1)) dvs))
    ""
    )
   )
 )
;data una rappresentazione BTR (stringa), restituisce la rappresentazione non vuota equivalente in cui le eventuali cifre zero (#\.) in testa, ininfluenti, sono rimosse
(define normalized-btr
  (lambda (ter)
    (if (> (string-length ter) 0)
        (if (char=? (string-ref ter 0) #\.)
             (normalized-btr (substring ter 1 (string-length ter)))
             (substring ter 0 (string-length ter))
             )
        #\
    )
  )
)
;data una rappresentazione BTR (stringa), restituisce la cifra meno significativa (carattere) oppure zero (#\.) se l’argomento è la stringa vuota
(define lsd
  (lambda (ter)
    (if (> (string-length ter) 0)
        (string-ref ter (- (string-length ter) 1))
        #\.
        )
    )
 )
;data una rappresentazione BTR (stringa), restituisce la parte che precede l’ultima cifra (stringa) oppure la stringa vuota ("") se l’argomento è la stringa vuota
(define head
  (lambda (ter)
    (if (> (string-length ter) 1)
        (string-ref ter (- (string-length ter) 2))
        #\.
        )
    )
 )
;date due cifre BTR “incolonnate” e il relativo riporto BTR in entrata (caratteri), restituisce la cifra BTR corrispondente (carattere) della rappresentazione della somma

;; Somma di tre cifre nel sistema ternario bilanciato (caratteri):
;; - u, v rappresentano le cifre "incolonnate",
;; - c rappresenta il riporto "in entrata";
;; - la cifra restituita rappresenta la cifra "incolonnata"
;;   con u, v nel risultato.
;;
;; Il riporto va considerato a parte,
;; definendo una procedura "carry" con analoga struttura per casi.

(define btr-digit-sum                    ; val:     carattere +/./-
  (lambda (u v c)                        ; u, v, c: caratteri +/./-
    (cond ((char=? u #\-)                ; u v c
           (cond ((char=? v #\-)
                  (cond ((char=? c #\-)  ; - - -
                         #\.)
                        ((char=? c #\.)  ; - - .
                         #\+)
                        ((char=? c #\+)  ; - - +
                         #\-)))
                 ((char=? v #\.)
                  (cond ((char=? c #\-)  ; - . -
                         #\+)
                        ((char=? c #\.)  ; - . .
                         #\-)
                        ((char=? c #\+)  ; - . +
                         #\.)))
                 ((char=? v #\+)         ; - + c
                  c)))
          ((char=? u #\.)
           (cond ((char=? v #\-)
                  (cond ((char=? c #\-)  ; . - -
                         #\+)
                        ((char=? c #\.)  ; . - .
                         #\-)
                        ((char=? c #\+)  ; . - +
                         #\.)))
                 ((char=? v #\.)         ; . . c
                  c)
                 ((char=? v #\+)
                  (cond ((char=? c #\-)  ; . + -
                         #\.)
                        ((char=? c #\.)  ; . + .
                         #\+)
                        ((char=? c #\+)  ; . + +
                         #\-)))))
          ((char=? u #\+)
           (cond ((char=? v #\-)         ; + - c
                  c)
                 ((char=? v #\.)
                  (cond ((char=? c #\-)  ; + . -
                         #\.)
                        ((char=? c #\.)  ; + . .
                         #\+)
                        ((char=? c #\+)  ; + . +
                         #\-)))
                 ((char=? v #\+)
                  (cond ((char=? c #\-)  ; + + -
                         #\+)
                        ((char=? c #\.)  ; + + .
                         #\-)
                        ((char=? c #\+)  ; + + +
                         #\.)))))
          )))
;date due cifre BTR “incolonnate” e il relativo riporto BTR in entrata (caratteri), restituisce il riporto BTR in uscita (carattere) conseguente alla somma delle cifre
    (define btr-digit-carry              ; val:     carattere +/./-
  (lambda (u v c)                        ; u, v, c: caratteri +/./-
    (cond ((char=? u #\-)                ; u v c
           (cond ((char=? v #\-)
                  (cond ((char=? c #\-)  ; - - -
                         #\-)
                        ((char=? c #\.)  ; - - .
                         #\.)
                        ((char=? c #\+)  ; - - +
                         #\.)))
                 ((char=? v #\.)
                  (cond ((char=? c #\-)  ; - . -
                         #\.)
                        ((char=? c #\.)  ; - . .
                         #\.)
                        ((char=? c #\+)  ; - . +
                         #\.)))
                 ((char=? v #\+)         ; - + c
                  c)))
          ((char=? u #\.)
           (cond ((char=? v #\-)
                  (cond ((char=? c #\-)  ; . - -
                         #\.)
                        ((char=? c #\.)  ; . - .
                         #\.)
                        ((char=? c #\+)  ; . - +
                         #\.)))
                 ((char=? v #\.)         ; . . c
                  c)
                 ((char=? v #\+)
                  (cond ((char=? c #\-)  ; . + -
                         #\.)
                        ((char=? c #\.)  ; . + .
                         #\.)
                        ((char=? c #\+)  ; . + +
                         #\+)))))
          ((char=? u #\+)
           (cond ((char=? v #\-)         ; + - c
                  c)
                 ((char=? v #\.)
                  (cond ((char=? c #\-)  ; + . -
                         #\.)
                        ((char=? c #\.)  ; + . .
                         #\.)
                        ((char=? c #\+)  ; + . +
                         #\+)))
                 ((char=? v #\+)
                  (cond ((char=? c #\-)  ; + + -
                         #\.)
                        ((char=? c #\.)  ; + + .
                         #\.)
                        ((char=? c #\+)  ; + + +
                         #\+)))))
          )))




; + . = + , - . +, . . ., - + .,+ + -R+,- - +R-
;ho 2 stringhe, 1: dividendo 2:divisore
;a) prendo la cifra meno significativa di 2 e faccio la somma tra quella cifra e la stringa 1    1("-+--+" "-.--")   -->    1("-+--" "-.-") R.
;b) prendo la cifra meno significativa di 2 e faccio la somma tra quella cifra e la stringa 1 (meno 1 cifra significativa di 2 per ogni chiamata ricorsiva) 1("-+--" "-.-")   -->     1("-+--" "-.")    ->       1("-.+" "-.") R+
;1("-.+" "-.")   ->    1("-." "-") R+      1("-." "-")       ->        1("--" "")

(normalized-btr "-+--+")
(lsd "....-+--+")
(head "-+--+")