;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Problema8) (read-case-sensitive #t) (teachpacks ((lib "hanoi.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "hanoi.ss" "installed-teachpacks")))))
;sposto n-1 cerchi dalla prima alla seconda asticella, sposto 1 cerchio nella terza asticella
(define hanoi
 (lambda (n)
   (hanoi-rec n 1 2 3)
   ))

(define hanoi-rec
 (lambda (n a b c)
   (if (= n 1)
       (list (list a b))
       (append (hanoi-rec (- n 1) a c b) (cons (list a b) (hanoi-rec (- n 1) c b a)))
       )
   ))

(define hanoi-disks
  (lambda (n k) ;n > 0, 0 <= k <= 2^n-1
    (hanoi-disks-rec n k 1 2 3 0 0 0)
    ))

(define hanoi-disks-rec
  (lambda (n k s d t ds dd dt)
    (if (= n 0)
        (list (list s ds) (list d dd) (list t dt))
        (if (< k (expt 2 (- n 1)))
            (hanoi-disks-rec (- n 1) k s t d (+ ds 1) dt dd)
            (hanoi-disks-rec (- n 1) (- k (expt 2 (- n 1))) t d s dt (+ dd 1) ds)
            )
        )
    ))

;(hanoi-disks 3 0); → '((1 3) (3 0) (2 0)) (hanoi-disks 5 13) → '((3 2) (2 1) (1 2))
;(hanoi-disks 3 1); → '((3 0) (2 1) (1 2))
;(hanoi-disks 3 2); → '((2 1) (1 1) (3 1)) (hanoi-disks 15 19705)
;(hanoi-disks 3 3); → '((1 1) (3 2) (2 0)) → '((3 4) (2 9) (1 2))
;(hanoi-disks 3 4); → '((3 2) (2 1) (1 0))
;(hanoi-disks 3 5); → '((2 1) (1 1) (3 1)) (hanoi-disks 15 32767)
;(hanoi-disks 3 6) ; → '((1 1) (3 0) (2 2)) → '((3 0) (2 15) (1 0))
;(hanoi-disks 3 7) ;→ '((3 0) (2 3) (1 0))

(define hanoi-picture
  (lambda (n k) ;n > 0, 0 <= k <= 2^n-1
    (hanoi-picture-rec n k 1 2 3 0 0 0 n)
    ))

(define hanoi-picture-rec
  (lambda (n k s d t ds dd dt num)
    (if (= n 0)
        (towers-background num)
        (if (< k (expt 2 (- n 1)))
            (above (disk-image n num s ds) (hanoi-picture-rec (- n 1) k s t d (+ ds 1) dt dd num))
            (above (disk-image n num d dd) (hanoi-picture-rec (- n 1) (- k (expt 2 (- n 1))) t d s dt (+ dd 1) ds num))
            )
        )
    ))

(hanoi-picture 5 0)
(hanoi-picture 5 13)
(hanoi-picture 5 22)
(hanoi-picture 5 31) 
(hanoi-picture 15 19705) 
            