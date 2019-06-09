;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Problema 6|) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks")) #f)))



(define L-tessellation
  (lambda(passo)
    (if (> passo 2)
        (glue-tiles
         (L-tessellation (- passo 2))
            (glue-tiles (shift-down (quarter-turn-left (L-tessellation (- passo 2))) 1)
               (glue-tiles
                    (shift-right (quarter-turn-right (L-tessellation (- passo 2))) 1)
                     (shift-down (shift-right (L-tessellation (- passo 2)) 0.5) 0.5)
                      )
                )
           )
        (glue-tiles L-tile
          (glue-tiles
             (shift-down (quarter-turn-left L-tile) 1)
                (glue-tiles
                   (shift-right (quarter-turn-right L-tile) 1)
                       (shift-down (shift-right L-tile 0.5) 0.5))))
    )
   )
)
      
(L-tessellation 2)