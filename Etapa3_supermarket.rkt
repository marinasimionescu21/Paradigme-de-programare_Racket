#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)

;; ATENȚIE: Pentru această etapă a temei este necesar să implementați
;;          întâi TDA-ul queue în fișierul queue.rkt.
;; Reveniți la sarcinile din acest fișier după ce ați implementat tipul 
;; queue și ați verificat implementarea folosind checker-ul.


; Structura counter nu se modifică.
; Ceea ce se modifică este implementarea câmpului queue:
; - în loc de listă, acesta va fi o coadă (o structură de tip queue)
; - acest lucru nu este vizibil în definiția structurii counter,
;   ci în implementarea operațiilor acestui tip de date (tipul counter)
(define-struct counter (index tt et queue) #:transparent)


; TODO
; Actualizați funcțiile de mai jos astfel încât ele să folosească
; o structură de tip queue pentru reprezentarea cozii de persoane.
; Elementele cozii continuă să fie perechi (nume . nr_produse).
; Este esențial să respectați "bariera de abstractizare", adică să
; operați cu coada doar folosind interfața acestui TDA:
; - empty-queue
; - queue-empty?
; - enqueue
; - dequeue
; - top
; Obs: Doar câteva funcții vor necesita actualizări.
(define (empty-counter index)           ; testată de checker
  (make-counter index 0 0 empty-queue))

(define (update f counters index)
  (cond ((null? counters) counters)
        ((< index 0) counters)
        ((= (counter-index (car counters)) index) (cons (f (car counters)) (update f (cdr counters) index)))
        (else (cons (car counters)(update f (cdr counters) index)))))

(define tt+
  (lambda (C)
    (lambda (minutes)
   (match C
    [(counter index tt et queue)
     (define Cc (struct-copy counter C[tt (+ tt minutes)])) Cc]))))

(define et+
  (lambda (C)
    (lambda (minutes)
   (match C
    [(counter index tt et queue)
     (define Cc (struct-copy counter C[et (+ et minutes)])) Cc]))))

(define (add-helper C name n-items)
  (match C
    [(counter index tt et queue)
     (cond [(= 0 et) (define Cc (struct-copy counter C[queue (enqueue (cons name n-items) queue)] [tt (+ tt n-items)] [et (+ et n-items)]))Cc]
           [else (define Cc (struct-copy counter C[queue (enqueue (cons name n-items) queue)] [tt (+ tt n-items)]))Cc])
     ]))

(define (add-to-counter name items)     ; testată de checker
  (λ (C); nu modificați felul în care funcția își primește argumentele
    (add-helper C name items)))

(define (min-helper-tt counters)
  (sort counters
        (lambda(c1 c2)
          (< (counter-tt c1) (counter-tt c2)))))

(define (min-helper-et counters)
  (sort counters
        (lambda(c3 c4)
          (< (counter-et c3) (counter-et c4)))))

(define (general-func f counters)
  (define C1 (first(min-helper-tt counters)))
  (define C2 (first (min-helper-et counters)))
   (cond ((equal? (list f) (list min-helper-tt)) (cons (counter-index C1) (counter-tt C1)))
         (else (cons (counter-index C2) (counter-et C2)))))
(define min-tt (lambda (counters) (general-func min-helper-tt counters)))
(define min-et (lambda (counters) (general-func min-helper-et counters)))


(define (mySum L)
  (apply + L))

(define (remove-first-from-counter C)   ; testată de checker
  (cond [(= (queue-size-l (counter-queue C)) 1) (empty-counter (counter-index C))]
        [(= (queue-size-l (counter-queue C)) 0) (define qq (struct-copy queue (counter-queue C)
                                                                        [left (cdr (foldl cons (queue-left (counter-queue C)) (queue-right (counter-queue C))))]
                                                                        [right null]
                                                                        [size-r 0]
                                                                        [size-l (- (queue-size-r (counter-queue C)) 1)]))
                                                (make-counter (counter-index C)
                                                              (mySum (map cdr (queue-left qq)))
                                                              (cdar(queue-left qq))
                                                              (dequeue (counter-queue C)))]
        [else (make-counter (counter-index C)
                            (mySum(cdr(map cdr (queue-left (counter-queue C)))))
                            (cdadr(queue-left (counter-queue C)))
                            (dequeue (counter-queue C)))]))

; TODO
; Implementați o funcție care calculează starea unei case după un număr dat de minute.
; Funcția presupune, fără să verifice, că în acest timp nu a ieșit nimeni din coadă, 
; deci va avea efect doar asupra câmpurilor tt și et.
; (cu alte cuvinte, este responsabilitatea utilizatorului să nu apeleze această funcție
; cu minutes > timpul până la ieșirea primului client din coadă)
; Atenție: casele fără clienți nu trebuie să ajungă la timpi negativi!
(define (pass-time-through-counter minutes)
  (λ (C)
    (cond [(and (= (counter-tt C) 0) (= (counter-et C) 0)) C]
          [(or (> minutes (counter-tt C)) (> minutes (counter-et C))) (empty-counter (counter-index C))]
          [else (define CC (struct-copy counter C [index (counter-index C)]
                                                  [tt (- (counter-tt C) minutes)]
                                                  [et (- (counter-et C) minutes)])) CC])))
   

; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 2, apar modificări în:
; - formatul listei de cereri (parametrul requests)
; - formatul rezultatului funcției (explicat mai jos)
; requests conține 4 tipuri de cereri (3 moștenite din etapa 2 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă            (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute       (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al tuturor caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)         (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                     (   NOU!   )
; Obs: Au dispărut cererile de tip remove-first, pentru a lăsa loc unui mecanism mai 
; sofisticat de a scoate clienții din coadă (pe măsură ce trece timpul).
; Sistemul trebuie să proceseze cele 4 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)  (ca înainte)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți) (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele (și cele fast, și cele slow), 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>       (ca înainte)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică.
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista caselor în starea finală (ca rezultatul din etapele 1 și 2)
; Obs: Pentru a contoriza ieșirile din cozi, puteți să lucrați într-o funcție ajutătoare
; (cu un parametru în plus față de funcția serve), pe care serve doar o apelează.
(define (serve requests fast-counters slow-counters)
  'your-code-here)
        
