#lang racket

;Minesweeper implementation in Racket with GUI
;...With buttons from racket/gui/base
;Because there already were implementations with CLI or Canvas

(require racket/gui/base)

(define grid-size-hor 9)
(define grid-size-vrt 9)
(define mine-count 10)

(define right-click-button%
  (class button%
    (init-field [right-click-callback null] [is-clickable-func (lambda () #t)])
    (super-new)
    (define/override (on-subwindow-event r e)
      (cond
        [(and (send e button-down? 'right) (is-clickable-func))
         (cond
           [(not (null? right-click-callback)) (right-click-callback this)])]
        [(is-clickable-func) (super on-subwindow-event r e)]))))

(define (all-coordinates rows cols)
  (for*/list ([i (in-range rows)]
              [j (in-range cols)])
    (cons i j)))

(define (cord-in-bounds row col rows cols)
  (and (>= row 0) (>= col 0) (< row rows) (< col cols)))

(define (adjacent-fields row col rows cols)
  (filter
   (lambda (x) (cord-in-bounds (car x) (cdr x) rows cols))
   (list (cons (- row 1) (- col 1))
         (cons (- row 1) col)
         (cons (- row 1) (+ col 1))
         (cons row (- col 1))
         (cons row (+ col 1))
         (cons (+ row 1) (- col 1))
         (cons (+ row 1) col)
         (cons (+ row 1) (+ col 1)))))

(define (generate-mine-field rows cols)
  (define mine-positions (take (shuffle (all-coordinates rows cols)) mine-count))
  (for/list ([row (in-range rows)])
    (for/list ([col (in-range cols)])
      ;Set value to "*" for mines or to the count of adjacent mines
      (if (member (cons row col) mine-positions)
          "💣"
          (length (set-intersect (adjacent-fields row col rows cols) mine-positions))))))

(define (set-button-label button value)
  (send button enable #f)
  (cond
    [(number? value) (send button set-label (number->string value))]
    [else (send button set-label value)]))

(define mine-field-values null)
(define mine-field-buttons null)
(define game-over #f)
(define clear-field-count 0)

(define (mine-field-value row col) (list-ref (list-ref mine-field-values row) col))
(define (mine-field-button row col) (list-ref (list-ref mine-field-buttons row) col))

(define (loose-game)
  (set! game-over #t)
  (send new-game-button set-label "🤕")
  ;loop through all coordinates and set all buttons with mines to "💣"
  (for ([row (in-range grid-size-hor)])
    (for ([col (in-range grid-size-vrt)])
      (define field-value (mine-field-value row col))
      (define field-button (mine-field-button row col))
      (cond [(equal? field-value "💣") (send field-button set-label field-value)]))))

(define (win-game)
  (set! game-over #t)
  (send new-game-button set-label "😎")
  (send (new dialog%
             [parent frame]
             [label "You won!"]
             [min-width 200]
             [min-height 50]) show #t))

(define (clear-field button field-value)
  (cond [(equal? (send button get-label) "")
         (begin
           (set-button-label button field-value)
           (set! clear-field-count (+ clear-field-count 1))
           (cond [(= clear-field-count (- (* grid-size-hor grid-size-vrt) mine-count)) (win-game)]))]))

(define (try-clear-field row col)
  (define field-value (mine-field-value row col))
  (define field-button (mine-field-button row col))
  (cond [(number? field-value)
         (if (= 0 field-value)
             (clear-0-fields row col)
             (clear-field field-button field-value))]
        [else (loose-game)]))

(define (clear-0-fields row col)
  (define field-value (mine-field-value row col))
  (define field-button (mine-field-button row col))
  (cond
    [(and (not (equal? (send field-button get-label) "0")) (equal? 0 field-value))
     (begin
       (clear-field field-button field-value)
       (define adj-fields (adjacent-fields row col grid-size-hor grid-size-vrt))
       ;clear-0-fields on all adjacent fields
       (for ([adjacent-field (in-list adj-fields)])
         (clear-0-fields (car adjacent-field) (cdr adjacent-field))
         ))]
    [else (clear-field field-button field-value)]))


(set! mine-field-values (generate-mine-field grid-size-hor grid-size-vrt))
(define frame (new frame% [label "Minesweeper"] [width 500] [height 500]))

(define (new-game)
  (begin
    (send new-game-button set-label "🙂")
    (set! mine-field-values (generate-mine-field grid-size-hor grid-size-vrt))
    (set! game-over #f)
    (set! clear-field-count 0)
    ;loop through all coordinates and set all buttons to ""
    (for ([row (in-range grid-size-hor)])
      (for ([col (in-range grid-size-vrt)])
        (begin (send (mine-field-button row col) set-label "")
               (send (mine-field-button row col) enable #t))))))

;create "new game" button
(define new-game-button
  (new button%
       [parent frame]
       [label "🙂"]
       [font (make-object font% 25 'default 'normal  'ultraheavy)]
      [callback (lambda (b e) (new-game))]))

(define win-msg (new message%
     [parent frame]
     [label "Left-Click -> Clear   |   Right-Click -> Flag"]))

(define panel (new horizontal-panel% [parent frame] [stretchable-width #f]))
(set! mine-field-buttons
      (for/list ([i (in-range grid-size-hor)])
        (define sub-panel (new vertical-panel% [parent panel]))
        (for/list ([j (in-range grid-size-vrt)])
          (new right-click-button%
               [parent sub-panel]
               [min-width 65]
               [min-height 65]
               [stretchable-width #f]
               [stretchable-height #f]
               [vert-margin 0]
               [horiz-margin 0]
               [font (make-object font% 25 'default 'normal  'ultraheavy)]
               [label ""]
               [is-clickable-func (lambda () (not game-over))]
               [right-click-callback (lambda (b)
                                       (cond [(equal? "🚩" (send b get-label))
                                              (send b set-label "")]
                                             [(equal? "" (send b get-label))
                                              (send b set-label "🚩")]))]
               [callback (lambda (b e)
                           (cond [(equal? "" (send b get-label)) (try-clear-field i j)]))]))))

(send frame show #t)