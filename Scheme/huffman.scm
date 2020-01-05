(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define symbol-leaf cadr)

(define weight-leaf caddr)


(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define left-branch car)

(define right-branch cadr)

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))


(define (decode bits tree)
  (let decode-1 ((bits bits) (current-branch tree))
    (if (null? bits) '()
        (let ((next-branch (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch))))))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (encode message tree)
  (if (null? message) '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree-root)
  (define (encode-descend tree code)
    (cond ((leaf? tree) (reverse code))
          ((memq symbol (symbols (left-branch tree)))
           (encode-descend (left-branch tree) (cons 0 code)))
          (else
           (encode-descend (right-branch tree) (cons 1 code)))))
  (if (memq symbol (symbols tree-root))
      (encode-descend tree-root '())
      (error "not found -- ENCODE-SYMBOL" symbol)))


(define (adjoin-set x set) ;; set as a sorted (descending) list
  (cond ((null? set) (list x))
        ((>= (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs) '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leafs)
  (if (null? (cdr leafs))
      (car leafs)
      (make-code-tree (car leafs) (successive-merge (cdr leafs)))))
