#lang racket/base

(define 
  (make-mobile left right)
  (list left right)
)
(define 
  (make-branch length structure)
  (list length structure)
)

; a. これに対応する、モビールの枝を返すセレクタleft-branchとright-branch、枝の構成要素を返すbranch-lengthとbranch-structureを書け。
(define 
  (left-branch mobile)
  (car mobile)
)
(define 
  (right-branch mobile)
  (cadr mobile)
)
(define 
  (branch-length branch)
  (car branch)
)
(define 
  (branch-structure branch)
  (cadr branch)
)

; b. これらのセレクタを使って、モビールの総重量を返す手続きtotal-weightを定義せよ。

; モビールの場合#t、ブランチの場合#f
(define 
  (mobile? item)
  (and (pair? item) (pair? (car item)))
)

(define 
  (branch-weight branch)

  (if (mobile? (branch-structure branch)) 
    (total-weight (branch-structure branch))
    (branch-structure branch)
  )
)

(define 
  (total-weight mobile)

  (+ (branch-weight (left-branch mobile)) 
     (branch-weight (right-branch mobile))
  )
)

(define 
  mobile1
  (make-mobile 
    (make-branch 1 2)
    (make-branch 
      3
      (make-mobile 
        (make-branch 4 5)
        (make-branch 6 7)
      )
    )
  )
)
(total-weight mobile1)
; 14

; c. ある二枝モビールがバランスが取れているかどうかテストする述語を設計せよ。
(define 
  (balanced? mobile)

  (define 
    (torque branch)

    (* (branch-length branch) 
       (branch-weight branch)
    )
  )

  (let 
    ((left (left-branch mobile)) 
      (left-structure (branch-structure (left-branch mobile)))
      (right (right-branch mobile))
      (right-structure (branch-structure (right-branch mobile)))
    )
    (and (= (torque left) (torque right)) 
         (if (mobile? left-structure) (balanced? left-structure) #t)
         (if (mobile? right-structure) (balanced? right-structure) #t)
    )
  )
)

; バランスが取れている
(define 
  mobile2
  (make-mobile 
    (make-branch 2 4.5)
    (make-branch 
      3
      (make-mobile 
        (make-branch 1 2)
        (make-branch 2 1)
      )
    )
  )
)
; バランスが取れていない
(define 
  mobile3
  (make-mobile 
    (make-branch 2 4)
    (make-branch 
      3
      (make-mobile 
        (make-branch 1 2)
        (make-branch 2 1)
      )
    )
  )
)
; バランスが取れていない
(define 
  mobile4
  (make-mobile 
    (make-branch 2 3)
    (make-branch 
      3
      (make-mobile 
        (make-branch 1 1)
        (make-branch 2 1)
      )
    )
  )
)
(balanced? mobile2)
(balanced? mobile3)
(balanced? mobile4)

; d. モビールの表現を変更し、以下のようなコンストラクタにする。あなたのプログラムを新しい表現に移行するにはどの程度の変更が必要だろうか。
(define (make-mobile2 left right) (cons left right))
(define 
  (make-branch2 length structure)
  (cons length structure)
)
; 変更は、モビールとブランチのセレクタおよびmobile?術後のみ修正すればいい。これは実装が自明であるため省略する。
