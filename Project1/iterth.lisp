;FNU AKANKSHA
;Oct 18 2016
;CS 541
;Lab 1

(defstruct stack
  elements)
 
(defun stack-push (element stack)
  (push element (stack-elements stack)))
 
(defun stack-pop (stack)(pop (stack-elements stack)))
 
(defun stack-empty (stack)
  (endp (stack-elements stack)))
 
(defun stack-top (stack)
  ;(print stack)
  (car  stack))
 
(defun stack-peek (stack)
  (stack-top stack))

(defun movedisk(from to disknum)
(format t "Move Disk ~D  ~D --> ~D~&" disknum from to)
)
(defun movedisks(Src Dest s d)
(setq topdisk1 (pop Src) )
(setq topdisk2 (pop Dest))

(cond
((eq NIL topdisk1)
(progn
(print "in 1")
(push topdisk2 Src)
(movedisk d s topdisk2)
;(print Source)
;(print Auxiliary)
;(print Destination)
)
)
((eq NIL topdisk2)
(progn
(print "in 2")
(push topdisk1 Dest)
(movedisk s d topdisk1)
;(print Source)
;(print Auxiliary)
;(print Destination)
)
)
((> topdisk1 topdisk2)
(progn
(print "in 3")
(push topdisk1 Src)
(push topdisk2 Src)
(movedisk d s topdisk2)
;(print Source)
;(print Auxiliary)
;(print Destination)
))
((< topdisk1 topdisk2)
(progn
(print "in 4")
(push topdisk2 Dest)
(push topdisk1 Dest)
(movedisk s d topdisk1)
;(print Source)
;(print Auxiliary)
;(print Destination)
))
)
(return-from movedisks (list Src Dest))
)


(defun check(i)
(cond
((= 1 (mod i 3))
(print i)
(setq lst (movedisks Source Destination s d))
(setq Source (first lst))
(setq Destination (second lst))
)
((= 2 (mod i 3))
(print i)
(setq lst1 (movedisks Source Auxiliary s a))
(setq Source (first lst1))
(setq Auxiliary (second lst1))
)
((= 0 (mod i 3))
(print i)
(setq lst2 (movedisks Auxiliary Destination a d))
(setq Auxiliary (first lst2))
(setq Destination (second lst2))
)
)
(print (reverse Source))
(print (reverse Auxiliary))
(print (reverse Destination))
)

(defun TOH(n)
(setq s 's)
(setq d 'd)
(setq a 'a)

(if (= 0 (mod n 2))
(progn
(setq d 'a)
(setq a'd)
))
(setq totalmoves (- (expt 2 n) 1))
(setq counter n)

(loop for i from 1 to totalmoves
do(check i)
)
)
(defun create(n)
(setq Source ())
(loop for i from n downto 1 by 1
do(push i Source)
)
(return-from create Source)
)
(defun MainHanoi(n)
(setq Source (create n))
(setq Destination ())
(setq Auxiliary ())
(TOH n)
)
