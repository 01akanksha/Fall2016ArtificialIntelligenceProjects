;FNU AKANKSHA
;Oct 18 2016
;CS 541
;Lab 1

(defun MoveHanoi(n to from Au)
    (cond
        (
            (> n 0)
            (MoveHanoi (- n 1) Au from to)
           (format t "Move Disk ~D  ~D --> ~D~&" n from to)
            (MoveHanoi (- n 1) to Au from)
        )
    )
)

(defun TowerOfHanoi(n)
    (MoveHanoi n 'Destination 'Source 'Auxillary)
)
