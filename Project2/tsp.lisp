;FNU Akanksha
;Nov 1,2016
;CS 541
;Lab 2

(defun read-file (filename)
  (with-open-file (in filename)
    (loop for line = (read-line in nil)
          while line
          collect line)))

(defstruct city
        xcord
        ycord
        citynum
)
(defun distanceTo (src dest)
     (setq dist (sqrt (coerce (+ (* (abs (- (city-xcord src) (city-xcord dest))) (abs (- (city-xcord src) (city-xcord dest)))) (* (abs (- (city-ycord src) (city-ycord dest))) (abs (- (city-ycord src) (city-ycord dest)))))'float)))
)

(defstruct tour
        path
        distance
        fitness
)

(defun new-tour (CurrentPath)
        (setq tour (make-tour :path CurrentPath :distance 0 :fitness 0))
)

(defun Randomize (data)
  (setq n (list-length data))
	(setq lst (make-list n))
  (loop for i from 0 to (- n 1)do
    (setf (nth i lst) (nth i data))
	)
  (loop for i from 0 to (- n 1) do
		(setq j (random n))
  	(setq temp (nth j lst))
    (setf (nth j lst) (nth i lst))
		(setf (nth i lst) temp)
  )
  (return-from Randomize lst)
)

(defun getRandomizedPopulation (num initialize)
  (setq ranPop ())
  (if initialize
    (loop for i from 1 to num do
      (push (new-tour (Randomize TourManager)) ranPop)
    )
    (setq ranPop (make-list num))
	)
	(return-from getRandomizedPopulation ranPop)
  )

(defun getFittest (pop)
  (setq fittest (nth 0 pop))
  (loop for i from 1 to  (- (list-length pop) 1)  do
    (setq CurrentTour (nth i pop))
    (if (> (getFitness CurrentTour) (getFitness fittest))
      (setq fittest CurrentTour)
    )
	)
	(return-from getFittest fittest)
)
(defun getFitness (tour)
    (if (= (tour-fitness tour) 0)
		(progn
			(setq dist (getDistance tour))
			(if (= dist 0)
				(setf (tour-fitness tour) most-positive-fixnum)
				(setf (tour-fitness tour) (coerce (/ 1 (getDistance tour)) 'float)))
		))
	(tour-fitness tour)
)

(defun getDistance(tour)
	(if (= (tour-distance tour) 0)
    (progn
      (setq tourDistance 0)
      (setq CurrentPath (tour-path tour))
      (setq n (list-length CurrentPath))
      (loop for i from 0 to (- n 1) do
        (setq fromCity (nth i CurrentPath))
        (if (< i (- n 1))
          (setq destinationCity (nth (+ i 1) CurrentPath))
          (setq destinationCity (nth 0 CurrentPath))
        )
        (setq tourDistance (+ tourDistance (distanceTo fromCity destinationCity)))
      )
      (setf (tour-distance tour) tourDistance)
    )
	(tour-distance tour))
)

(defvar tournamentSize 5)
(defvar mutationRate 0.015)

(defun tournamentSelection (pop)
	(setq num (list-length pop))
	(setq tournament (getRandomizedPopulation tournamentSize nil))
	(loop for i from 0 to (- tournamentSize 1) do
		(setq j (random num))
		(setf (nth i tournament) (nth j pop))
	)
	(setq fittest (getFittest tournament))
	;(return from tournamentSelection fittest)
)

(defun checkifinpath (child city)
	(find city child)
)

(defun crossover(a b)
	(setq num (list-length TourManager))
	(setq path (make-list num))
	(setq startPos (random num))
	(setq endPos (random num))
	(loop for i from 0 to (- num 1) do
		(if (and (and (< startPos endPos) (> i startPos)) (< i endPos))
			(setf (nth i path) (nth i (tour-path a)))
			(if (> startPos endPos)
				(if (not (and (< i startPos) (> i endPos)))
						(setf (nth i path) (nth i (tour-path a)))
				)
			)
		)
	)
	(loop for i from 0 to (- num 1) do
		(if (not (checkifinpath path (nth i (tour-path b)) ))
			(loop for j from 0 to num do
				(if (not (nth j path))
					(progn
						(setf (nth j path) (nth i (tour-path b)))
						(return)
          )
				)
			)
		)
	)
	(setq child (new-tour path))
	(return-from crossover child)
)

(defun mutation (child)
	(setq Currenttour (tour-path child))
	(setq num (list-length Currenttour))
	(loop for p1 from 0 to (- num 1) do
		(setq j (random 1.00))
		(if (< j mutationRate)
			(progn
				(setq p2 (random num))
				(setq city1 (nth p1 Currenttour))
				(setq city2 (nth p2 Currenttour))
				(setf (nth p1 Currenttour) city2)
				(setf (nth p2 Currenttour) city1)
			)
		)
	)
	(return-from mutation child)
)

(defun evolvePopulation (pop)
  (setq num (list-length pop))
	(setq childPop (getRandomizedPopulation num nil))
  (setf (nth 0 childPop) (getFittest pop))
	(setq elitismOffset 1)
	(loop for i from elitismOffset to (- num 1) do
    (setq parent1 (tournamentSelection pop))
		(setq parent2 (tournamentSelection pop))
		(setq child (crossover parent1 parent2))
		(setf (nth i ChildPop) child)
	)
	(loop for i from elitismOffset to (- num 1) do
  	(setf (nth i ChildPop) (mutation (nth i ChildPop)))
	)
	(return-from evolvePopulation ChildPop)
)

(defun findStartingPoint (start)
	(loop for n in TourManager do
		(if (= (city-citynum n) start)
			(setq StartCity n)
    )
	)
	(return-from findStartingPoint StartCity)
)

(defun printTour (tour)
	(setq tourPath (tour-path tour))
	(setq lst ())
	(setq lst1 ())
	(setq foundStart nil)
	(loop for xcord in tourPath do
		(if (not foundStart )
      (if (= (city-citynum startCity) (city-citynum xcord))
        (setq foundStart t)
        (push xcord lst)
      )
		)
		(if (not (not foundStart))
			(push xcord lst1)
		)
  )
	(setq lst1 (append (reverse lst1) (reverse lst)))
	(setq citynumLst ())
	(loop for xcord in lst1 do
		(push (city-citynum xcord) citynumLst)
	)
	(setq citynumLst (reverse citynumLst))
	(format t "~%city travelled in order starting from the start city-~%")
	(printList citynumLst)

)

(defun printList (lst)
    (loop for xcord in lst do
	(format t "~d | " xcord))
	(format t "~d~%" (first lst))
)


(defun tspmain (filename)
	(setf *random-state* (make-random-state t))
      (setq line (read-file filename))
      (setq N (parse-integer (first line)))     
            (setq cities ())
			(setq citynumList ())
            (loop for i from 1 to N do
              (setq val (nth i line))
              (setq a (with-input-from-string (in val)
                 (loop for a = (read in nil nil) while a collect a))
              )
              (setq city (make-city :xcord (second a) :ycord (third a)))
              (setf (city-citynum city) (first a))
			  (setq citynumList (cons (first a) citynumList))
              (setq cities (cons city cities))
            )
            (defvar TourManager cities)

            (setq Start (parse-integer (nth (+ N 1) line)))          
                (defVar startCity (findStartingPoint start))
                (setq pop (getRandomizedPopulation 50 t))

                (format t "Initial distance travelled by the salesman(1st generation) = ~d~%" (getDistance (getFittest pop)))
                (setq pop (evolvePopulation pop))

                (loop for i from 0 to 100 do
                  (setq pop (evolvePopulation pop))
                )
                (setq finTr (getFittest pop))
                (format t "Final distance travelled by salesman after gentic evolution(100 generations) = ~d~%" (getDistance finTr))              
                (printTour finTr)             
)

(setq filename  (first *args*))
(tspmain filename)



