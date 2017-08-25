;FNU AKANKSHA
;15-Nov-2016

(defun neuralnetwork(learningrate inputsize hiddensize outputsize)
	;initialize and randomizeweights
	(setq lr learningrate)
	(setq is inputsize)
	(setq hs hiddensize)
	(setq os outputsize)
	(setq lst (init lr is hs os))
	(setq wtitoh (first lst))
	(setq wthtoo (second lst))
	;(setq lst(init lr is hs os))
	(setq lst1 (randomizeWeights wtitoh wthtoo is hs os))
	
	(return-from neuralnetwork lst1)
)

(defun randomizeWeights(wtitoh wthtoo is hs os)
;Build the weight matrices
	(loop for i from 0 to (- is 1)
  		do(loop for j from 0 to (- hs 1)
		do(setf (aref wtitoh i j) (rand -1.0 1.0))
 		)
	)

	(loop for k from 0 to (- hs 1)
                do(loop for l from 0 to (- os 1)
                do(setf (aref wthtoo k l)(rand -1.0 1.0))
                )
        )	
	(return-from randomizeWeights (list wtitoh wthtoo ai ah ao iters is hs os lr))
)

(defun rand(a b)
(return-from rand (random (+ a (- b a))))
)
(defun init(lr is hs os)
;initializing
	(setf weightsItOH (make-array (list is hs)))
	(setf weightHtoO (make-array (list hs os)))
	(setf ai (make-array (list is)))
	(setf ah (make-array (list hs)))
	(setf ao (make-array (list os)))
	(setf (aref ah (- hs 1)) 1.0)
	(setf (aref ai (- is 1)) 1.0)
	(setf iters 0)

	(return-from init (list weightsItOH weightHtoO ai ah ao iters))
)

;new method starts here

(defstruct SplitData
;To Divide data into digit and corresponding image data and generated binary output
	label
	output
	data
)

(defun get-file (filename)
	;to read from file which is converted to space seperated csv
	(with-open-file (stream filename)
		(loop for line = (read-line stream nil)
			while line
				collect line
		)
	)
)

(defun GenerateOutputBinary (index)
;to convert the actual digit into 10 digits binary format
	(setq out (make-list 10 :initial-element 0))
	(setf (nth index out) 1)
	(return-from GenerateOutputBinary out)
)

(defun getData(dataFile)
	;convert data into usable form digit data and output
		(setq line (get-file dataFile))
		(setq trainingData ())
		(setq N (length line))
		(loop for i from 0 to (- N 1) do
			(setq val (nth i line))
			(setq d (with-input-from-string (in val)
				(loop for d = (read in nil nil) while d collect d))
			)
			(setq res (make-SplitData :label (first d) :data (cdr d) :output (GenerateOutputBinary (first d))))
			(setq trainingData (cons res trainingData))
		)	
		(return-from getData trainingData)
	
)

(defstruct (NeuralNetwork
	(:constructor new-NeuralNetwork(inputSize hiddenSize outputSize weightsItoH weightHtoO)))
	inputSize
	hiddenSize
	outputSize
	weightsItoH
	weightHtoO
	;structure to create a neural network with the above paramaters
)

(defun RandomMatrices (lengthList)
;to randomize weights from the input and hidden layer
	(setq wtmatrix (make-array lengthList))
	(setq d1 (- (first lengthList) 1))
	(setq d2 (- (second lengthList) 1))
	(loop for i from 0 to d1 do
		(loop for j from 0 to d2 do
			(setf (aref wtmatrix i j) (- (random .25) .25))
		)
	)
	(return-from RandomMatrices wtmatrix)
)

(defun sigmoid (x)
	(/ 1.0 (+ 1 (exp(- x))))
)

(defun dsigmoid (y)
	(* y (- 1 y))
)

(defun forwardPropagate(input)
;to calculate the activations for output and hidden layer after forward propagating weights and returning it
	(loop for i from 0 to (- (length input) 1) do
		(setf (nth i ai) (/ (nth i input) 255.0))
	)
	(setf (nth (length input) ai) 1.0)
	(setq hs (+ 1 (NeuralNetwork-hiddenSize neuralNet)))
	(setf (nth (- hs 1) ah) 1.0)	
	(loop for j from 0 to (- hs 2) do
		(setq wt 0)
		(loop for i from 0 to (length input) do
;(print j)
			(setq wt (+ wt (* (aref (NeuralNetwork-weightsItoH neuralNet) i j) (nth i ai))))
		)
		(setf (nth j ah) (sigmoid wt))
	)	
	(setq os (NeuralNetwork-outputSize neuralNet))	
	(loop for k from 0 to (- os 1) do
		(setq wt 0)
		(loop for j from 0 to (- hs 1) do
			(setq wt (+ wt (* (aref (NeuralNetwork-weightHtoO neuralNet) j k) (nth j ah))))
		)
		(setf (nth k ao) (sigmoid wt))
	)
	(return-from forwardPropagate ao)
)

(defun backPropagate (errors)
;backpropagation algorithm to update the weights and train the NN.
	(setq outSize (NeuralNetwork-outputSize neuralNet))
	(setq hidSize (NeuralNetwork-hiddenSize neuralNet))
	(setq inSize (NeuralNetwork-inputSize neuralNet))
	(setq deltaj (make-list hidSize))
	(setq deltak (mapcar #'* (mapcar #' dsigmoid ao) errors))
		(loop for j from 0 to (- hidSize 1) do
		(setq del 0)
		(loop for k from 0 to (- outSize 1) do
			(setq del (+ del (* (* (dsigmoid (nth j ah)) (nth k deltak)) (aref (NeuralNetwork-weightHtoO neuralNet) j k))))			
		)
		(setf (nth j deltaj) del)
	)
	(loop for i from 0 to inSize do
		(loop for j from 0 to (- hidSize 1) do
			(setf (aref (NeuralNetwork-weightsItoH neuralNet) i j) (+ (aref (NeuralNetwork-weightsItoH neuralNet) i j) (* lr (nth j deltaj) (nth i ai))))			
		)
	)	
	(loop for j from 0 to hidSize do
		(loop for k from 0 to (- outSize 1) do
			(setf (aref (NeuralNetwork-weightHtoO neuralNet) j k) (+ (aref (NeuralNetwork-weightHtoO neuralNet) j k) (* lr(nth k deltak) (nth j ah))))
		)
	)
)


(defun validate()
;to calculate accuracy for the validation_data
	(setq acc 0)
	(loop for index from 0 to (- (length validation_data) 1) do 
		(setq result (test (SplitData-data (nth index validation_data))))
		(if (= result (SplitData-label (nth index validation_data)))
		(setq acc (+ acc 1))
		)
	)
	(/ acc (* (length validation_data) 1.0))
)

(defun getMaxIndex (lst)
;to get maximum index ie position at which there is element with value 1
	(position (reduce #'max lst) lst :test #'equal)
)

(defun train (data)
;Perform training and do the validation
	(setq dataLength (length data))
	(setq outSize (NeuralNetwork-outputSize neuralNet))
	(setq Oldacc 0)
	(setq count 0)
	(loop for i from 0 to 10 do
		(print (list 'iterationNo i))
		(loop for index from 0 to (- dataLength 1) do
			(forwardPropagate (SplitData-data (nth index data)))
			(print "Details after forward prop")
			(print neuralNet)
			(setq expected (SplitData-output (nth index data)))
			(setq errors (mapcar #'- expected ao))
			(backPropagate errors)
			(print "Details after forward prop")
			 (print neuralNet)
 		)
		(setq valAcc (validate))
		(print valAcc)
		(if (< valAcc Oldacc)
			(if (>= count 3)
				(return)
				(setq count (+ count 1))
			)
			;(if (> count 0)
			;	(setq count 0)
			;)
		)		
		(if (> valAcc 0.93)
			(return)
		)
		(setq Oldacc valAcc)
	)
)

(defun test (testdata)
;to get the output after forward propagation.
	(setq output (forwardPropagate testdata))
	(setq result(getMaxIndex output))
	result
)

(defun trainNeuralNetwork (data)
;to initialize the neural network with all the parameters and call the training on it
	(setq inputLength (length (SplitData-data (nth 0 data))))
	(setq outputLength 10)
	(setq hiddenLength 30)
	(setq itoh (list (+ inputLength 1) hiddenLength))
	(setq htoO (list (+ hiddenLength 1) outputLength))
	(setq weightI (RandomMatrices itoh))
	(setq weightO (RandomMatrices htoO))
	(defVar neuralNet (new-NeuralNetwork inputLength hiddenLength outputLength weightI weightO))
	(defVar ai (make-list (+ inputLength 1)))
	(defVar ah (make-list (+ 1 hiddenLength)))
	(defVar ao (make-list outputLength))
	(defVar lr .3)
	(train data)
	(return-from trainNeuralNetwork neuralNet)
)

(defun testNeuralNetwork (data)
;test neural network and calculate the accuracy with test data
	(setq acc 0)	
	(loop for index from 0 to (- (length data) 1) do
		(setq result (test (SplitData-data (nth index data))))
		(if (= result (SplitData-label (nth index data)))
		(setq acc (+ acc 1))
		)
	)
	(print (/ acc (* (length data) 1.0)))
	(/ acc (* (length data) 1.0))
	
)

(defun split (data)
;to split the data into validation(10000) and training(50000)
	(list (subseq data 0 50000)
			(subseq data 50000)
	)
)

(defun main (traindata testdata)
;Main to invoke all the methods to train and test 
	(setf *random-state* (make-random-state t))
	(setq trainval (getData traindata))
	(setq trainval (split trainval))
	(setq trainingData (first trainval))
	(defVar validation_data (second trainval))
;	(print validation_data)	
	(setq NeuralNetworks (trainNeuralNetwork trainingData))
(print "Final Details:")
(print NeuralNetworks)
	;(print NeuralNetwork)
	(setq testingData (getData testdata))
	(setq result (testNeuralNetwork testingData))
	;(print result)
)



