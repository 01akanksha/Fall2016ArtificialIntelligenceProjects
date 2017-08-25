save the neunet.lisp and mnist_train.csv and mnist_test.csv at ada.cs.pdx.edu

to run -
 clisp -q -q -on-error abort -x '(progn (load "neunet") (main "mnist_train.csv" "mnist_test.csv") (quit))'

output takes long to start displaying..