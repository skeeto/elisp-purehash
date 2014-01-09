;;; purehash-tests.el -- tests for purehash

;;; Commentary:

;;; Code:

(require 'ert)
(require 'purehash)

(ert-deftest purehash-retrieval ()
  (let ((table (purehash-create :test #'eq)))
    (setf (purehash-get :a table) 0
          (purehash-get :b table) 1
          (purehash-get :c table) 2
          (purehash-get :d table) 3)
    (should (= (purehash-get :b table :nothing) 1))
    (should (= (purehash-get :d table :nothing) 3))
    (should (= (purehash-get :c table :nothing) 2))
    (should (= (purehash-get :a table :nothing) 0))))

(ert-deftest purehash-clr ()
  (let ((table (purehash-create)))
    (setf (purehash-get :a table) 0
          (purehash-get :b table) 0)
    (should (= (purehash-count table) 2))
    (should (eq (purehash-clr table) table))
    (should (= (purehash-count table) 0))
    (should (eq (purehash-get :a table :empty) :empty))
    (should (eq (purehash-get :b table :empty) :empty))))

(ert-deftest purehash-test ()
  (let ((table (purehash-create :test #'string=)))
    (setf (purehash-get "foo" table) :foo
          (purehash-get "bar" table) :bar
          (purehash-get "baz" table) :baz)
    (should (eq (purehash-get "foo" table) :foo))
    (should (eq (purehash-get "bar" table) :bar))
    (should (eq (purehash-get "baz" table) :baz))))

(ert-deftest purehash-map ()
  (let ((table (purehash-create :test #'equal)))
    (setf (purehash-get "foo" table) :foo
          (purehash-get "bar" table) :bar
          (purehash-get "baz" table) :baz)
    (let ((pairs (purehash-map #'list table)))
      (should (equal (cl-sort pairs #'string< :key #'cl-first)
                     '(("bar" :bar) ("baz" :baz) ("foo" :foo)))))))

(ert-deftest purehash-rehash ()
  (let ((table (purehash-create :size 4 :rehash-size 4 :rehash-threshold 0.8)))
    (should (= (purehash-size table) 4))
    (setf (purehash-get :a table) 0
          (purehash-get :b table) 1
          (purehash-get :c table) 2
          (purehash-get :d table) 3)
    (should (= (purehash-size table) 8))
    (cl-incf (purehash-get :a table))
    (cl-incf (purehash-get :b table))
    (cl-incf (purehash-get :c table))
    (cl-incf (purehash-get :d table))
    (should (= (purehash-size table) 8))
    (setf (purehash-get :e table) 4
          (purehash-get :f table) 5
          (purehash-get :g table) 6
          (purehash-get :h table) 7)
    (should (= (purehash-size table) 12))))

(defvar purehash-test-random-state [cl-random-state-tag -1 30 267466518]
  "Use the same random state for each run.")

(defmacro purehash-test-measure-time (&rest body)
  "Measure and return the running time of the code block."
  (declare (indent defun))
  (let ((start (make-symbol "start")))
    `(let ((,start (float-time)))
       ,@body
       (- (float-time) ,start))))

(cl-defun purehash-benchmark-pure (&optional (n 25000))
  "Perform a benchmark on a purehash table."
  (purehash-test-measure-time
    (let ((table (purehash-create)))
      (cl-loop for i from 0 to n
               do (setf (purehash-get i table) i))
      (cl-loop for i from 0 to n
               do (purehash-get i table))
      (purehash-size table))))

(cl-defun purehash-benchmark-native (&optional (n 25000))
  "Perform the same benchmark on a native hash table."
  (purehash-test-measure-time
    (let ((table (make-hash-table)))
      (cl-loop for i from 0 to n
               do (setf (gethash i table) i))
      (cl-loop for i from 0 to n
               do (gethash i table))
      (hash-table-size table))))

(defun purehash-benchmark ()
  "Run a benchmark between purehash tables and native hash tables."
  (interactive)
  (cl-flet ((f (x) (format "%0.4f" x)))
    (let* ((pure (purehash-benchmark-pure))
           (native (purehash-benchmark-native))
           (result (list :pure (f pure)
                         :native (f native)
                         :ratio (f (/ pure native)))))
      (princ (format "%S\n" result))
      result)))

(provide 'purehash-tests)

;;; purehash-tests.el ends here
