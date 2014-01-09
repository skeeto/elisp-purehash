;;; purehash.el --- pure Elisp hash tables -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <wellons@nullprogram.com>

;;; Commentary:

;; This is a hash table implementation in pure Emacs Lisp with the
;; same set of functions. It's about ten times slower than the native
;; hash table. It continues to use `sxhash', which is probably why
;; it's as fast as it is.

;; Differences:
;; * Any comparison function is valid for :test.
;; * `purehash-map' (i.e. `maphash') collects and returns its results.
;; * No special support in `cl-loop'.
;; * For Emacs versions earlier than 23.3, it's readable.

;;; Code:

(require 'cl-lib)

(cl-defstruct (purehash (:constructor purehash--create))
  "A pure Elisp hash table implementation."
  test count rehash-size rehash-threshold -vector)

(cl-defun purehash-create (&key size test rehash-size rehash-threshold)
  "Create a new purehash table.
Has the same parameters and defaults as `make-hash-table' except
for :weakness. Also, :test can be any comparison function."
  (purehash--create :count 0
                    :-vector (make-vector (or size 65) nil)
                    :test (or test #'eql)
                    :rehash-size (or rehash-size 1.5)
                    :rehash-threshold (or rehash-threshold 0.8)))

(defun purehash-size (table)
  "Return the target capacity of TABLE."
  (length (purehash--vector table)))

(defun purehash--lookup (key table)
  "Return (index . key-pair-cons) for KEY in TABLE."
  (let* ((vector (purehash--vector table))
         (test (purehash-test table))
         (index (mod (sxhash key) (length vector))))
    (cons index (cl-assoc key (aref vector index) :test test))))

(defun purehash-get (key table &optional default)
  "Look up KEY in TABLE, defaulting to DEFAULT if not present."
  (or (cddr (purehash--lookup key table))
      default))

(defun purehash--threshold-p (table)
  "Return non-nil if TABLE is over threshold."
  (> (/ (purehash-count table) 1.0 (purehash-size table))
     (purehash-rehash-threshold table)))

(defun purehash-put (key value table)
  "Store VALUE in TABLE under KEY."
  (let* ((vector (purehash--vector table))
         (test (purehash-test table))
         (index (mod (sxhash key) (length vector)))
         (cons (cl-assoc key (aref vector index) :test test)))
    (prog1 value
      (cl-incf (purehash-count table))
      (if cons
          (setf (cdr cons) value)
        (push (cons key value) (aref vector index))
        (when (purehash--threshold-p table)
          (purehash--rehash table))))))

(gv-define-setter purehash-get (value key table)
  `(purehash-put ,key ,value ,table))

(defun purehash-rem (key table)
  "Remove entry for KEY from TABLE."
  (let* ((lookup (purehash--lookup key table))
         (index (car lookup))
         (cons (cdr lookup))
         (vector (purehash--vector table)))
    (when cons
      (let ((slot (aref vector index)))
        (prog1 t
          (cl-decf (purehash-count table))
          (setf (aref vector index) (delete cons slot)))))))

(defun purehash-clr (table)
  "Remove all entries from TABLE."
  (let ((vector (purehash--vector table)))
    (setf (purehash-count table) 0)
    (prog1 table  ; Emacs bug workaround (bug#16206)
      (dotimes (i (length vector))
        (setf (aref vector i) nil)))))

(defun purehash-map (function table)
  "Apply FUNCTION to each key and value in TABLE, collecting the results."
  (cl-loop for slot across (purehash--vector table) nconc
           (cl-loop for (key . value) in slot collect
                    (funcall function key value))))

(defun purehash--rehash (table)
  "Grow TABLE according to its rehash-size."
  (let* ((rehash-size (purehash-rehash-size table))
         (old-size (purehash-size table))
         (new-size (if (integerp rehash-size)
                       (+ rehash-size old-size)
                     (round (* rehash-size old-size))))
         (new-vector (make-vector new-size nil))
         (pairs (purehash-map #'cons table)))
    (setf (purehash--vector table) new-vector
          (purehash-count table) 0)
    (cl-loop for (key . value) in pairs
             do (setf (purehash-get key table) value)
             finally (return table))))

(defun purehash-to-hash-table (purehash)
  "Return a native hash table with the same contents as PUREHASH."
  (let ((hash-table (make-hash-table :test (purehash-test purehash))))
    (prog1 hash-table
      (purehash-map (lambda (k v) (setf (gethash k hash-table) v)) purehash))))

(defun purehash-from-hash-table (hash-table)
  "Return a native hash table with the same contents as PUREHASH."
  (let ((purehash (purehash-create :test (hash-table-test hash-table))))
    (prog1 purehash
      (cl-loop for key hash-keys of hash-table using (hash-value value)
               do (setf (purehash-get key purehash) value)))))

(provide 'purehash)

;;; purehash.el ends here
