(ns sudoku (:use clojure.set))

(def values (set (range 1 10)))

(defn is-empty [n] (== n 0))
(def numbers (set (range 1 10)))

(defn index-of [e coll] (first (keep-indexed #(if (= e %2) %1) coll)))

;; Helper functions for sets
(defn make-set [coll] (disj (set coll) 0))
(defn value-m-n [matrix m n] (nth (nth matrix m) n))
(defn replace-m-n [matrix m n val] (assoc matrix m (assoc (nth matrix m) n val)))
(value-m-n matrix 1 0)
(replace-m-n matrix 0 1 5)

;; Helper functions for finding current valid values for fields
(defn horizontal-set [matrix m] (make-set (nth matrix m)))

(defn vertical-set
  ([matrix m] (make-set (vertical-set matrix m ())))
  ([matrix m acc] (if
                   (empty? matrix) acc
                   (vertical-set (rest matrix) m (cons (nth (first matrix) m) acc)))))

(defn square-set
  ([matrix row col] (square-set matrix (- row (mod row 3)) (- col (mod col 3)) 0 ()))
  ([matrix startRow startCol counter acc] (if
                                           (== counter 3) (make-set acc)
                                           (square-set matrix
                                                       (inc startRow)
                                                       startCol
                                                       (inc counter)
                                                       (conj (conj (conj acc (value-m-n matrix startRow startCol)) (value-m-n matrix startRow (inc startCol))) (value-m-n matrix startRow (inc (inc startCol))))))))

;; Validates if value can be inserted in position m,n
(defn validate [matrix m n value]
  (not (or
        (contains? (square-set matrix m n) value)
        (or
         (contains? (horizontal-set matrix m) value)
         (contains? (vertical-set matrix n) value)))))

;; Searches for a valid value for a position in the matrix, and replaces it with it
(defn find-value [matrix m n val]
  (cond
    (== val 10) false
    (validate matrix m n val) (or (solve (replace-m-n matrix m n val) m (inc n)) (find-value matrix m n (inc val)))
    :else (find-value matrix m n (inc val))))

;; Solve puzzle
(defn solve
  ([matrix] (solve matrix 0 0))
  ([matrix row column] (cond
                         (and (== 8 row) (== column 9)) matrix
                         (= 9 column) (solve matrix (inc row) 0)
                         (not= (value-m-n matrix row column) 0) (solve matrix row (inc column))
                         :else (find-value matrix row column 1))))

;; Returns all the available values for that position
(defn find-values [matrix row column]
  (difference values (set (concat (vertical-set matrix column) (concat (horizontal-set matrix row) (square-set matrix row column))))))

(defn replace-values [matrix row column]
  (replace-m-n matrix row column (find-values matrix row column)))
(replace-values matrix 0 2)

;; Get possible values for each empty position
(defn transform
  ([matrix] (transform matrix 0 0))
  ([matrix row column] (cond
                         (and (== 8 row) (== column 9)) matrix
                         (= 9 column) (transform matrix (inc row) 0)
                         (not= (value-m-n matrix row column) 0) (transform matrix row (inc column))
                         :else (transform (replace-values matrix row column) row (inc column)))))
(solve matrix)
(run-tests)