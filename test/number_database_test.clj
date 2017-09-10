(ns number-database-test
	(:require [clojure.test :refer :all]
		[logical-interpreter :refer :all]))

(def number-database "
	add(zero, zero, zero).
	add(zero, one, one).
	add(zero, two, two).
	add(one, zero, one).
	add(one, one, two).
	add(one, two, zero).
	add(two, zero, two).
	add(two, one, zero).
	add(two, two, one).
	subtract(X, Y, Z) :- add(Y, Z, X).
")

(deftest number-database-fact-test
	(testing "add(one, one, two) should be true"
		(is (= (evaluate-query number-database "add(one, one, two)")
				true)))
	(testing "add(two, one, one) should be false"
		(is (= (evaluate-query number-database "add(two, one, one)")
				false))))

(deftest number-database-rule-test
	(testing "subtract(one, one, two) should be false"
		(is (= (evaluate-query number-database "subtract(one, one, two)")
				false)))
	(testing "subtract(two, one, one) should be true"
		(is (= (evaluate-query number-database "subtract(two, one, one)")
				true))))
