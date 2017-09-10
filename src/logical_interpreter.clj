(ns logical-interpreter)
(require '[clojure.string :as str])

(defn has-a-rule 
	[database]
	(.contains database ":-")
)

(defn is-query-valid
	[query]
	(.contains query "(")
)

(defn is-query-in-database
	[database query]
	(.contains database query)
)

(defn read-parent-rule
	[rule-facts first-rule query database]
	(def open-parentesis-index (.indexOf query "("))
	(def coma-index (.indexOf query ","))
	(def first-query-param (subs query (+ open-parentesis-index 1) coma-index))
	(def first-fact-to-search (str first-rule "(" first-query-param ")"))
	(def is-first-fact-valid (.contains database first-fact-to-search))
	
	(def second-rule-fact (subs rule-facts coma-index))
	(def first-char (subs second-rule-fact 0 1))
	(if (= first-char " ")
		(def second-rule-fact (subs second-rule-fact 1))
	)
	(def open-parentesis-index (.indexOf second-rule-fact "("))
	(def second-rule (subs second-rule-fact 0 open-parentesis-index))
	(def second-query-param (subs query (+ coma-index 2) (- (.length query) 1)))
	(def second-fact-to-search (str second-rule "(" second-query-param ", " first-query-param ")"))
	(def is-second-fact-valid (.contains database second-fact-to-search))
	(def valid-query (and is-first-fact-valid is-second-fact-valid))
	(if valid-query
		1
		0
	)
)

(defn read-number-rule
	[database-line rule-facts first-rule query database]
	(def open-parentesis-index (.indexOf database-line "("))
	(def database-rule (subs database-line 0 open-parentesis-index))
	(def open-parentesis-index (.indexOf query "("))
	(def query-rule (subs query 0 open-parentesis-index))
	(def is-a-rule (.contains database-rule query-rule))

	(def open-parentesis-index (.indexOf query "("))
	(def coma-index (.indexOf query ","))
	(def first-query-param (subs query (+ open-parentesis-index 1) coma-index))

	(def second-part-of-query (subs query (+ coma-index 1)))
	(def second-coma-index (.indexOf second-part-of-query ","))
	(def second-query-param (subs second-part-of-query 1 second-coma-index))

	(def third-part-of-query (subs second-part-of-query (+ second-coma-index 1)))
	(def third-query-param (subs third-part-of-query 1 (- (.length third-part-of-query) 1)))
	(def fact-to-search (str first-rule "(" second-query-param ", " third-query-param ", " first-query-param ")"))
	(def is-fact-valid (.contains database fact-to-search))

	(if (and is-fact-valid is-a-rule)
		1
		0
	)
)

(defn read-rule
	[rule query database]
	(def index (.indexOf rule ":- "))
	(def index (+ index 3))
	(def rule-facts (subs rule index))
	(def first-parentesis-index (.indexOf rule-facts "("))
	(def first-rule (subs rule-facts 0 first-parentesis-index))
	(def after-close-parentesis-index (.indexOf rule-facts ")"))
	(def after-first-fact-character (subs rule-facts (+ after-close-parentesis-index 1) (+ after-close-parentesis-index 2)))
	(if (= after-first-fact-character ",")
		(read-parent-rule rule-facts first-rule query database)
		(read-number-rule rule rule-facts first-rule query database)
	)
)

(defn rule-of-query
	[query]
	(def index (.indexOf query "("))
	(subs query 0 index)
)

(defn check-rule
	[database query]
	(def database-vector (str/split database #"\n"))
	(def is-valid 
		(for [database-line database-vector] 
			(if (has-a-rule database-line)
				(if (.contains database-line (rule-of-query query))
					(read-rule database-line query database)
					0
				)
				0
			)
		)
	)
	
	(def result (reduce + is-valid))
	(if (= result 1)
		true
		false
	)
)

(defn evaluate-query
	"Returns true if the rules and facts in database imply query, false if not. If
	either input can't be parsed, returns nil"
	[database query]
	(if (has-a-rule database)
		(if (is-query-valid query)
			(if (is-query-in-database database query)
				true
				(if (.contains query ",")
					(check-rule database query)
					false
				)
			)
			nil
		)
		nil
	)
)
