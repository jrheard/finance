;; ## A program that tells you how you interact with your money.
;;
;; One of Mint's strengths is that it lets you consolidate all of your
;; financial accounts and information in one place. One of its not-strengths
;; is that it isn't always great at answering the kinds of questions I want to ask
;; about that data. I'm limited to only ever receiving answers to the questions that
;; Mint's engineers get around to answering. This program provides some of those missing answers.
;;
;; The program expects a Mint transactions .csv dump to exist at `resources/transactions.csv`.
;;

(ns finance.core
  "A Clojure source file starts with an `ns` declaration. This file's namespace is `finance.core`
   because it's in src/finance/core.clj, so we declare that at the start of the declaration. After that,
   there's a spot for an optional docstring, which it's a good idea to always include.
   From there on, you've got your imports. When writing your programs, prefer to import libraries using `:require`,
   as opposed to `:use`, which by default behaves like `import *`.

   Within a `:require` directive, the `:refer` directive tells Clojure specifically which functions you'd
   like to import into your namespace to be made available via unqualified symbols; you can always access
   the entire namespace's collection of public vars via e.g.  `clj-time.coerce/to-date`, where the part
   before the slash is the name of the namespace you're conerned with, and the part after the slash is
   the var you're interested in. When I say \"unqualified symbols\", I mean that a directive like
   `(:require [foo.bar :refer [baz]])` will make the var `baz` available via the symbol `baz`,
   as opposed to making you have to type `foo.bar/baz`.

   The `:as` directive does what you probably expect; if we had done `(:require [foo.bar :as bar])`, then the namespace's
   vars would be available by typing e.g. `bar/baz`."
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.set]
            [clj-time.core :refer [date-time year]]
            [clj-time.format :refer [parse formatter]]))

;; ## Parsers
;; Transactions come in as maps like this:
;;
;; `{"Account Name" "CREDIT CARD", "Category" "Pharmacy", "Amount" "17.39", "Descripton" "Walgreens", "Date" "1/11/2013"}`
;;
;; These parsers help transform those stringy maps into maps with more usefully typed data.

(defn parse-date
  "Takes a string of the format '1/11/2013', returns a DateTime instance."
  [s]
  (parse (formatter "MM/dd/yyyy") s))

(defn parse-transaction
  "Takes a map of {string -> string}, returns a parsed transaction map.
  A call to `(into a-map [[foo bar] [baz quux]])` is basically equivalent to a non-side-effecting version of
  `a_dict.update({foo: bar, baz: quux})` from python. So in the code above, we're saying: take a map called
  `transaction` and, for each k/v pair in `field-parsers`, run the map's corresponding value through the relevant
  `field-parsers` function. That is: if an input map looks like `{\"Amount\" \"3.0\"}`, we'll run `Float/parseFloat` on
  that value and our updated map will look like `{\"Amount\" 3.0}`.

  This anonymous function that we're mapping over `field-parsers` is using destructuring: it takes one argument, which
  is a key-value pair represented by a list with two items, and pulls that list apart via the syntax `[k v]`. So within
  body of the anonymous function, `k` refers to a key from `field-parsers`, and `v` refers to the corresponding value.
  If you ever find yourself reaching for the Clojure equivalent of looping over `a_dict.iteritems()`, this style of anonymous
  function is probably what you're looking for."
  [transaction]
  (let [field-parsers {"Amount" #(Float/parseFloat %)
                       "Date"    parse-date}]
    (into transaction
          (map (fn [[k v]] [k (v (transaction k))])
               field-parsers))))


;; ## Filters
;; The .csv dump that Mint gives us contains a bunch of transactions.
;; Some of them are duplicates - for instance, transactions under the category
;; "Credit Card Payment" in the account "CREDIT CARD" represent lump credit card payments,
;; and basically represent bundles of all transactions in the "CREDIT CARD" account that aren't
;; marked as "Credit Card Payment".
;;
;; Also, some transactions represent money that's just been moved from checking to an
;; investment account, and don't mean that I've just thrown a few thousand bucks away.
;; So when we're calculating spending, we probably won't want to include investment-related transactions.

(defn make-transaction-filter
  "Takes a `field` string that's a key into a transaction map and a seq of `values` that represent possible values
  that might appear in a transaction map; returns a fn that takes a `transaction` map and returns truthy if its
  entry for the specified `field` contained one of the specified `values`, nil otherwise.

  Whereas in Python I might have defined a Validator class with defined behavior and had IncomeValidator
  and IgnorableValidator subclass it with specific data, in Clojure the obvious solution is instead
  to just use higher-order functions. As a result, this is a function that returns a function, and the returned
  function is a predicate that takes a map and returns true if it meets the specified criteria, false otherwise."
  [field values]
  (fn [transaction]
    (some #(re-seq % (transaction field))
          values)))

(def income? (make-transaction-filter "Category" [#"Income" #"Transfer" #"Paycheck"]))

(def ignorable?  (make-transaction-filter "Description" [#"Vanguard"
                                                         #"Check 7.*"
                                                         #"Transfer to CREDIT CARD"
                                                         #"Vgi Prime Mm"
                                                         #"Vgilifest Gro"]))

(defn credit-card-dupe?
  "Takes a transaction map and returns `true` if it's a credit card payment, falsy otherwise.
  Those payments are considered as duplicate transactions, since they're rollups of
  lots of other smaller transactions that are also included in the .csv dump.

  This function has to perform some more advanced checking than `income?` and `ignorable?` do,
  because it has to first see if the transaction it's given is from the CREDIT CARD account,
  and then it has to see if its category is \"Credit Card Payment\".

  Whereas in Python we might have defined another subclass of Validator, here instead we just compose
  the return values of a couple of higher-order functions. `juxt` is a function that takes a sequence
  of functions and then returns a function that, when called, returns a vector containing the results
  of applying each of those functions to the given input.

  So for example: if transaction is from the CREDIT CARD account but isn't a payment, the call to the
  function that `juxt` returned will evaulate to `[true false]`, and the `every?` call will then be falsy,
  indicating that this transaction wasn't a credit card dupe. Simple!"
  [transaction]
  (every? identity ((juxt (make-transaction-filter "Account Name" [#"CREDIT CARD"])
                          (make-transaction-filter "Category" [#"Credit Card Payment"]))
                    transaction)))

(defn happened-in-2012? [date]
  (= (year date) 2012))

(defn set-of
  "I've found this function to be really useful whenever I have a vector of values and want to end up
  with the set of all of those values that meet some criteria.

  E.g.: `(set-of even? [1 2 3 4 5 6 3 4 2]))`"
  [pred coll]
  (into #{} (filter pred coll)))

(defn filter-income-from-spending
  "Takes a seq of transaction maps and returns a vector of `[income spending]`, where
  `income` is a seq of transactions in which I was given money in 2012, and
  `spending` is a seq of transactions in which I spent money in 2012."
  [transactions]
  (let [transactions (set-of #(happened-in-2012? (% "Date")) transactions)
        income (set-of income? transactions)
        spending (clojure.set/difference transactions
                                         income
                                         (set-of #(or (ignorable? %)
                                                      (credit-card-dupe? %))
                                                 transactions))]
    [income spending]))

(defn group-transactions
  "Takes a seq of transactions and an optional string `field` (\"Description\" by default),
  returns a map of `{field-value -> transaction-amount}`, where `transaction-amount` is the sum
  of the corresponding amounts for all `transactions` that shared the same `field-value`.
  Useful for e.g. grouping all of your Amazon transactions together.

  This is the first function that we've seen that takes advantage of the fact that functions can
  define multiple arities. There's a version of group-transactions that takes only one parameter,
  and a version that takes two. You'll notice that the one-parameter version just fills in a default value
  and delegates to the two-parameter version; this is a pretty common Clojure idiom for specifying default
  values for parameters.

  The first thing this function does is that it calls `clojure.set/index`, which is pretty similar to
  `yelp_lib.grouping.group_by_key`. At that point, `grouped` looks like
  `{{\"Description\" \"Amazon\"} [list-of-amazon-transactions]}`, etc.

  After that, we basically just map over `grouped` to generate a series of 2-item lists like `[\"Amazon\" 350.10]`,
  and then we stuff all of those 2-item lists into an empty map. Our return value is a map of the form
 `{\"Amazon\" 350.10}`. "
  ([transactions] (group-transactions transactions "Description"))
  ([transactions field]
  (let [grouped (clojure.set/index transactions [field])]
    (into {}
          (map (fn [[k transactions]]
                 [(k field) (reduce + (map #(get % "Amount") transactions))])
               grouped)))))

(defn sort-transactions
  "Takes a map of grouped transactions, returns a seq of grouped-transaction k/v pairs in descending order by transaction amount.

  Here we're doing the same thing from earlier, where we interact with a map's keys and values by mapping an anonymous function over
  it and using destructuring to get at each key and value. Our return value looks like `[[\"Amazon\" 350.10] [\"Postmates\" 123.45]]`."
  [grouped-transactions]
  (reverse (sort-by (fn [[k v]] v) grouped-transactions)))

(defn sum-transactions
  "Takes a map of grouped transactions, returns the sum of all of the transactions' amounts."
  [grouped-transactions]
  (reduce + (map second grouped-transactions)))

;; ## I/O

(defn read-csv [filename]
  (with-open [f (io/reader (io/resource filename))]
    (doall (csv/read-csv f))))

(defn get-transactions
  "Returns a seq of parsed transaction maps based on the contents of resources/transactions.csv.

  The most interesting thing that's going on here is that we're assembling a vector by using the `reduce` function.
  `reduce` takes a function, an optional initial value, and a collection `coll` to operate on. The specified function
  has to take two parameters and return a value; the first parameter is an \"accumulator\" that basically represents
  \"what we've got so far\", and the second parameter is a value of `coll` that we want to somehow update the accumulator with.

  Here, we're just starting with the empty list and using the function `conj` (for \"conjoin\") to append a new item on to the accumulator.
  That's how you assemble a list in immuability-land; not too complicated.

  The list we're assembling contains values that look like `(zipmap (first lines) another-line)`. `zipmap`'s just a function that
  takes two vectors and returns a map, e.g. `(zipmap [1 2 3] [:a :b :c])` yields `{1 :a 2 :b 3 :c}`. We're using it here to
  turn each line of the .csv into a Clojure map, where the keys are values from the header (the first line of the .csv) and the
  values are the actual values each line contains."
  []
  (let [lines (read-csv "transactions.csv")]
    (map parse-transaction
         (reduce #(conj % (zipmap (first lines) %2))
                 []
                 (rest lines)))))

(defn -main [& args]
  (let [[income spending] (filter-income-from-spending (get-transactions))]
    (println (apply -
                    (map (comp sum-transactions group-transactions)
                         [income spending])))))
