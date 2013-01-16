;; ## A program that tells you how you interact with your money.
;;
;; One of Mint's strengths is that it lets you consolidate all of your
;; financial accounts and information in one place. One of its not-strengths
;; is that it isn't always great at answering the kinds of questions I want to ask
;; about that data. I'm limited to only ever receiving answers to the questions that
;; Mint's engineers get around to answering. This program provides some of those missing answers.
;;
;; The program expects a Mint transactions .csv dump to exist at `resources/transactions.csv`.

(ns finance.core
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.set]
            [clj-time.coerce :refer [to-long]]
            [clj-time.core :refer [date-time]]
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
  "Takes a map of {string -> string}, returns a parsed transaction map."
  [transaction]
  (let [field-parsers {"Amount" #(Float/parseFloat %)
                       "Date"    parse-date}]
    (into transaction
          (map (fn [[k v]] [k (v (get transaction k))]) field-parsers))))

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
  entry for the specified `field` contained one of the specified `values`, nil otherwise."
  [field values]
  (fn [transaction]
    (some #(re-seq % (get transaction field))
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
  lots of other smaller transactions that are also included in the .csv dump."
  [transaction]
  (when ((make-transaction-filter "Account Name" [#"CREDIT CARD"]) transaction)
    (= (get transaction "Category")
       "Credit Card Payment")))

(defn happened-in-2012? [date]
  (apply < (map to-long [(date-time 2012 01 01)
                         date
                         (date-time 2012 12 31)])))

(defn set-of [pred coll]
  (into #{} (filter pred coll)))

(defn filter-income-from-spending
  "Takes a seq of transaction maps and returns a list of `[income spending]`, where
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
  Useful for e.g. grouping all of your Amazon transactions together."
  ([transactions] (group-transactions transactions "Description"))
  ([transactions field]
  (let [grouped (clojure.set/index transactions [field])]
    (into {}
          (map (fn [[k ts]]
                 [(get k field) (reduce + (map #(get % "Amount") ts))])
               grouped)))))

(defn sort-transactions
  "Takes a map of grouped transactions, returns a seq of grouped-transaction k/v pairs in descending order by transaction amount."
  [grouped-transactions]
  (reverse (sort-by (fn [[k v]] v) grouped-transactions)))

(defn sum-transactions
  "Takes a map of grouped transactions, returns the sum of all of the transactions' amounts."
  [grouped-transactions]
  (reduce + (map second grouped-transactions)))

;; ## I/O

(defn get-transactions
  "Returns a seq of parsed transaction maps based on the contents of resources/transactions.csv."
  []
  (let [lines (with-open [f (io/reader (io/resource "transactions.csv"))]
                (doall (csv/read-csv f)))
        header (first lines)
        lines (rest lines)]
    (map parse-transaction
         (reduce #(conj % (zipmap header %2)) [] lines))))

(defn -main [& args]
  (let [[income spending] (filter-income-from-spending (get-transactions))]
    (println (apply -
                    (map (comp sum-transactions group-transactions)
                         [income spending])))))
