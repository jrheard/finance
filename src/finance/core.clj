(ns finance.core
  "Tells me how much money I earned/spent in 2012."
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.set]
            [clj-time.coerce :refer [to-long]]
            [clj-time.core :refer [date-time]]
            [clj-time.format :refer [parse formatter]]))

(defn parse-date
  "Takes a string like '1/11/2013', returns a DateTime instance."
  [date-str]
  (parse (formatter "MM/dd/yyyy") date-str))

(defn parse-transaction
  "Takes a map of {string -> string}, returns a parsed transaction map."
  [transaction]
  (let [field-parsers {"Amount" #(Float/parseFloat %)
                       "Date"    parse-date}]
    (into transaction
          (map (fn [[k v]] [k (v (get transaction k))]) field-parsers))))

(defn get-transactions []
  (let [lines (with-open [f (io/reader (io/resource "transactions.csv"))]
                (doall (csv/read-csv f)))
        header (first lines)
        lines (rest lines)]
    (map parse-transaction
         (reduce #(conj % (zipmap header %2)) [] lines))))

(defn valid-date
  "Returns true if the specified date happened in 2012, false otherwise."
  [date]
  (apply < (map to-long [(date-time 2012 01 01) date (date-time 2012 12 31)])))

(def income-categories #{"Income", "Transfer", "Paycheck"})

(defn get-income-and-spending []
  (let [transactions (into #{} (filter #(valid-date (% "Date")) (get-transactions)))
        income (into #{} (filter #(income-categories (get % "Category")) transactions))
        spending (clojure.set/difference transactions income)]
    [income spending]))

(defn group-transactions
  ([transactions] (group-transactions transactions "Description"))
  ([transactions field]
  (let [grouped (clojure.set/index transactions [field])]
    (into {}
          (map (fn [[k ts]]
                 [(get k field) (reduce + (map #(get % "Amount") ts))])
               grouped)))))

(defn -main [& args]
  (let [[income spending] (get-income-and-spending)]
    (reverse (sort-by (fn [[k v]] v) (group-transactions income)))))
