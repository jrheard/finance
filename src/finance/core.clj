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

(defn make-transaction-filter [field values]
  (fn [transaction]
    (some #(re-seq % (get transaction field))
          values)))

(def income? (make-transaction-filter "Category" [#"Income" #"Transfer" #"Paycheck"]))
(def ignorable? (make-transaction-filter "Description" [#"Vanguard" #"Check 7.*" #"Transfer to CREDIT CARD" #"Vgi Prime Mm" #"Vgilifest Gro"]))

(defn credit-card-dupe? [transaction]
  (when ((make-transaction-filter "Account Name" [#"CREDIT CARD"]) transaction)
    (not (= (get transaction "Category")
            "Credit Card Payment"))))

(defn get-income-and-spending []
  (let [transactions (into #{} (filter #(valid-date (% "Date")) (get-transactions)))
        income (into #{} (filter income? transactions))
        spending (clojure.set/difference transactions
                                         income
                                         (into #{} (filter #(or (ignorable? %)
                                                                (credit-card-dupe? %))
                                                           transactions)))]
    [income spending]))

(defn group-transactions
  ([transactions] (group-transactions transactions "Description"))
  ([transactions field]
  (let [grouped (clojure.set/index transactions [field])]
    (into {}
          (map (fn [[k ts]]
                 [(get k field) (reduce + (map #(get % "Amount") ts))])
               grouped)))))

(defn sort-transactions [grouped-transactions]
  (reverse (sort-by (fn [[k v]] v) grouped-transactions)))

(defn sum-transactions [grouped-transactions]
  (reduce + (map second grouped-transactions)))

(defn -main [& args]
  (let [[income spending] (get-income-and-spending)]
    (println (apply -
                    (map (comp sum-transactions group-transactions)
                         [income spending])))))
