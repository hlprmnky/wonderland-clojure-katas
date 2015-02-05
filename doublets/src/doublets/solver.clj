(ns doublets.solver
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def words (-> "words.edn"
               (io/resource)
               (slurp)
               (read-string)))

(defn shared-letters
  "Given two strings of equal length, returns a bitfield showing letters which match
  (shared-letters \"abcd\" \"abce\")
  => [1 1 1 0]"
  [one two]
  (if (= (.length one) (.length two))
    (vec (map #(if (= %1 %2) 1 0)
              (.toCharArray one)
              (.toCharArray two)))
    []))

(defn neighbor?
  [letters]
  (= 1 (count (filter zero? letters))))

(defn find-neighbors
  "Given a word and a list of words, find the 'neighbors' of the word in the list"
  [word list]
  (let [search word
        my-map (zipmap (map #(shared-letters search %) list) list)
        my-keys (filter neighbor? (keys my-map))]
    (into [] (map #(get my-map %) my-keys))))

(defn new-neighbors
  "Filter from neighbors of word in word-list elements we've seen"
  [word word-list seen]
  (filter #(not (some #{%} seen)) (find-neighbors word word-list)))

(defn check-neighbors
  "Walk the graph of new-neighbors until we find target or find we can't go further"
   [target path list seen]
   (if (empty? (filter #(not (some #{%} seen)) list))
       []
       (if (some #{target} list)
           (conj path target)
           (map #(check-neighbors
             target
             (conj path %)
             (new-neighbors % words seen)
             (conj seen %)) list))))

(defn doublets
  "Blow stack and die in a beautiful pyre, or maybe just return the path from word1 to word2"
  [word1 word2]
  (if (= (.length word1) (.length word2))
      (flatten (check-neighbors word2 [word1] (find-neighbors word1 words) #{word1}))
      []))
