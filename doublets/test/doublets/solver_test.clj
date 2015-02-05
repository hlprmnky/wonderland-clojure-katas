(ns doublets.solver-test
  (:require [clojure.test :refer :all]
            [doublets.solver :refer :all]))

(deftest solver-test
  (testing "with word links found"
    (is (= ["head" "heal" "teal" "tell" "tall" "tail"]
           (doublets "head" "tail")))

    (is (= ["door" "boor" "book" "look" "lock"]
           (doublets "door" "lock")))

    (is (= ["bank" "bonk" "book" "look" "loon" "loan"]
           (doublets "bank" "loan")))

    (is (= ["wheat" "cheat" "cheap" "cheep" "creep" "creed" "breed" "bread"]
           (doublets "wheat" "bread"))))

  (testing "with no word links found"
    (is (= []
           (doublets "ye" "freezer")))))

(deftest shared-letters-test
  (testing "finds shared letters by position between two words"
    (is (= [0 0 0 0] (shared-letters "abcd" "efgh")))
    (is (= [1 0 0 0] (shared-letters "abcd" "aefg")))
    (is (= [1 0 1 0] (shared-letters "abcd" "aecg")))
    (is (= [1 1 1 1] (shared-letters "abcd" "abcd")))
    (is (= [] (shared-letters "abcd" "abcde")))
    (is (= [0 1 0 0] (shared-letters "abcd" "dbac")))))

(deftest find-neighbors-test
  (let [word-list ["zzzz" "abcd" "abce" "" "abc" "abcde" "bbcd" "abed"]]
    (testing "finds neighbors in a search space"
      (is (= #{"abce" "abed" "bbcd"} (into #{} (find-neighbors "abcd" word-list)))))))

(deftest new-neighbors-test
  (let [word-list ["zzzz" "abcd" "abce" "" "abc" "abcde" "bbcd" "abed"]
        seen #{"abed"}]
    (testing "returns only neighbors we haven't met yet"
      (is (= #{"abce" "bbcd"} (into #{} (new-neighbors "abcd" word-list seen)))))))
