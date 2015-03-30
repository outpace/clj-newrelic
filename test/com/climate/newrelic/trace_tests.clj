(ns com.climate.newrelic.trace-tests
  (:require [clojure.test :refer :all]
            [clojure.test-helper :as util]
            [com.climate.newrelic.trace :as nr]))

(nr/defn-traced adds
  "adds some numbers"
  ([] 0)
  ([a] a)
  ([a b] (+ a b))
  ([a b c & ds]
    (apply + a b c ds)))

(deftest test-adds
  (testing "metadata"
    (let [m (meta #'adds)]
      (is (= '([] [a] [a b] [a b c & ds]) (:arglists m)))
      (is (= "adds some numbers" (:doc m)))
      (is (class? (::nr/interface m)))
      (is (class? (::nr/type m)))))
  (testing "functionality"
    (is (= (adds) 0))
    (is (= (adds 1) 1))
    (is (= (adds 1 2) 3))
    (is (= (adds 1 2 3) 6))
    (is (= (adds 1 2 3 4) 10))
    (is (= (adds 1 2 3 4 5) 15))))


(deftest test-reflection
  (util/should-not-reflect
    (com.climate.newrelic.trace/defn-traced get-length
      [^java.util.List l]
      (.size l))))

(nr/defn-traced destructures [[foo bar] {:keys [baz qux]} & [blah]]
  {:foo foo
   :bar bar
   :baz baz
   :qux qux
   :blah blah})

(deftest test-destructure
  (testing "metadata"
    (let [md (meta #'destructures)]
      (is (= '([[foo bar] {:keys [baz qux]} & [blah]]) (:arglists md)))
      (is (class? (::nr/interface md)))
      (is (class? (::nr/type md)))))
  (testing "functionality"
    (is (= (destructures [1 2] {:baz 3 :qux 4} 5)
           {:foo 1 :bar 2 :baz 3 :qux 4 :blah 5}))))

(nr/defn-traced self-ref
  ([] (self-ref 5))
  ([x] x))

(deftest test-self-ref
  (testing "metadata"
    (let [md (meta #'self-ref)]
      (is (= '([] [x]) (:arglists md)))
      (is (class? (::nr/interface md)))
      (is (class? (::nr/type md)))))
  (testing "functionality"
    (is (= (self-ref 7) 7))
    (is (= (self-ref) 5))))
