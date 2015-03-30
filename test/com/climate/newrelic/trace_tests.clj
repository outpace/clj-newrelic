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
  (let [m (meta #'adds)]
    (testing "metadata"
      (is (= '([] [a] [a b] [a b c & ds]) (:arglists m)))
      (is (= "adds some numbers" (:doc m)))
      (is (class? (::nr/interface m)))
      (is (class? (::nr/type m))))
    (testing "functionality"
      (is (= (adds) 0))
      (is (= (adds 1) 1))
      (is (= (adds 1 2) 3))
      (is (= (adds 1 2 3) 6))
      (is (= (adds 1 2 3 4) 10))
      (is (= (adds 1 2 3 4 5) 15)))
    (testing "trace annotation"
      (let [trace-type (::nr/type m)
            trace-methods (filter #(= "invoke" (.getName %)) (.getDeclaredMethods trace-type))]
        (doseq [method trace-methods]
          (let [annot (.getAnnotation method com.newrelic.api.agent.Trace)]
            (is (not (nil? annot)))
            (is (true? (.dispatcher annot)))
            (is (= (str (ns-name (:ns m)) \. (:name m)) (.metricName annot)))))))))


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
  (let [md (meta #'destructures)]
    (testing "metadata"
      (is (= '([[foo bar] {:keys [baz qux]} & [blah]]) (:arglists md)))
      (is (class? (::nr/interface md)))
      (is (class? (::nr/type md))))
    (testing "functionality"
      (is (= (destructures [1 2] {:baz 3 :qux 4} 5)
             {:foo 1 :bar 2 :baz 3 :qux 4 :blah 5})))
    (testing "trace annotation"
      (let [trace-type (::nr/type md)
            trace-methods (filter #(= "invoke" (.getName %)) (.getDeclaredMethods trace-type))]
        (doseq [method trace-methods]
          (let [annot (.getAnnotation method com.newrelic.api.agent.Trace)]
            (is (not (nil? annot)))
            (is (true? (.dispatcher annot)))
            (is (= (str (ns-name (:ns md)) \. (:name md)) (.metricName annot)))))))))

(nr/defn-traced self-ref
  ([] (self-ref 5))
  ([x] x))

(deftest test-self-ref
  (let [md (meta #'self-ref)]
    (testing "metadata"
      (is (= '([] [x]) (:arglists md)))
      (is (class? (::nr/interface md)))
      (is (class? (::nr/type md))))
    (testing "functionality"
      (is (= (self-ref 7) 7))
      (is (= (self-ref) 5)))
   (testing "trace annotation"
      (let [trace-type (::nr/type md)
            trace-methods (filter #(= "invoke" (.getName %)) (.getDeclaredMethods trace-type))]
        (doseq [method trace-methods]
          (let [annot (.getAnnotation method com.newrelic.api.agent.Trace)]
            (is (not (nil? annot)))
            (is (true? (.dispatcher annot)))
            (is (= (str (ns-name (:ns md)) \. (:name md)) (.metricName annot)))))))))
