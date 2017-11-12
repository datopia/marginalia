(ns marginalia.test.parse
  "This module does stuff"
  (:use clojure.test)
  (:require [marginalia.parser :as p]))

(deftest test-inline-literals
  (is (= (count (marginalia.parser/parse "(ns test)")) 1))
  ;; (is (= (count (marginalia.parser/parse "(ns test)\n123")) 1)) ;; still failing
  (is (= (count (marginalia.parser/parse "(ns test)\n123\n")) 1))
  (is (= (count (marginalia.parser/parse "(ns test)\n\"string\"")) 1))
  (is (= (count (marginalia.parser/parse "(ns test)\n\"some string\"")) 1))
  (is (= (count (marginalia.parser/parse "(ns test (:require [marginalia.parser :as parser]))\n(defn foo [] ::parser/foo)")) 1)))


(def simple-fn
  "(defn some-fn
  \"the docstring\"
  [x]
  (* x x))")

(deftest test-parse-fn-docstring
  (let [{:keys [type docstring]} (first (marginalia.parser/parse simple-fn))]
    (is (= :code type))
    (is (= "the docstring" docstring))))

(deftest inline-comments
  (testing "inline comments ignored by default"
    (binding [p/*comments-enabled* (atom true)]
      (let [result (p/parse
                     "(ns test)

                     (defn foo
                       \"docstring\"
                       []
                       (let [x 1]
                         ;; A
                         x))")]
        (is (= 2 (count result)))
        (is (= "docstring" (:docstring (second result))))))

    (binding [p/*comments-enabled* (atom true)]
      ;; tests that prelude is appended to docstring
      (let [result (p/parse
                     "(ns test)

                     ;; A
                     (defn foo
                       \"docstring\"
                       []
                       (let [x 1]
                         ;; B
                         x))")]
        (is (= 2 (count result)))
        (is (= "docstring\n\nA" (:docstring (second result)))))))

  (testing "inline comments added to docstring as paragraphs"
    (binding [p/*comments-enabled* (atom true)
              p/*lift-inline-comments* true]
      (let [result (p/parse
                     "(ns test)

                     (defn foo
                       \"docstring\"
                       []
                       (let [x 1]
                         ;; A
                         x))")]
        (is (= 3 (count result)))
        (is (= "docstring\n\n\nA" (:docstring (second result))))))

    (binding [p/*comments-enabled* (atom true)
              p/*lift-inline-comments* true]
      ;; A and B should be separate paragraphs
      (let [result (p/parse
                     "(ns test)

                     (defn foo
                       \"docstring\"
                       []
                       ;; A
                       (let [x 1]
                         ;; B
                         x))")]
        (is (= 3 (count result)))
        (is (= "docstring\n\n\nA\n\nB" (:docstring (second result)))))))

  (testing "inline comments added to prelude after docstring"
    (binding [p/*comments-enabled* (atom true)
              p/*lift-inline-comments* true]
      ;; prelude A follows docstring, then B and C as separate paragraphs
      (let [result (p/parse
                     "(ns test)

                     ;; A
                     (defn foo
                       \"docstring\"
                       []
                       ;; B
                       (let [x 1]
                         ;; C
                         x))")]
        (is (= 3 (count result)))
        (is (= "docstring\n\nA\n\nB\n\nC" (:docstring (second result))))))

    (binding [p/*comments-enabled* (atom true)
              p/*lift-inline-comments* true]
      ;; this checks that consecutive comment lines stay in the same paragraph
      (let [result (p/parse
                     "(ns test)

                     ;; A
                     (defn foo
                       \"docstring\"
                       []
                       ;; B
                       ;; C
                       (let [x 1]
                         ;; D
                         x))")]
        (is (= 3 (count result)))
        (is (= "docstring\n\nA\n\nB\nC\n\nD" (:docstring (second result))))))

    (binding [p/*comments-enabled* (atom true)
              p/*lift-inline-comments* true]
      ;; this checks that a comment above the function doesn't merge in
      ;; when separated by a blank line
      (let [result (p/parse
                     "(ns test)

                     ;; A

                     (defn foo
                       \"docstring\"
                       []
                       ;; B
                       ;; C
                       (let [x 1]
                         ;; D
                         x))")]
        (is (= 4 (count result)))
        (is (= "A" (:raw (second result))))
        (is (= "docstring\n\n\nB\nC\n\nD" (:docstring (nth result 2))))))

    (binding [p/*comments-enabled* (atom true)
              p/*lift-inline-comments* true]
      ;; this checks that a comment above the function does merge in
      ;; when a blank comment joins it to the function
      (let [result (p/parse
                     "(ns test)

                     ;; A
                     ;;
                     (defn foo
                       \"docstring\"
                       []
                       ;; B
                       ;; C
                       (let [x 1]
                         ;; D
                         x))")]
        (is (= 3 (count result)))
        (is (= "docstring\n\nA\n\n\nB\nC\n\nD" (:docstring (second result))))))))
