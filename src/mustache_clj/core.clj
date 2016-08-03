(ns mustache-clj.core (:gen-class) (:use clojure.test))

(let [brackets     (into #{} "{}")
      bracket-vals {"{{" 1 "}}" -1}]
  (defn render [template data]
    (let [tokens (map (partial apply str) (partition-by (partial contains? brackets) template))
          counts (reductions + (map #(get bracket-vals % 0) tokens))
          paired (filter #(not (contains? bracket-vals (:token %)))
                   (map #(hash-map :token %1 :bracket %2) tokens counts))
          result (apply str (map #(if (= 0 (:bracket %))
                                     (:token %)
                                     (get data (keyword (:token %)) "")) paired))]
      result)))

; From https://github.com/fhd/clostache/blob/master/test/clostache/test_parser.clj
(deftest test-render-simple
  (is (= "Hello, Felix!" (render "Hello, {{name}}!" {:name "Felix"}))))

(run-tests)
