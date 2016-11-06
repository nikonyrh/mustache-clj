(ns mustache-clj.core (:gen-class)
  (:use clojure.test)
  (:require [clojure.string :as str]))

(let [bracket-chars {\{ 1 \} 2}
      bracket-vals  {"{{" 1 "}}" -1 "{{{" 10 "}}}" -10}
      bracket-kws   (set (map keyword (keys bracket-vals)))
      bracket-types {0 :const 1 :reference 10 :reference \# :path-start \/ :path-end}]
  (defn lexer [template]
    ; Split a string to tokens, group by brackets, determine their types
    (let [template       (str/replace template #"\{\{&(.*?)\}\}" "{{{$1}}}")
          tokens         (map (partial apply str) (partition-by #(bracket-chars % 0) template))
          bracket-counts (reductions + (map #(get bracket-vals % 0) tokens))
          make-token     (fn [bracket-count token]
                            (let [type  (or (bracket-types (first token)) (bracket-types bracket-count))]
                                {:type  type
                                 :value ((if (= :reference type) keyword identity) token)
                                 :raw   (= 10 bracket-count)}))]
      (->> tokens
        (map make-token bracket-counts)                              ; Create token hash-maps
        (filter #(not (contains? bracket-kws (keyword (:value %))))) ; Remove {{, }}, {{{ and }}} elements
        (map #(if (= :reference (:type %)) % (dissoc % :raw)))))))   ; Remove :raw key from other types than :reference

(let [filter-type (fn [type coll] (filter #(not= (:type %) type) coll))]
  (defn parser [tokens]
    (if (nil? (:path (first tokens)))
      ; Let's add paths first
      (let [push-path   (fn [path token] (conj path (keyword (apply str (rest (:value token))))))
            update-path (fn [path token] (case (:type token)
                                           :path-start (push-path path token)
                                           :path-end   (pop path)
                                           path))
            ; Go through tokens while keeping track of the path we are at...
            add-paths (fn [result path tokens]
                        (if-let [token (first tokens)]
                          (let [new-path (update-path path token)]
                            ; Reversing new-path as we want to drop starting from beginning and not the end
                            (recur (conj result (assoc token :path (reverse new-path))) new-path (rest tokens)))
                          ; ...and finally remove :path-start nodes as we don't need them anymore
                          (filter-type :path-start result)))
            tokens-with-paths (add-paths [] (list :root) tokens)]
        ; Pass path-augmented nodes to the AST generator,
        (assoc (parser tokens-with-paths) :path nil))
      (let [first-path #(-> % :path first)
            path       (first-path (first tokens)) ; All tokens should have identical 1st path
            partitions (partition-by first-path (map #(update % :path rest) tokens))]
        {:path path :tokens (if (> (count partitions) 1) ; Call recursively until a single partition remains
                              (map parser partitions)
                              (->> partitions first (filter-type :path-end) (map #(dissoc % :path))))}))))

(let [get-data (fn [data path]
                 (let [value (get data path)]
                   ; data values should be sequential (vec or list),
                   ; but apparently the standard also supports single values.
                   (if (sequential? value) value (list value))))]
  (defn merge-ast-and-data [data ast]
    (if-not (:tokens ast)
      ; Not tokens, thus this isn't AST node but a single token
      (if (= (:type ast) :reference)
        ; :value is replaced by looking up its value from the current context's data
        (update ast :value #(case % :. data (% data))) ast)
      (if-let [path (:path ast)]
        ; With :path on this AST node we'll iterate over each data item and AST child nodes,
        ; with a nil path we'll just process each AST child node, while keeping the same data
        (for [data (get-data data path) ast (:tokens ast)] (merge-ast-and-data data ast))
        (for [                          ast (:tokens ast)] (merge-ast-and-data data ast))))))

; Finally putting it all together!
(defn render
  ([template] (render template {}))
  ([template data]
   (let [mapping     {\' "&apos;" \& "&amp;" (first "\"") "&quot;" \> "&gt;" \< "&lt;"}
         escape-html (fn [value] (apply str (map #(get mapping % %) value)))
         escaper    #((if (= (:raw %) false) escape-html identity) (:value %))]
     (->> template lexer parser (merge-ast-and-data data) flatten (map escaper) (apply str)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Testing our custom lexter, parser and other utility functions
(deftest test-parser
  (let [ast (-> "Hello{{#names}}, {{name}} (kids{{#kids}} {{.}}{{/kids}}){{/names}}!" lexer parser)]
    (is (= {:type :reference, :value :name, :raw false} (-> ast :tokens (nth 1) :tokens (nth 0) :tokens (nth 1))))))

(deftest test-lexer
  (let [tokens (-> "Hello{{#names}}, {{name}} (kids{{#kids}} {{{.}}}{{/kids}}){{/names}}!" lexer)]
    (is (= {:value "#kids", :type :path-start} (nth tokens 5)))))

(deftest test-merge-ast-and-data
  (let [data {:names [{:name "Felix" :kids ["a" "b"]} {:name "Jenny"}]}
        ast  (->> "Hello{{#names}}, {{&name}} (kids{{#kids}} {{.}}{{/kids}}){{/names}}!"
               lexer parser (merge-ast-and-data data) flatten)]
    (is (= {:type :reference, :value "b", :raw false} (nth ast 7)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; From https://github.com/fhd/clostache/blob/master/test/clostache/test_parser.clj
(deftest test-render-simple
  (is (= "Hello, Felix!" (render "Hello, {{name}}!" {:name "Felix"}))))

(deftest test-render-with-dollar-sign
  (is (= "Hello, $Felix!" (render "Hello, {{! This is a comment.}}{{name}}!" {:name "$Felix"}))))

(deftest test-nil-variable
  (is (= "Hello, ." (render "Hello, {{name}}." {:name nil}))))

(deftest test-missing-variables
  (is (= "Hello, . " (render "Hello, {{name}}. {{greeting}}" {}))))

(deftest test-render-html-unescaped
  (is (= "&\\\"<>" (render "{{{string}}}" {:string "&\\\"<>"}))))

(deftest test-render-html-unescaped-ampersand
  (is (= "&\"<>" (render "{{&string}}" {:string "&\"<>"}))))

(deftest test-render-html-escaped
  (is (= "&amp;&quot;&lt;&gt;&apos;" (render "{{string}}" {:string "&\"<>'"}))))

(deftest test-render-list
  (is (= "Hello, Felix, Jenny!" (render "Hello{{#names}}, {{name}}{{/names}}!"
                                        {:names [{:name "Felix"} {:name "Jenny"}]}))))

(deftest test-render-list-twice
  (is (= "Hello, Felix, Jenny! Hello, Felix, Jenny!"
         (render (str "Hello{{#names}}, {{name}}{{/names}}! "
                      "Hello{{#names}}, {{name}}{{/names}}!")
                 {:names [{:name "Felix"} {:name "Jenny"}]}))))

(deftest test-render-single-value
  (is (= "Hello, Felix!" (render "Hello{{#person}}, {{name}}{{/person}}!" {:person {:name "Felix"}}))))

(run-tests)
