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
                                 :value (if (= :reference type) (keyword token) token)
                                 :raw   (= 10 bracket-count)}))]
      (->> tokens
        (map make-token bracket-counts)                              ; Create token hash-maps
        (filter #(not (contains? bracket-kws (keyword (:value %))))) ; Remove {{, }}, {{{ and }}} elements
        (map #(if (= :reference (:type %)) % (dissoc % :raw)))))))   ; Remove :raw key from other types than :reference

(deftest test-lexer
  (let [tokens (-> "Hello{{#names}}, {{name}} (kids{{#kids}} {{{.}}}{{/kids}}){{/names}}!" lexer)]
    (is (= {:value "#kids", :type :path-start} (nth tokens 5)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn parser
  ([tokens]
   (let [push-path   (fn [path token] (conj path (keyword (apply str (rest (:value token))))))
         update-path (fn [path token] (case (:type token)
                                        :path-start (push-path path token)
                                        :path-end   (pop path)
                                        path))
         ; Go through tokens while keeping track of the path we are at.
         add-paths (fn [result path tokens]
                     (if-let [token (first tokens)]
                       (let [new-path (update-path path token)]
                          (recur (conj result (assoc token :path new-path)) new-path (rest tokens)))
                       ; Finally remove :path-start nodes as we don't need them anymore
                       (filter #(not= (:type %) :path-start) result)))
         tokens-with-paths (add-paths [] [] tokens)]
     ; Pass path-augmented nodes to the AST generator
     (parser tokens-with-paths 0)))
  ([tokens-with-paths tree-lvl]
   (let [get-path (fn [token] (get (:path token) tree-lvl))]
     (if (not-every? nil? (map get-path tokens-with-paths))
       ; Descend to deeper levels of :path until all AST sibling nodes have identical paths
       (map #(parser % (inc tree-lvl)) (partition-by get-path tokens-with-paths))
       {:path   (:path (first tokens-with-paths))
        ; Finally remove :path-end tokens and :path keys as we don't need them anymore
        :tokens (map #(dissoc % :path) (filter #(not= (:type %) :path-end) tokens-with-paths))}))))

(deftest test-parser
  (let [ast (-> "Hello{{#names}}, {{name}} (kids{{#kids}} {{.}}{{/kids}}){{/names}}!" lexer parser)]
    (is (= [:names :kids] (-> ast (nth 1) (nth 1) :path)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn flatten-ast
  ([ast-node data] (flatten-ast ast-node data 0 []))
  ([ast-node data tree-level path-prefix]
   (if (seq? ast-node)
     (flatten (map #(flatten-ast % data tree-level path-prefix) ast-node))
     (let [data-key    (keyword (nth (:path ast-node) tree-level nil))
           data        (get data data-key)
           next-prefix (conj path-prefix data-key)
           next-level  (inc tree-level)]
       (if (nil? data)
         (map #(if-not (= :reference (:type %)) % (assoc % :value (conj path-prefix (:value %)))) (:tokens ast-node))
         (map #(flatten-ast ast-node %1 next-level (conj next-prefix %2)) data (range)))))))

(deftest test-flatten-ast
  (let [ast  (-> "Hello{{#names}}, {{name}} (kids{{#kids}} {{.}}{{/kids}}){{/names}}!" lexer parser)
        data {:names [{:name "Felix" :kids ["a" "b"]} {:name "Jenny"}]}]
    ;(flatten-ast ast data)
    (is (= {:value [:names 0 :kids 1 :.], :type :reference, :raw false} (-> (flatten-ast ast data) (nth 10))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn flatten-data
  ([data] (into {} (flatten (flatten-data data []))))
  ([data path-prefix]
   (if-not (map? data)
    {path-prefix data} ; Replace the key, keep data as is
    (for [[k v] data]  ; Replace keys recursively, while keeping track of the parents' keys and indexes
     (let [new-prefix (conj path-prefix k)]
      (if (sequential? v)
        (map #(flatten-data %1 (conj new-prefix %2)) v (range))
        {new-prefix v}))))))

(deftest test-flatten-data
  (let [data {:names [{:name "Felix" :kids ["a" "b"]} {:name "Jenny"}]}]
    (is (= "b" ((flatten-data data) [:names 0 :kids 1])))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn replace-symbols [tokens data]
  (let [mapping {\& "&amp;" \\ "&quot;" \> "&lt;" \< "&gt;"}
        escape-html (fn [value] (apply str (map #(get mapping % %) value)))
        token-fun #(let [bracket (:bracket %)
                         token   (:value   %)]
                     (if (= 0 bracket) token
                       ((if (= 1 bracket) escape-html identity) (or (data (keyword token)) ""))))
        result (apply str (map token-fun tokens))]
    {:values tokens :result result}))

(defn render [template data]
  (replace-symbols (parse-template template) data))

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
  (is (= "&amp;&quot;&lt;&gt;" (render "{{string}}" {:string "&\\\"<>"}))))

(deftest test-render-list
  (is (= "Hello, Felix, Jenny!" (render "Hello{{#names}}, {{name}}{{/names}}!"
                                        {:names [{:name "Felix"} {:name "Jenny"}]}))))

(deftest test-render-list-twice
  (is (= "Hello, Felix, Jenny! Hello, Felix, Jenny!"
         (render (str "Hello{{#names}}, {{name}}{{/names}}! "
                      "Hello{{#names}}, {{name}}{{/names}}!")
                 {:names [{:name "Felix"} {:name "Jenny"}]}))))

(deftest test-render-single-value
  (is (= "Hello, Felix!" (render "Hello{{#person}}, {{name}}{{/person}}!"))))
{:person {:name "Felix"}}

(run-tests)
