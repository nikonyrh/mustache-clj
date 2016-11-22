(ns mustache-clj.tests (:gen-class)
  (:use clojure.test mustache-clj.core)
  (:require [clojure.string :as str]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Testing our custom lexter, parser and other utility functions
(deftest test-lexer
  (let [tokens (-> "Hello{{#names}}, {{name}} (kids{{#kids}} {{{.}}}{{/kids}}){{/names}}!" lexer)]
    (is (= {:value "#kids", :type :path-start} (nth tokens 5)))))

(deftest test-parser
  (let [ast (-> "Hello{{#names}}, {{name}} (kids{{#kids}} {{.}}{{/kids}}){{/names}}!" lexer parser)]
    (is (= {:type :reference, :value :name, :raw false} (-> ast :tokens (nth 1) :tokens (nth 0) :tokens (nth 1))))))

(deftest test-merge-ast-and-data
  (let [data {:names [{:name "Felix" :kids ["a" "b"]} {:name "Jenny"}]}
        ast  (->> "Hello{{#names}}, {{&name}} (kids{{#kids}} {{.}}{{/kids}}){{/names}}!"
               lexer parser (merge-ast-and-data data) flatten)]
    (is (= {:type :reference, :value "b", :raw false} (nth ast 7)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; From https://github.com/fhd/clostache/blob/master/test/clostache/test_parser.clj
(deftest test-render-simple
  (is (= "Hello, Felix!" (render "Hello, {{name}}!" {:name "Felix"}))))

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
  (is (= "Hello, Felix, Jenny, nice to see, Felix, Jenny!"
         (render "Hello{{#names}}, {{name}}{{/names}}, nice to see{{#names}}, {{name}}{{/names}}!"
                 {:names [{:name "Felix"} {:name "Jenny"}]}))))

(deftest test-render-single-value
  (is (= "Hello, Felix!" (render "Hello{{#person}}, {{name}}{{/person}}!" {:person {:name "Felix"}}))))

(deftest test-render-empty-list
  (is (= "" (render "{{#things}}Something{{/things}}" {:things []}))))

(deftest test-render-nested-list
  (is (= "z" (render "{{#x}}{{#y}}{{z}}{{/y}}{{/x}}" {:x {:y {:z "z"}}}))))

(deftest test-render-comment
  (is (= "Hello, Felix!" (render "Hello, {{! This is a comment.}}{{name}}!" {:name "Felix"}))))

(deftest test-render-tags-with-whitespace
  (is (= "Hello, Felix" (render "Hello, {{# names }}{{ name }}{{/ names }}" {:names [{:name "Felix"}]}))))

(deftest test-render-boolean-true
  (is (= "Hello, Felix" (render "Hello, {{#condition}}Felix{{/condition}}" {:condition true}))))

(deftest test-render-boolean-false
  (is (= "Hello, " (render "Hello, {{#condition}}Felix{{/condition}}" {:condition false}))))

(deftest test-render-inverted-empty-list
  (is (= "Empty" (render "{{^things}}Empty{{/things}}" {:things []}))))

(deftest test-render-inverted-list
  (is (= "" (render "{{^things}}Empty{{/things}}" {:things ["Something"]}))))

(deftest test-render-inverted-boolean-true
  (is (= "Hello, " (render "Hello, {{^condition}}Felix{{/condition}}" {:condition true}))))

(deftest test-render-inverted-boolean-false
  (is (= "Hello, Felix" (render "Hello, {{^condition}}Felix{{/condition}}" {:condition false}))))

(deftest test-render-tag-with-dotted-name-like-section
  (is (= "Hello, Felix" (render "Hello, {{felix.name}}" {:felix {:name "Felix"}}))))


(deftest test-render-with-partial
  (is (= "Hi, Felix" (render "Hi, {{>name}}" {:n "Felix"} {:name "{{n}}"}))))

(deftest test-render-partial-recursive
  (is (= "One Two Three Four Five" (render "One {{>two}}"
                                            {}
                                            {:two "Two {{>three}}"
                                             :three "Three {{>four}}"
                                             :four "Four {{>five}}"
                                             :five "Five"}))))

(deftest test-render-with-variable-containing-template
  (is (= "{{hello}},world" (render "{{tmpl}},{{hello}}" {:tmpl "{{hello}}" :hello "world"}))))

(deftest test-render-sorted-set
  (let [sort-by-x (fn [x y] (compare (:x x) (:x y)))
        l         (sorted-set-by sort-by-x {:x 1} {:x 5} {:x 3})]
    (is (= "135" (render "{{#l}}{{x}}{{/l}}" {:l l})))
    (is (= ""    (render "{{^l}}X{{/l}}"     {:l l})))
    (is (= "X"   (render "{{^l}}X{{/l}}"     {:l (sorted-set)})))))

(deftest test-path-whitespace-handled-consistently
  (is (= (render "{{a}}"     {:a "value"})      "value"))
  (is (= (render "{{ a }}"   {:a "value"})      "value"))
  (is (= (render "{{a.b}}"   {:a {:b "value"}}) "value"))
  (is (= (render "{{ a.b.c }}" {:a {:b {:c "value"}}}) "value")))

;(run-tests)
