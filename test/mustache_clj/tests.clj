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
  (is (= "Hello, Felix, Jenny, nice to see, Felix, Jenny!"
         (render "Hello{{#names}}, {{name}}{{/names}}, nice to see{{#names}}, {{name}}{{/names}}!"
                 {:names [{:name "Felix"} {:name "Jenny"}]}))))

(deftest test-render-single-value
  (is (= "Hello, Felix!" (render "Hello{{#person}}, {{name}}{{/person}}!" {:person {:name "Felix"}}))))

;(run-tests)
