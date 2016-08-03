(ns mustache-clj.core (:gen-class)
  (:use clojure.test)
  (:require [clojure.string :as str]))

(let [mapping {\& "&amp;" \\ "&quot;" \> "&lt;" \< "&gt;"}]
  (defn escape-html [value] (apply str (map #(get mapping % %) value))))

(defn -render [paired data]
  (apply str (map #(if (= 0 (:bracket %))
                     (:token %)
                     (let [value (or (data (keyword (:token %))) "")
                           fun   (if (= 1 (:bracket %)) escape-html identity)]
                       (fun value)))
               paired)))

(let [brackets     (into #{} "{}")
      bracket-vals {"{{" 1 "}}" -1 "{{{" 10 "}}}" -10}]
  (defn render [template-orig data]
    (let [template (str/replace template-orig #"\{\{&(.*?)\}\}" "{{{$1}}}")
          tokens   (map (partial apply str) (partition-by (partial contains? brackets) template))
          counts   (reductions + (map #(get bracket-vals % 0) tokens))
          paired   (filter #(not (contains? bracket-vals (:token %)))
                     (map #(hash-map :token %1 :bracket %2) tokens counts))]
      (-render paired data))))

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

(run-tests)
