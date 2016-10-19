(ns mustache-clj.core (:gen-class)
  (:use clojure.test)
  (:require [clojure.string :as str]))

(let [mapping {\& "&amp;" \\ "&quot;" \> "&lt;" \< "&gt;"}]
  (defn escape-html [value] (apply str (map #(get mapping % %) value))))

(let [brackets     {\{ 1 \} 2}
      bracket-vals {"{{" 1 "}}" -1 "{{{" 100 "}}}" -100}]
  (defn str-to-tokens [template]
    (let [template       (str/replace template #"\{\{&(.*?)\}\}" "{{{$1}}}")
          tokens         (map (partial apply str) (partition-by #(brackets % 0) template))
          bracket-counts (reductions + (map #(get bracket-vals % 0) tokens))
          tokens         (filter #(not (contains? bracket-vals (:token %)))
                           (map #(hash-map :token %1 :bracket %2) tokens bracket-counts))]
      tokens)))
;(-> "Hello{{#names}}, {{name}}{{/names}}!" str-to-tokens)

(defn tokens-with-paths [tokens]
  (let [path-tokens #{\# \/}
        add-paths   (fn [result path tokens]
                      (if-let [token (first tokens)]
                        (let [type (first (:token token))
                              path (if (or (= 0 (:bracket token))
                                           (not (contains? path-tokens type)))
                                     path
                                     (case type
                                       \# (conj path (apply str (rest (:token token))))
                                       \/ (pop  path)))]
                           (recur (conj result (assoc token :path path)) path (rest tokens)))
                        result))]
    (filter #(not (= \# (first (:token %)))) (add-paths [] [] tokens))))
;(-> "Hello{{#names}}, {{name}} (kids{{#kids}} {{.}}{{/kids}}){{/names}}!" str-to-tokens tokens-with-paths)

(let [path-at-lvl #(get (:path %2) %1)]
  (defn tree-from-paths
    ([tokens] (tree-from-paths tokens 0))
    ([tokens tree-lvl]
     (let [partitions      (partition-by (partial path-at-lvl tree-lvl) tokens)
           first-partition (first partitions)]
       (if (and
             (nil? (path-at-lvl tree-lvl first-partition))
             (= 1 (count partitions)))
         (filter #(not (= \/ (first (:token %)))) first-partition)
         (map #(tree-from-paths % (inc tree-lvl)) partitions))))))
;(-> "Hello{{#names}}, {{name}} (kids{{#kids}} {{.}}{{/kids}}){{/names}}!" str-to-tokens tokens-with-paths tree-from-paths)

;{:names [{:name "Felix" :kids ["a" "b"]} {:name "Jenny"}]}
;"Hello, {{names_0_name}} (kids {{names_0_kid_0}} {{names_0_kid_1}}), {{names_1_name}} (kids )!"

(defn replace-symbols [tokens data]
  (let [token-fun #(let [bracket (:bracket %)
                         token   (:token   %)]
                     (if (= 0 bracket) token
                       ((if (= 1 bracket) escape-html identity) (or (data (keyword token)) ""))))
        result (apply str (map token-fun tokens))]
    {:tokens tokens :result result}))

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
