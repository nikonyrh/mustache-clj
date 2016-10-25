(ns mustache-clj.core (:gen-class)
  (:use clojure.test)
  (:require [clojure.string :as str]))

(let [bracket-chars {\{ 1 \} 2}
      bracket-vals  {"{{" 1 "}}" -1 "{{{" 10 "}}}" -10}
      bracket-types {0 :const 1 :normal 10 :raw \# :path-start \/ :path-end}]
  (defn lexer [template]
    ; Split a string to tokens, group by brackets, determine their types
    (let [template       (str/replace template #"\{\{&(.*?)\}\}" "{{{$1}}}")
          tokens         (map (partial apply str) (partition-by #(bracket-chars % 0) template))
          bracket-counts (reductions + (map #(get bracket-vals % 0) tokens))
          tokens         (filter #(not (contains? bracket-vals (:value %)))
                           (map #(hash-map :value %1 :type (bracket-types %2)) tokens bracket-counts))
          tokens         (map #(if (= (:type %) :const) %
                                (assoc % :type (bracket-types (first (:value %)) :reference))) tokens)]
      ; Go through tokens again, keeping track of the path we are at. Originally these
      ; were separate functions but it was easier to develop and test this way.
      (let [push-path   (fn [path token] (conj path (apply str (rest (:value token)))))
            update-path (fn [path token] (case (:type token)
                                           :path-start (push-path path token)
                                           :path-end   (pop path)
                                           path))
            add-paths (fn [result path tokens]
                        (if-let [token (first tokens)]
                          (let [new-path (update-path path token)]
                             (recur (conj result (assoc token :path new-path)) new-path (rest tokens)))
                          result))
            tokens-with-paths (add-paths [] [] tokens)]
        (filter #(not= (:type %) :path-start)) tokens-with-paths))))

(deftest test-lexer
  (let [tokens (-> "Hello{{#names}}, {{name}} (kids{{#kids}} {{.}}{{/kids}}){{/names}}!" lexer)]
    (is (= {:path ["names" "kids"], :value ".", :bracket 1} (nth tokens 5)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let [path-at-lvl #(get (:path %2) %1)]
  (defn parser
    ([tokens] (parser tokens 0))
    ([tokens tree-lvl]
     (let [partitions      (partition-by (partial path-at-lvl tree-lvl) tokens)
           first-partition (first partitions)]
       (if (and
             (nil? (path-at-lvl tree-lvl first-partition))
             (= 1  (count partitions)))
         (filter #(not= (:type %) :path-end) first-partition)
         (map    #(parser % (inc tree-lvl))  partitions))))))

(deftest test-parser
  (let [ast (-> "Hello{{#names}}, {{name}} (kids{{#kids}} {{.}}{{/kids}}){{/names}}!" lexer parser)]
    (is (= {:path ["names" "kids"], :value " ", :bracket 0}
           (-> ast (nth 1) (nth 1) (nth 0))))))
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
