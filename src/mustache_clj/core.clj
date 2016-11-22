(ns mustache-clj.core (:gen-class)
  (:use clojure.test)
  (:require [clojure.string :as str]))

(defn filter-type [type coll] (filter #(not= (:type %) type) coll))

; From https://gist.github.com/xpe/46128da5accb0acdf30d
(defn iterate-until-stable
  "Takes a function of one arg and calls (f x), (f (f x)), and so on
  until the value does not change."
  [x f] (loop [v x] (let [v' (f v)] (if (= v' v) v (recur v')))))

(let [bracket-chars {\{ 1 \} 2}
      bracket-vals  {"{{" 1 "}}" -1 "{{{" 10 "}}}" -10}
      bracket-kws   (set (map keyword (keys bracket-vals)))
      bracket-types {0 :const 1 :reference 10 :reference \# :path-start \/ :path-end \! :comment \^ :path-start}]
  (defn lexer [template]
    ; Split a string to tokens, group by brackets, determine their types
    (let [template       (-> template
                             (str/replace #"\{\{&(.*?)\}\}" "{{{$1}}}")
                             (str/replace #"\{\{([^ ]?)[ ]*([^ ]*?)[ ]*\}\}" "{{$1$2}}")
                             (iterate-until-stable #(str/replace % #"\{\{([^\.\{]+)\.([^\}]+)\}\}" "{{#$1}}{{$2}}{{/$1}}")))
          tokens         (->> template (partition-by #(bracket-chars % 0)) (map (partial apply str)))
          bracket-counts (->> tokens   (map #(get bracket-vals % 0))       (reductions +))
          make-token     (fn [bracket-count token]
                            (let [type (or (and (not= 0 bracket-count) (bracket-types (first token)))
                                           (bracket-types bracket-count))]
                                {:type  type
                                 :value ((if (= :reference type) keyword identity) token)
                                 :raw   (= 10 bracket-count)}))]
      (->> tokens
        (map make-token bracket-counts)                              ; Create token hash-maps
        (filter-type :comment)                                       ; Remove comments
        (filter #(not (contains? bracket-kws (keyword (:value %))))) ; Remove {{, }}, {{{ and }}} elements
        (map #(if     (= :reference  (:type %)) % (dissoc % :raw)))  ; Remove :raw key from other types than :reference
        (map #(if-not (= :path-start (:type %)) % (assoc  % :inverted (= \^ (first (:value %))))))))))

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
                        result))]
      ; Pass path-augmented nodes to the AST generator, replace place-holder path :root with nil
      (->> tokens (add-paths [] (list :root)) parser))
    (let [first-path #(-> % :path first)
          path       (first-path (first tokens)) ; All tokens should have identical 1st path
          path       (if (= :root path) nil path)
          partitions (->> tokens (map #(update % :path rest)) (partition-by first-path))]
      {:path     path
       :inverted (if path (:inverted (first tokens)) false)
       :tokens   (if (> (count partitions) 1)
                   (map parser partitions) ; Call recursively until a single partition remains
                   (->> partitions first   ; Remove un-needed tokens and :path
                        (filter-type :path-start)
                        (filter-type :path-end)
                        (map #(dissoc % :path))))})))

; data values should be sequential (vec or list),
; but apparently the standard also supports single values.
(let [to-sequential (fn [i] (if (sequential? i) i (list i)))
      get-data      (fn [inverted data path]
                      (let [value (get data path)]
                        (if inverted (if-not value (list true))
                          (if value (to-sequential value)))))]
  (defn merge-ast-and-data [data ast]
    (if-not (:tokens ast)
      ; Not tokens, thus this isn't AST node but a single token
      (if (= (:type ast) :reference)
        ; :value is replaced by looking up its value from the current context's data
        (update ast :value #(case % :. data (% data))) ast)
      (let [path      (:path ast)
            this-data (get-data (:inverted ast) data path)]
        ; With :path on this AST node we'll iterate over each data item and AST child nodes,
        ; with a nil path we'll just process each AST child node, while keeping the same data
        (if path
          (for [ast (:tokens ast) data this-data] (merge-ast-and-data data ast))
          (for [ast (:tokens ast)               ] (merge-ast-and-data data ast)))))))

; Finally putting it all together!
(defn render
  ([template]      (render template {}))
  ([template data] (render template data {}))
  ([template data partials]
   (let [mapping     {\' "&apos;" \& "&amp;" (first "\"") "&quot;" \> "&gt;" \< "&lt;"}
         escape-html (fn [value] (apply str (map #(get mapping % %) value)))
         escaper    #((if (= (:raw %) false) escape-html identity) (:value %))]
     (->> template lexer parser (merge-ast-and-data data) flatten (map escaper) (apply str)))))
