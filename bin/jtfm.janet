#! /usr/bin/env janet

(comment import ./args :prefix "")
(defn a/parse-args
  [args]
  (def the-args (array ;args))
  #
  (def head (get the-args 0))
  #
  (def conf-file ".jtfm.jdn")
  #
  (when (or (= head "-h") (= head "--help")
            # might have been invoked with no paths in repository root
            (and (not head)
                 (not= :file (os/stat conf-file :mode))))
    (break @{:help true}))
  #
  (def opts
    (if head
      (if-not (and (string/has-prefix? "{" head)
                   (string/has-suffix? "}" head))
        @{}
        (let [parsed
              (try (parse (string "@" head))
                ([e] (eprint e)
                     (errorf "failed to parse options: %n" head)))]
          (assertf (and parsed (table? parsed))
                   "expected table but found: %s" (type parsed))
          (array/remove the-args 0)
          parsed))
      @{}))
  #
  (def [includes excludes]
    (cond
      # paths on command line take precedence over conf file
      (not (empty? the-args))
      [the-args @[]]
      # conf file
      (= :file (os/stat conf-file :mode))
      (let [conf (try (parse (slurp conf-file))
                   ([e] (error e)))]
        (assertf conf "failed to parse: %s" conf-file)
        (assertf (dictionary? conf)
                 "expected dictionary, got: %s" (type conf))
        #
        [(array ;(get conf :includes @[]))
         (array ;(get conf :excludes @[]))])
      #
      (errorf "unexpected result parsing: %n" args)))
  #
  (defn merge-indexed
    [left right]
    (default left [])
    (default right [])
    (distinct [;left ;right]))
  #
  (merge opts
         {:includes (merge-indexed includes (get opts :includes))
          :excludes (merge-indexed excludes (get opts :excludes))}))

(comment

  (a/parse-args ["src/main.janet"])
  # =>
  @{:excludes @[]
    :includes @["src/main.janet"]}

  (a/parse-args ["-h"])
  # =>
  @{:help true}

  (a/parse-args ["{:overwrite true}" "src/main.janet"])
  # =>
  @{:excludes @[]
    :includes @["src/main.janet"]
    :overwrite true}

  (a/parse-args [`{:excludes ["src/args.janet"]}` "src/main.janet"])
  # =>
  @{:excludes @["src/args.janet"]
    :includes @["src/main.janet"]}

  )


(comment import ./search :prefix "")
(def s/sep
  (if (= :windows (os/which))
    `\`
    "/"))

(defn s/find-files
  [dir &opt pred]
  (default pred identity)
  (def paths @[])
  (defn helper
    [a-dir]
    (each path (os/dir a-dir)
      (def sub-path
        (string a-dir s/sep path))
      (case (os/stat sub-path :mode)
        :directory
        (when (not= path ".git")
          (when (not (os/stat (string sub-path s/sep ".gitrepo")))
            (helper sub-path)))
        #
        :file
        (when (pred sub-path)
          (array/push paths sub-path)))))
  (helper dir)
  paths)

(comment

  (s/find-files "." |(string/has-suffix? ".janet" $))

  )

(defn s/clean-end-of-path
  [path a-sep]
  (when (one? (length path))
    (break path))
  (if (string/has-suffix? a-sep path)
    (string/slice path 0 -2)
    path))

(comment

  (s/clean-end-of-path "hello/" "/")
  # =>
  "hello"

  (s/clean-end-of-path "/" "/")
  # =>
  "/"

  )

(defn s/has-janet-shebang?
  [path]
  (with [f (file/open path)]
    (def first-line (file/read f :line))
    (when first-line
      (and (string/find "env" first-line)
           (string/find "janet" first-line)))))

(defn s/collect-paths
  [includes &opt pred]
  (default pred identity)
  (def filepaths @[])
  # collect file and directory paths
  (each thing includes
    (def apath (s/clean-end-of-path thing s/sep))
    (def mode (os/stat apath :mode))
    # XXX: should :link be supported?
    (cond
      (= :file mode)
      (array/push filepaths apath)
      #
      (= :directory mode)
      (array/concat filepaths (s/find-files apath pred))
      #
      (do
        (eprintf "No such file or not an ordinary file or directory: %s"
                 apath)
        (os/exit 1))))
  #
  filepaths)

(defn s/search-paths
  [query-fn opts]
  (def {:name name :paths src-paths} opts)
  #
  (def all-results @[])
  (def hit-paths @[])
  (each path src-paths
    (def src (slurp path))
    (when (pos? (length src))
      (when (or (not name)
                (string/find name src))
        (array/push hit-paths path)
        (def results
          (try
            (query-fn src opts)
            ([e]
              (eprintf "search failed for: %s" path))))
        (when (and results (not (empty? results)))
          (each item results
            (array/push all-results (merge item {:path path})))))))
  #
  [all-results hit-paths])


(comment import ./rewrite :prefix "")
(comment import ./jipper :prefix "")
# bl - begin line
# bc - begin column
# el - end line
# ec - end column
(defn j/make-attrs
  [& items]
  (zipcoll [:bl :bc :el :ec]
           items))

(defn j/atom-node
  [node-type peg-form]
  ~(cmt (capture (sequence (line) (column)
                           ,peg-form
                           (line) (column)))
        ,|[node-type (j/make-attrs ;(slice $& 0 -2)) (last $&)]))

(defn j/reader-macro-node
  [node-type sigil]
  ~(cmt (capture (sequence (line) (column)
                           ,sigil
                           (any :non-form)
                           :form
                           (line) (column)))
        ,|[node-type (j/make-attrs ;(slice $& 0 2) ;(slice $& -4 -2))
           ;(slice $& 2 -4)]))

(defn j/collection-node
  [node-type open-delim close-delim]
  # to avoid issues when transforming this file
  (def replace_ (symbol "replace"))
  ~(cmt
     (capture
       (sequence
         (line) (column)
         ,open-delim
         (any :input)
         (choice ,close-delim
                 (error
                   (,replace_ (sequence (line) (column))
                              ,|(string/format
                                  "line: %p column: %p missing %p for %p"
                                  $0 $1 close-delim node-type))))
         (line) (column)))
     ,|[node-type (j/make-attrs ;(slice $& 0 2) ;(slice $& -4 -2))
        ;(slice $& 2 -4)]))

(def j/loc-grammar
  ~@{:main (sequence (line) (column)
                     (some :input)
                     (line) (column))
     #
     :input (choice :non-form
                    :form)
     #
     :non-form (choice :whitespace
                       :comment)
     #
     :whitespace ,(j/atom-node :whitespace
                             '(choice (some (set " \0\f\t\v"))
                                      (choice "\r\n"
                                              "\r"
                                              "\n")))
     # :whitespace
     # (cmt (capture (sequence (line) (column)
     #                         (choice (some (set " \0\f\t\v"))
     #                                 (choice "\r\n"
     #                                         "\r"
     #                                         "\n"))
     #                         (line) (column)))
     #      ,|[:whitespace (make-attrs ;(slice $& 0 -2)) (last $&)])
     #
     :comment ,(j/atom-node :comment
                          '(sequence "#"
                                     (any (if-not (set "\r\n") 1))))
     #
     :form (choice # reader macros
                   :fn
                   :quasiquote
                   :quote
                   :splice
                   :unquote
                   # collections
                   :array
                   :bracket-array
                   :tuple
                   :bracket-tuple
                   :table
                   :struct
                   # atoms
                   :number
                   :constant
                   :buffer
                   :string
                   :long-buffer
                   :long-string
                   :keyword
                   :symbol)
     #
     :fn ,(j/reader-macro-node :fn "|")
     # :fn (cmt (capture (sequence (line) (column)
     #                             "|"
     #                             (any :non-form)
     #                             :form
     #                             (line) (column)))
     #          ,|[:fn (make-attrs ;(slice $& 0 2) ;(slice $& -4 -2))
     #             ;(slice $& 2 -4)])
     #
     :quasiquote ,(j/reader-macro-node :quasiquote "~")
     #
     :quote ,(j/reader-macro-node :quote "'")
     #
     :splice ,(j/reader-macro-node :splice ";")
     #
     :unquote ,(j/reader-macro-node :unquote ",")
     #
     :array ,(j/collection-node :array "@(" ")")
     # :array
     # (cmt
     #   (capture
     #     (sequence
     #       (line) (column)
     #       "@("
     #       (any :input)
     #       (choice ")"
     #               (error
     #                 (replace (sequence (line) (column))
     #                          ,|(string/format
     #                              "line: %p column: %p missing %p for %p"
     #                              $0 $1 ")" :array))))
     #       (line) (column)))
     #   ,|[:array (make-attrs ;(slice $& 0 2) ;(slice $& -4 -2))
     #      ;(slice $& 2 -4)])
     #
     :tuple ,(j/collection-node :tuple "(" ")")
     #
     :bracket-array ,(j/collection-node :bracket-array "@[" "]")
     #
     :bracket-tuple ,(j/collection-node :bracket-tuple "[" "]")
     #
     :table ,(j/collection-node :table "@{" "}")
     #
     :struct ,(j/collection-node :struct "{" "}")
     #
     :number ,(j/atom-node :number
                         ~(drop (sequence (cmt (capture (some :num-char))
                                               ,scan-number)
                                          (opt (sequence ":" (range "AZ" "az"))))))
     #
     :num-char (choice (range "09" "AZ" "az")
                       (set "&+-._"))
     #
     :constant ,(j/atom-node :constant
                           '(sequence (choice "false" "nil" "true")
                                      (not :name-char)))
     #
     :name-char (choice (range "09" "AZ" "az" "\x80\xFF")
                        (set "!$%&*+-./:<?=>@^_"))
     #
     :buffer ,(j/atom-node :buffer
                         '(sequence `@"`
                                    (any (choice :escape
                                                 (if-not "\"" 1)))
                                    `"`))
     #
     :escape (sequence "\\"
                       (choice (set `"'0?\abefnrtvz`)
                               (sequence "x" (2 :h))
                               (sequence "u" (4 :h))
                               (sequence "U" (6 :h))
                               (error (constant "bad escape"))))
     #
     :string ,(j/atom-node :string
                         '(sequence `"`
                                    (any (choice :escape
                                                 (if-not "\"" 1)))
                                    `"`))
     #
     :long-string ,(j/atom-node :long-string
                              :long-bytes)
     #
     :long-bytes {:main (drop (sequence :open
                                        (any (if-not :close 1))
                                        :close))
                  :open (capture :delim :n)
                  :delim (some "`")
                  :close (cmt (sequence (not (look -1 "`"))
                                        (backref :n)
                                        (capture (backmatch :n)))
                              ,=)}
     #
     :long-buffer ,(j/atom-node :long-buffer
                              '(sequence "@" :long-bytes))
     #
     :keyword ,(j/atom-node :keyword
                          '(sequence ":"
                                     (any :name-char)))
     #
     :symbol ,(j/atom-node :symbol
                         '(some :name-char))
     })

(comment

  (get (peg/match j/loc-grammar " ") 2)
  # =>
  '(:whitespace @{:bc 1 :bl 1 :ec 2 :el 1} " ")

  (get (peg/match j/loc-grammar "true?") 2)
  # =>
  '(:symbol @{:bc 1 :bl 1 :ec 6 :el 1} "true?")

  (get (peg/match j/loc-grammar "nil?") 2)
  # =>
  '(:symbol @{:bc 1 :bl 1 :ec 5 :el 1} "nil?")

  (get (peg/match j/loc-grammar "false?") 2)
  # =>
  '(:symbol @{:bc 1 :bl 1 :ec 7 :el 1} "false?")

  (get (peg/match j/loc-grammar "# hi there") 2)
  # =>
  '(:comment @{:bc 1 :bl 1 :ec 11 :el 1} "# hi there")

  (get (peg/match j/loc-grammar "1_000_000") 2)
  # =>
  '(:number @{:bc 1 :bl 1 :ec 10 :el 1} "1_000_000")

  (get (peg/match j/loc-grammar "8.3") 2)
  # =>
  '(:number @{:bc 1 :bl 1 :ec 4 :el 1} "8.3")

  (get (peg/match j/loc-grammar "1e2") 2)
  # =>
  '(:number @{:bc 1 :bl 1 :ec 4 :el 1} "1e2")

  (get (peg/match j/loc-grammar "0xfe") 2)
  # =>
  '(:number @{:bc 1 :bl 1 :ec 5 :el 1} "0xfe")

  (get (peg/match j/loc-grammar "2r01") 2)
  # =>
  '(:number @{:bc 1 :bl 1 :ec 5 :el 1} "2r01")

  (get (peg/match j/loc-grammar "3r101&01") 2)
  # =>
  '(:number @{:bc 1 :bl 1 :ec 9 :el 1} "3r101&01")

  (get (peg/match j/loc-grammar "2:u") 2)
  # =>
  '(:number @{:bc 1 :bl 1 :ec 4 :el 1} "2:u")

  (get (peg/match j/loc-grammar "-8:s") 2)
  # =>
  '(:number @{:bc 1 :bl 1 :ec 5 :el 1} "-8:s")

  (get (peg/match j/loc-grammar "1e2:n") 2)
  # =>
  '(:number @{:bc 1 :bl 1 :ec 6 :el 1} "1e2:n")

  (get (peg/match j/loc-grammar "printf") 2)
  # =>
  '(:symbol @{:bc 1 :bl 1 :ec 7 :el 1} "printf")

  (get (peg/match j/loc-grammar ":smile") 2)
  # =>
  '(:keyword @{:bc 1 :bl 1 :ec 7 :el 1} ":smile")

  (get (peg/match j/loc-grammar `"fun"`) 2)
  # =>
  '(:string @{:bc 1 :bl 1 :ec 6 :el 1} "\"fun\"")

  (get (peg/match j/loc-grammar "``long-fun``") 2)
  # =>
  '(:long-string @{:bc 1 :bl 1 :ec 13 :el 1} "``long-fun``")

  (get (peg/match j/loc-grammar "@``long-buffer-fun``") 2)
  # =>
  '(:long-buffer @{:bc 1 :bl 1 :ec 21 :el 1} "@``long-buffer-fun``")

  (get (peg/match j/loc-grammar `@"a buffer"`) 2)
  # =>
  '(:buffer @{:bc 1 :bl 1 :ec 12 :el 1} "@\"a buffer\"")

  (get (peg/match j/loc-grammar "@[8]") 2)
  # =>
  '(:bracket-array @{:bc 1 :bl 1
                     :ec 5 :el 1}
                   (:number @{:bc 3 :bl 1
                              :ec 4 :el 1} "8"))

  (get (peg/match j/loc-grammar "@{:a 1}") 2)
  # =>
  '(:table @{:bc 1 :bl 1
             :ec 8 :el 1}
           (:keyword @{:bc 3 :bl 1
                       :ec 5 :el 1} ":a")
           (:whitespace @{:bc 5 :bl 1
                          :ec 6 :el 1} " ")
           (:number @{:bc 6 :bl 1
                      :ec 7 :el 1} "1"))

  (get (peg/match j/loc-grammar "~x") 2)
  # =>
  '(:quasiquote @{:bc 1 :bl 1
                  :ec 3 :el 1}
                (:symbol @{:bc 2 :bl 1
                           :ec 3 :el 1} "x"))

  (get (peg/match j/loc-grammar "' '[:a :b]") 2)
  # =>
  '(:quote @{:bc 1 :bl 1
             :ec 11 :el 1}
           (:whitespace @{:bc 2 :bl 1
                          :ec 3 :el 1} " ")
           (:quote @{:bc 3 :bl 1
                     :ec 11 :el 1}
                   (:bracket-tuple @{:bc 4 :bl 1
                                     :ec 11 :el 1}
                                   (:keyword @{:bc 5 :bl 1
                                               :ec 7 :el 1} ":a")
                                   (:whitespace @{:bc 7 :bl 1
                                                  :ec 8 :el 1} " ")
                                   (:keyword @{:bc 8 :bl 1
                                               :ec 10 :el 1} ":b"))))

  )

(def j/loc-top-level-ast
  (put (table ;(kvs j/loc-grammar))
       :main ~(sequence (line) (column)
                        :input
                        (line) (column))))

(defn j/par
  [src &opt start single]
  (default start 0)
  (if single
    (if-let [[bl bc tree el ec]
             (peg/match j/loc-top-level-ast src start)]
      @[:code (j/make-attrs bl bc el ec) tree]
      @[:code])
    (if-let [captures (peg/match j/loc-grammar src start)]
      (let [[bl bc] (slice captures 0 2)
            [el ec] (slice captures -3)
            trees (array/slice captures 2 -3)]
        (array/insert trees 0
                      :code (j/make-attrs bl bc el ec)))
      @[:code])))

# XXX: backward compatibility
(def j/ast j/par)

(comment

  (j/par "(+ 1 1)")
  # =>
  '@[:code @{:bc 1 :bl 1
             :ec 8 :el 1}
     (:tuple @{:bc 1 :bl 1
               :ec 8 :el 1}
             (:symbol @{:bc 2 :bl 1
                        :ec 3 :el 1} "+")
             (:whitespace @{:bc 3 :bl 1
                            :ec 4 :el 1} " ")
             (:number @{:bc 4 :bl 1
                        :ec 5 :el 1} "1")
             (:whitespace @{:bc 5 :bl 1
                            :ec 6 :el 1} " ")
             (:number @{:bc 6 :bl 1
                        :ec 7 :el 1} "1"))]

  )

(defn j/gen*
  [an-ast buf]
  (case (first an-ast)
    :code
    (each elt (drop 2 an-ast)
      (j/gen* elt buf))
    #
    :buffer
    (buffer/push-string buf (in an-ast 2))
    :comment
    (buffer/push-string buf (in an-ast 2))
    :constant
    (buffer/push-string buf (in an-ast 2))
    :keyword
    (buffer/push-string buf (in an-ast 2))
    :long-buffer
    (buffer/push-string buf (in an-ast 2))
    :long-string
    (buffer/push-string buf (in an-ast 2))
    :number
    (buffer/push-string buf (in an-ast 2))
    :string
    (buffer/push-string buf (in an-ast 2))
    :symbol
    (buffer/push-string buf (in an-ast 2))
    :whitespace
    (buffer/push-string buf (in an-ast 2))
    #
    :array
    (do
      (buffer/push-string buf "@(")
      (each elt (drop 2 an-ast)
        (j/gen* elt buf))
      (buffer/push-string buf ")"))
    :bracket-array
    (do
      (buffer/push-string buf "@[")
      (each elt (drop 2 an-ast)
        (j/gen* elt buf))
      (buffer/push-string buf "]"))
    :bracket-tuple
    (do
      (buffer/push-string buf "[")
      (each elt (drop 2 an-ast)
        (j/gen* elt buf))
      (buffer/push-string buf "]"))
    :tuple
    (do
      (buffer/push-string buf "(")
      (each elt (drop 2 an-ast)
        (j/gen* elt buf))
      (buffer/push-string buf ")"))
    :struct
    (do
      (buffer/push-string buf "{")
      (each elt (drop 2 an-ast)
        (j/gen* elt buf))
      (buffer/push-string buf "}"))
    :table
    (do
      (buffer/push-string buf "@{")
      (each elt (drop 2 an-ast)
        (j/gen* elt buf))
      (buffer/push-string buf "}"))
    #
    :fn
    (do
      (buffer/push-string buf "|")
      (each elt (drop 2 an-ast)
        (j/gen* elt buf)))
    :quasiquote
    (do
      (buffer/push-string buf "~")
      (each elt (drop 2 an-ast)
        (j/gen* elt buf)))
    :quote
    (do
      (buffer/push-string buf "'")
      (each elt (drop 2 an-ast)
        (j/gen* elt buf)))
    :splice
    (do
      (buffer/push-string buf ";")
      (each elt (drop 2 an-ast)
        (j/gen* elt buf)))
    :unquote
    (do
      (buffer/push-string buf ",")
      (each elt (drop 2 an-ast)
        (j/gen* elt buf)))
    ))

(defn j/gen
  [an-ast]
  (let [buf @""]
    (j/gen* an-ast buf)
    # XXX: leave as buffer?
    (string buf)))

# XXX: backward compatibility
(def j/code j/gen)

(comment

  (j/gen
    [:code])
  # =>
  ""

  (j/gen
    '(:whitespace @{:bc 1 :bl 1
                    :ec 2 :el 1} " "))
  # =>
  " "

  (j/gen
    '(:buffer @{:bc 1 :bl 1
                :ec 12 :el 1} "@\"a buffer\""))
  # =>
  `@"a buffer"`

  (j/gen
    '@[:code @{:bc 1 :bl 1
               :ec 8 :el 1}
       (:tuple @{:bc 1 :bl 1
                 :ec 8 :el 1}
               (:symbol @{:bc 2 :bl 1
                          :ec 3 :el 1} "+")
               (:whitespace @{:bc 3 :bl 1
                              :ec 4 :el 1} " ")
               (:number @{:bc 4 :bl 1
                          :ec 5 :el 1} "1")
               (:whitespace @{:bc 5 :bl 1
                              :ec 6 :el 1} " ")
               (:number @{:bc 6 :bl 1
                          :ec 7 :el 1} "1"))])
  # =>
  "(+ 1 1)"

  )

(comment

  (def src "{:x  :y \n :z  [:a  :b    :c]}")

  (j/gen (j/par src))
  # =>
  src

  )

(comment

  (comment

    (let [src (slurp (string (os/getenv "HOME")
                             "/src/janet/src/boot/boot.janet"))]
      (= (string src)
         (j/gen (j/par src))))

    )

  )

########################################################################

# based on code by corasaurus-hex

# `slice` doesn't necessarily preserve the input type

# XXX: differs from clojure's behavior
#      e.g. (butlast [:a]) would yield nil(?!) in clojure
(defn j/butlast
  [indexed]
  (if (empty? indexed)
    nil
    (if (tuple? indexed)
      (tuple/slice indexed 0 -2)
      (array/slice indexed 0 -2))))

(comment

  (j/butlast @[:a :b :c])
  # =>
  @[:a :b]

  (j/butlast [:a])
  # =>
  []

  )

(defn j/rest
  [indexed]
  (if (empty? indexed)
    nil
    (if (tuple? indexed)
      (tuple/slice indexed 1 -1)
      (array/slice indexed 1 -1))))

(comment

  (j/rest [:a :b :c])
  # =>
  [:b :c]

  (j/rest @[:a])
  # =>
  @[]

  )

# XXX: can pass in array - will get back tuple
(defn j/tuple-push
  [tup x & xs]
  (if tup
    [;tup x ;xs]
    [x ;xs]))

(comment

  (j/tuple-push [:a :b] :c)
  # =>
  [:a :b :c]

  (j/tuple-push nil :a)
  # =>
  [:a]

  (j/tuple-push @[] :a)
  # =>
  [:a]

  )

(defn j/to-entries
  [val]
  (if (dictionary? val)
    (pairs val)
    val))

(comment

  (sort (j/to-entries {:a 1 :b 2}))
  # =>
  @[[:a 1] [:b 2]]

  (j/to-entries {})
  # =>
  @[]

  (j/to-entries @{:a 1})
  # =>
  @[[:a 1]]

  # XXX: leaving non-dictionaries alone and passing through...
  #      is this desirable over erroring?
  (j/to-entries [:a :b :c])
  # =>
  [:a :b :c]

  )

# XXX: when xs is empty, "all" becomes nil
(defn j/first-rest-maybe-all
  [xs]
  (if (or (nil? xs) (empty? xs))
    [nil nil nil]
    [(first xs) (j/rest xs) xs]))

(comment

  (j/first-rest-maybe-all [:a :b])
  # =>
  [:a [:b] [:a :b]]

  (j/first-rest-maybe-all @[:a])
  # =>
  [:a @[] @[:a]]

  (j/first-rest-maybe-all [])
  # =>
  [nil nil nil]

  # XXX: is this what we want?
  (j/first-rest-maybe-all nil)
  # =>
  [nil nil nil]

  )

########################################################################

(defn j/zipper
  ``
  Returns a new zipper consisting of two elements:

  * `a-root` - the passed in root node.
  * `state` - table of info about node's z-location in the tree with keys:
    * `:ls` - left siblings
    * `:pnodes` - path of nodes from root to current z-location
    * `:pstate` - parent node's state
    * `:rs` - right siblings
    * `:changed?` - indicates whether "editing" has occured

  `state` has a prototype table with four functions:

  * :branch? - fn that tests if a node is a branch (has children)
  * :children - fn that returns the child nodes for the given branch.
  * :make-node - fn that takes a node + children and returns a new branch
    node with the same.
  * :make-state - fn for creating a new state
  ``
  [a-root branch?-fn children-fn make-node-fn]
  #
  (defn make-state_
    [&opt ls_ rs_ pnodes_ pstate_ changed?_]
    (table/setproto @{:ls ls_
                      :pnodes pnodes_
                      :pstate pstate_
                      :rs rs_
                      :changed? changed?_}
                    @{:branch? branch?-fn
                      :children children-fn
                      :make-node make-node-fn
                      :make-state make-state_}))
  #
  [a-root (make-state_)])

(comment

  # XXX

  )

# ds - data structure
(defn j/ds-zip
  ``
  Returns a zipper for nested data structures (tuple/array/table/struct),
  given a root data structure.
  ``
  [ds]
  (j/zipper ds
          |(or (dictionary? $) (indexed? $))
          j/to-entries
          (fn [p xs] xs)))

(comment

  (def a-node
    [:x [:y :z]])

  (def [the-node the-state]
    (j/ds-zip a-node))

  the-node
  # =>
  a-node

  # merge is used to "remove" the prototype table of `st`
  (merge {} the-state)
  # =>
  @{}

  )

(defn j/node
  "Returns the node at `zloc`."
  [zloc]
  (get zloc 0))

(comment

  (j/node (j/ds-zip [:a :b [:x :y]]))
  # =>
  [:a :b [:x :y]]

  )

(defn j/state
  "Returns the state for `zloc`."
  [zloc]
  (get zloc 1))

(comment

  # merge is used to "remove" the prototype table of `st`
  (merge {}
         (-> (j/ds-zip [:a [:b [:x :y]]])
             j/state))
  # =>
  @{}

  )

(defn j/branch?
  ``
  Returns true if the node at `zloc` is a branch.
  Returns false otherwise.
  ``
  [zloc]
  (((j/state zloc) :branch?) (j/node zloc)))

(comment

  (j/branch? (j/ds-zip [:a :b [:x :y]]))
  # =>
  true

  )

(defn j/children
  ``
  Returns children for a branch node at `zloc`.
  Otherwise throws an error.
  ``
  [zloc]
  (if (j/branch? zloc)
    (((j/state zloc) :children) (j/node zloc))
    (error "Called `children` on a non-branch zloc")))

(comment

  (j/children (j/ds-zip [:a :b [:x :y]]))
  # =>
  [:a :b [:x :y]]

  )

(defn j/make-state
  ``
  Convenience function for calling the :make-state function for `zloc`.
  ``
  [zloc &opt ls rs pnodes pstate changed?]
  (((j/state zloc) :make-state) ls rs pnodes pstate changed?))

(comment

  # merge is used to "remove" the prototype table of `st`
  (merge {}
         (j/make-state (j/ds-zip [:a :b [:x :y]])))
  # =>
  @{}

  )

(defn j/down
  ``
  Moves down the tree, returning the leftmost child z-location of
  `zloc`, or nil if there are no children.
  ``
  [zloc]
  (when (j/branch? zloc)
    (let [[z-node st] zloc
          [k rest-kids kids]
          (j/first-rest-maybe-all (j/children zloc))]
      (when kids
        [k
         (j/make-state zloc
                     []
                     rest-kids
                     (if (not (empty? st))
                       (j/tuple-push (get st :pnodes) z-node)
                       [z-node])
                     st
                     (get st :changed?))]))))

(comment

  (j/node (j/down (j/ds-zip [:a :b [:x :y]])))
  # =>
  :a

  (-> (j/ds-zip [:a :b [:x :y]])
      j/down
      j/branch?)
  # =>
  false

  (try
    (-> (j/ds-zip [:a])
        j/down
        j/children)
    ([e] e))
  # =>
  "Called `children` on a non-branch zloc"

  (deep=
    #
    (merge {}
           (-> [:a [:b [:x :y]]]
               j/ds-zip
               j/down
               j/state))
    #
    '@{:ls ()
       :pnodes ((:a (:b (:x :y))))
       :pstate @{}
       :rs ((:b (:x :y)))})
  # =>
  true

  )

(defn j/right
  ``
  Returns the z-location of the right sibling of the node
  at `zloc`, or nil if there is no such sibling.
  ``
  [zloc]
  (let [[z-node st] zloc
        {:ls ls :rs rs} st
        [r rest-rs rs] (j/first-rest-maybe-all rs)]
    (when (and (not (empty? st)) rs)
      [r
       (j/make-state zloc
                   (j/tuple-push ls z-node)
                   rest-rs
                   (get st :pnodes)
                   (get st :pstate)
                   (get st :changed?))])))

(comment

  (-> (j/ds-zip [:a :b])
      j/down
      j/right
      j/node)
  # =>
  :b

  (-> (j/ds-zip [:a])
      j/down
      j/right)
  # =>
  nil

  )

(defn j/make-node
  ``
  Returns a branch node, given `zloc`, `a-node` and `kids`.
  ``
  [zloc a-node kids]
  (((j/state zloc) :make-node) a-node kids))

(comment

  (j/make-node (j/ds-zip [:a :b [:x :y]])
             [:a :b] [:x :y])
  # =>
  [:x :y]

  )

(defn j/up
  ``
  Moves up the tree, returning the parent z-location of `zloc`,
  or nil if at the root z-location.
  ``
  [zloc]
  (let [[z-node st] zloc
        {:ls ls
         :pnodes pnodes
         :pstate pstate
         :rs rs
         :changed? changed?} st]
    (when pnodes
      (let [pnode (last pnodes)]
        (if changed?
          [(j/make-node zloc pnode [;ls z-node ;rs])
           (j/make-state zloc
                       (get pstate :ls)
                       (get pstate :rs)
                       (get pstate :pnodes)
                       (get pstate :pstate)
                       true)]
          [pnode pstate])))))

(comment

  (def m-zip
    (j/ds-zip [:a :b [:x :y]]))

  (deep=
    (-> m-zip
        j/down
        j/up)
    m-zip)
  # =>
  true

  (deep=
    (-> m-zip
        j/down
        j/right
        j/right
        j/down
        j/up
        j/up)
    m-zip)
  # =>
  true

  )

# XXX: used by `root` and `df-next`
(defn j/end?
  "Returns true if `zloc` represents the end of a depth-first walk."
  [zloc]
  (= :end (j/state zloc)))

(defn j/root
  ``
  Moves all the way up the tree for `zloc` and returns the node at
  the root z-location.
  ``
  [zloc]
  (if (j/end? zloc)
    (j/node zloc)
    (if-let [p (j/up zloc)]
      (j/root p)
      (j/node zloc))))

(comment

  (def a-zip
    (j/ds-zip [:a :b [:x :y]]))

  (j/node a-zip)
  # =>
  (-> a-zip
      j/down
      j/right
      j/right
      j/down
      j/root)

  )

(defn j/df-next
  ``
  Moves to the next z-location, depth-first.  When the end is
  reached, returns a special z-location detectable via `end?`.
  Does not move if already at the end.
  ``
  [zloc]
  #
  (defn recur
    [a-loc]
    (if (j/up a-loc)
      (or (j/right (j/up a-loc))
          (recur (j/up a-loc)))
      [(j/node a-loc) :end]))
  #
  (if (j/end? zloc)
    zloc
    (or (and (j/branch? zloc) (j/down zloc))
        (j/right zloc)
        (recur zloc))))

(comment

  (def a-zip
    (j/ds-zip [:a :b [:x]]))

  (j/node (j/df-next a-zip))
  # =>
  :a

  (-> a-zip
      j/df-next
      j/df-next
      j/node)
  # =>
  :b

  (-> a-zip
      j/df-next
      j/df-next
      j/df-next
      j/df-next
      j/df-next
      j/end?)
  # =>
  true

  )

(defn j/replace
  "Replaces existing node at `zloc` with `a-node`, without moving."
  [zloc a-node]
  (let [[_ st] zloc]
    [a-node
     (j/make-state zloc
                 (get st :ls)
                 (get st :rs)
                 (get st :pnodes)
                 (get st :pstate)
                 true)]))

(comment

  (-> (j/ds-zip [:a :b [:x :y]])
      j/down
      (j/replace :w)
      j/root)
  # =>
  [:w :b [:x :y]]

  (-> (j/ds-zip [:a :b [:x :y]])
      j/down
      j/right
      j/right
      j/down
      (j/replace :w)
      j/root)
  # =>
  [:a :b [:w :y]]

  )

(defn j/edit
  ``
  Replaces the node at `zloc` with the value of `(f node args)`,
  where `node` is the node associated with `zloc`.
  ``
  [zloc f & args]
  (j/replace zloc
           (apply f (j/node zloc) args)))

(comment

  (-> (j/ds-zip [1 2 [8 9]])
      j/down
      (j/edit inc)
      j/root)
  # =>
  [2 2 [8 9]]

  (-> (j/ds-zip [1 2 [8 9]])
      j/down
      (j/edit inc)
      j/right
      (j/edit inc)
      j/right
      j/down
      (j/edit dec)
      j/right
      (j/edit dec)
      j/root)
  # =>
  [2 3 [7 8]]

  )

(defn j/insert-child
  ``
  Inserts `child` as the leftmost child of the node at `zloc`,
  without moving.
  ``
  [zloc child]
  (j/replace zloc
           (j/make-node zloc
                      (j/node zloc)
                      [child ;(j/children zloc)])))

(comment

  (-> (j/ds-zip [:a :b [:x :y]])
      (j/insert-child :c)
      j/root)
  # =>
  [:c :a :b [:x :y]]

  )

(defn j/append-child
  ``
  Appends `child` as the rightmost child of the node at `zloc`,
  without moving.
  ``
  [zloc child]
  (j/replace zloc
           (j/make-node zloc
                      (j/node zloc)
                      [;(j/children zloc) child])))

(comment

  (-> (j/ds-zip [:a :b [:x :y]])
      (j/append-child :c)
      j/root)
  # =>
  [:a :b [:x :y] :c]

  )

(defn j/rightmost
  ``
  Returns the z-location of the rightmost sibling of the node at
  `zloc`, or the current node's z-location if there are none to the
  right.
  ``
  [zloc]
  (let [[z-node st] zloc
        {:ls ls :rs rs} st]
    (if (and (not (empty? st))
             (indexed? rs)
             (not (empty? rs)))
      [(last rs)
       (j/make-state zloc
                   (j/tuple-push ls z-node ;(j/butlast rs))
                   []
                   (get st :pnodes)
                   (get st :pstate)
                   (get st :changed?))]
      zloc)))

(comment

  (-> (j/ds-zip [:a :b [:x :y]])
      j/down
      j/rightmost
      j/node)
  # =>
  [:x :y]

  )

(defn j/remove
  ``
  Removes the node at `zloc`, returning the z-location that would have
  preceded it in a depth-first walk.
  Throws an error if called at the root z-location.
  ``
  [zloc]
  (let [[z-node st] zloc
        {:ls ls
         :pnodes pnodes
         :pstate pstate
         :rs rs} st]
    #
    (defn recur
      [a-zloc]
      (if-let [child (and (j/branch? a-zloc) (j/down a-zloc))]
        (recur (j/rightmost child))
        a-zloc))
    #
    (if (not (empty? st))
      (if (pos? (length ls))
        (recur [(last ls)
                (j/make-state zloc
                            (j/butlast ls)
                            rs
                            pnodes
                            pstate
                            true)])
        [(j/make-node zloc (last pnodes) rs)
         (j/make-state zloc
                     (get pstate :ls)
                     (get pstate :rs)
                     (get pstate :pnodes)
                     (get pstate :pstate)
                     true)])
      (error "Called `remove` at root"))))

(comment

  (-> (j/ds-zip [:a :b [:x :y]])
      j/down
      j/right
      j/remove
      j/node)
  # =>
  :a

  (try
    (j/remove (j/ds-zip [:a :b [:x :y]]))
    ([e] e))
  # =>
  "Called `remove` at root"

  )

(defn j/left
  ``
  Returns the z-location of the left sibling of the node
  at `zloc`, or nil if there is no such sibling.
  ``
  [zloc]
  (let [[z-node st] zloc
        {:ls ls :rs rs} st]
    (when (and (not (empty? st))
               (indexed? ls)
               (not (empty? ls)))
      [(last ls)
       (j/make-state zloc
                   (j/butlast ls)
                   [z-node ;rs]
                   (get st :pnodes)
                   (get st :pstate)
                   (get st :changed?))])))

(comment

  (-> (j/ds-zip [:a :b :c])
      j/down
      j/right
      j/right
      j/left
      j/node)
  # =>
  :b

  (-> (j/ds-zip [:a])
      j/down
      j/left)
  # =>
  nil

  )

(defn j/df-prev
  ``
  Moves to the previous z-location, depth-first.
  If already at the root, returns nil.
  ``
  [zloc]
  #
  (defn recur
    [a-zloc]
    (if-let [child (and (j/branch? a-zloc)
                        (j/down a-zloc))]
      (recur (j/rightmost child))
      a-zloc))
  #
  (if-let [left-loc (j/left zloc)]
    (recur left-loc)
    (j/up zloc)))

(comment

  (-> (j/ds-zip [:a :b [:x :y]])
      j/down
      j/right
      j/df-prev
      j/node)
  # =>
  :a

  (-> (j/ds-zip [:a :b [:x :y]])
      j/down
      j/right
      j/right
      j/down
      j/df-prev
      j/node)
  # =>
  [:x :y]

  )

(defn j/insert-right
  ``
  Inserts `a-node` as the right sibling of the node at `zloc`,
  without moving.
  ``
  [zloc a-node]
  (let [[z-node st] zloc
        {:ls ls :rs rs} st]
    (if (not (empty? st))
      [z-node
       (j/make-state zloc
                   ls
                   [a-node ;rs]
                   (get st :pnodes)
                   (get st :pstate)
                   true)]
      (error "Called `insert-right` at root"))))

(comment

  (def a-zip
    (j/ds-zip [:a :b [:x :y]]))

  (-> a-zip
      j/down
      (j/insert-right :z)
      j/root)
  # =>
  [:a :z :b [:x :y]]

  (try
    (j/insert-right a-zip :e)
    ([e] e))
  # =>
  "Called `insert-right` at root"

  )

(defn j/insert-left
  ``
  Inserts `a-node` as the left sibling of the node at `zloc`,
  without moving.
  ``
  [zloc a-node]
  (let [[z-node st] zloc
        {:ls ls :rs rs} st]
    (if (not (empty? st))
      [z-node
       (j/make-state zloc
                   (j/tuple-push ls a-node)
                   rs
                   (get st :pnodes)
                   (get st :pstate)
                   true)]
      (error "Called `insert-left` at root"))))

(comment

  (def a-zip
    (j/ds-zip [:a :b [:x :y]]))

  (-> a-zip
      j/down
      (j/insert-left :z)
      j/root)
  # =>
  [:z :a :b [:x :y]]

  (try
    (j/insert-left a-zip :e)
    ([e] e))
  # =>
  "Called `insert-left` at root"

  )

(defn j/rights
  "Returns siblings to the right of `zloc`."
  [zloc]
  (when-let [st (j/state zloc)]
    (get st :rs)))

(comment

  (-> (j/ds-zip [:a :b [:x :y]])
      j/down
      j/rights)
  # =>
  [:b [:x :y]]

  )

(defn j/lefts
  "Returns siblings to the left of `zloc`."
  [zloc]
  (if-let [st (j/state zloc)
           ls (get st :ls)]
    ls
    []))

(comment

  (-> (j/ds-zip [:a :b])
      j/down
      j/lefts)
  # =>
  []

  (-> (j/ds-zip [:a :b [:x :y]])
      j/down
      j/right
      j/right
      j/lefts)
  # =>
  [:a :b]

  )

(defn j/leftmost
  ``
  Returns the z-location of the leftmost sibling of the node at `zloc`,
  or the current node's z-location if there are no siblings to the left.
  ``
  [zloc]
  (let [[z-node st] zloc
        {:ls ls :rs rs} st]
    (if (and (not (empty? st))
             (indexed? ls)
             (not (empty? ls)))
      [(first ls)
       (j/make-state zloc
                   []
                   [;(j/rest ls) z-node ;rs]
                   (get st :pnodes)
                   (get st :pstate)
                   (get st :changed?))]
      zloc)))

(comment

  (-> (j/ds-zip [:a :b [:x :y]])
      j/down
      j/leftmost
      j/node)
  # =>
  :a

  (-> (j/ds-zip [:a :b [:x :y]])
      j/down
      j/rightmost
      j/leftmost
      j/node)
  # =>
  :a

  )

(defn j/path
  "Returns the path of nodes that lead to `zloc` from the root node."
  [zloc]
  (when-let [st (j/state zloc)]
    (get st :pnodes)))

(comment

  (j/path (j/ds-zip [:a :b [:x :y]]))
  # =>
  nil

  (-> (j/ds-zip [:a :b [:x :y]])
      j/down
      j/path)
  # =>
  [[:a :b [:x :y]]]

  (-> (j/ds-zip [:a :b [:x :y]])
      j/down
      j/right
      j/right
      j/down
      j/path)
  # =>
  [[:a :b [:x :y]] [:x :y]]

  )

(defn j/right-until
  ``
  Try to move right from `zloc`, calling `pred` for each
  right sibling.  If the `pred` call has a truthy result,
  return the corresponding right sibling.
  Otherwise, return nil.
  ``
  [zloc pred]
  (when-let [right-sib (j/right zloc)]
    (if (pred right-sib)
      right-sib
      (j/right-until right-sib pred))))

(comment

  (-> [:code
       [:tuple
        [:comment "# hi there"] [:whitespace "\n"]
        [:symbol "+"] [:whitespace " "]
        [:number "1"] [:whitespace " "]
        [:number "2"]]]
      j/ds-zip
      j/down
      j/right
      j/down
      (j/right-until |(match (j/node $)
                      [:comment]
                      false
                      #
                      [:whitespace]
                      false
                      #
                      true))
      j/node)
  # =>
  [:symbol "+"]

  )

(defn j/left-until
  ``
  Try to move left from `zloc`, calling `pred` for each
  left sibling.  If the `pred` call has a truthy result,
  return the corresponding left sibling.
  Otherwise, return nil.
  ``
  [zloc pred]
  (when-let [left-sib (j/left zloc)]
    (if (pred left-sib)
      left-sib
      (j/left-until left-sib pred))))

(comment

  (-> [:code
       [:tuple
        [:comment "# hi there"] [:whitespace "\n"]
        [:symbol "+"] [:whitespace " "]
        [:number "1"] [:whitespace " "]
        [:number "2"]]]
      j/ds-zip
      j/down
      j/right
      j/down
      j/rightmost
      (j/left-until |(match (j/node $)
                     [:comment]
                     false
                     #
                     [:whitespace]
                     false
                     #
                     true))
      j/node)
  # =>
  [:number "1"]

  )

(defn j/search-from
  ``
  Successively call `pred` on z-locations starting at `zloc`
  in depth-first order.  If a call to `pred` returns a
  truthy value, return the corresponding z-location.
  Otherwise, return nil.
  ``
  [zloc pred]
  (if (pred zloc)
    zloc
    (when-let [next-zloc (j/df-next zloc)]
      (when (j/end? next-zloc)
        (break nil))
      (j/search-from next-zloc pred))))

(comment

  (-> (j/ds-zip [:a :b :c])
      j/down
      (j/search-from |(match (j/node $)
                      :b
                      true))
      j/node)
  # =>
  :b

  (-> (j/ds-zip [:a :b :c])
      j/down
      (j/search-from |(match (j/node $)
                      :d
                      true)))
  # =>
  nil

  (-> (j/ds-zip [:a :b :c])
      j/down
      (j/search-from |(match (j/node $)
                      :a
                      true))
      j/node)
  # =>
  :a

  )

(defn j/search-after
  ``
  Successively call `pred` on z-locations starting after
  `zloc` in depth-first order.  If a call to `pred` returns a
  truthy value, return the corresponding z-location.
  Otherwise, return nil.
  ``
  [zloc pred]
  (when (j/end? zloc)
    (break nil))
  (when-let [next-zloc (j/df-next zloc)]
    (if (pred next-zloc)
      next-zloc
      (j/search-after next-zloc pred))))

(comment

  (-> (j/ds-zip [:b :a :b])
      j/down
      (j/search-after |(match (j/node $)
                       :b
                       true))
      j/left
      j/node)
  # =>
  :a

  (-> (j/ds-zip [:b :a :b])
      j/down
      (j/search-after |(match (j/node $)
                       :d
                       true)))
  # =>
  nil

  (-> (j/ds-zip [:a [:b :c [2 [3 :smile] 5]]])
      (j/search-after |(match (j/node $)
                       [_ :smile]
                       true))
      j/down
      j/node)
  # =>
  3

  )

(defn j/unwrap
  ``
  If the node at `zloc` is a branch node, "unwrap" its children in
  place.  If `zloc`'s node is not a branch node, do nothing.

  Throws an error if `zloc` corresponds to a top-most container.
  ``
  [zloc]
  (unless (j/branch? zloc)
    (break zloc))
  #
  (when (empty? (j/state zloc))
    (error "Called `unwrap` at root"))
  #
  (def kids (j/children zloc))
  (var i (dec (length kids)))
  (var curr-zloc zloc)
  (while (<= 0 i) # right to left
    (set curr-zloc
         (j/insert-right curr-zloc (get kids i)))
    (-- i))
  # try to end up at a sensible spot
  (set curr-zloc
       (j/remove curr-zloc))
  (if-let [ret-zloc (j/right curr-zloc)]
    ret-zloc
    curr-zloc))

(comment

  (-> (j/ds-zip [:a :b [:x :y]])
      j/down
      j/right
      j/right
      j/unwrap
      j/root)
  # =>
  [:a :b :x :y]

  (-> (j/ds-zip [:a :b [:x :y]])
      j/down
      j/unwrap
      j/root)
  # =>
  [:a :b [:x :y]]

  (-> (j/ds-zip [[:a]])
      j/down
      j/unwrap
      j/root)
  # =>
  [:a]

  (-> (j/ds-zip [[:a :b] [:x :y]])
      j/down
      j/down
      j/remove
      j/unwrap
      j/root)
  # =>
  [:b [:x :y]]

  (try
    (-> (j/ds-zip [:a :b [:x :y]])
        j/unwrap)
    ([e] e))
  # =>
  "Called `unwrap` at root"

  )

(defn j/wrap
  ``
  Replace nodes from `start-zloc` through `end-zloc` with a single
  node of the same type as `wrap-node` containing the nodes from
  `start-zloc` through `end-zloc`.

  If `end-zloc` is not specified, just wrap `start-zloc`.

  The caller is responsible for ensuring the value of `end-zloc`
  is somewhere to the right of `start-zloc`.  Throws an error if
  an inappropriate value is specified for `end-zloc`.
  ``
  [start-zloc wrap-node &opt end-zloc]
  (default end-zloc start-zloc)
  #
  # 1. collect all nodes to wrap
  #
  (def kids @[])
  (var cur-zloc start-zloc)
  (while (and cur-zloc
              # XXX: expensive?
              (not (deep= (j/node cur-zloc)
                          (j/node end-zloc)))) # left to right
    (array/push kids (j/node cur-zloc))
    (set cur-zloc (j/right cur-zloc)))
  (when (nil? cur-zloc)
    (error "Called `wrap` with invalid value for `end-zloc`."))
  # also collect the last node
  (array/push kids (j/node end-zloc))
  #
  # 2. replace locations that will be removed with non-container nodes
  #
  (def dummy-node
    (j/make-node start-zloc wrap-node (tuple)))
  (set cur-zloc start-zloc)
  # trying to do this together in step 1 is not straight-forward
  # because the desired exiting condition for the while loop depends
  # on cur-zloc becoming end-zloc -- if `replace` were to be used
  # there, the termination condition never gets fulfilled properly.
  (for i 0 (dec (length kids)) # left to right again
    (set cur-zloc
         (-> (j/replace cur-zloc dummy-node)
             j/right)))
  (set cur-zloc
       (j/replace cur-zloc dummy-node))
  #
  # 3. remove all relevant locations
  #
  (def new-node
    (j/make-node start-zloc wrap-node (tuple ;kids)))
  (for i 0 (dec (length kids)) # right to left
    (set cur-zloc
         (j/remove cur-zloc)))
  # 4. put the new container node into place
  (j/replace cur-zloc new-node))

(comment

  (def start-zloc
    (-> (j/ds-zip [:a [:b] :c :x])
        j/down
        j/right))

  (j/node start-zloc)
  # =>
  [:b]

  (-> (j/wrap start-zloc [])
      j/root)
  # =>
  [:a [[:b]] :c :x]

  (def end-zloc
    (j/right start-zloc))

  (j/node end-zloc)
  # =>
  :c

  (-> (j/wrap start-zloc [] end-zloc)
      j/root)
  # =>
  [:a [[:b] :c] :x]

  (try
    (-> (j/wrap end-zloc [] start-zloc)
        j/root)
    ([e] e))
  # =>
  "Called `wrap` with invalid value for `end-zloc`."

  )

########################################################################

(defn j/has-children?
  ``
  Returns true if `a-node` can have children.
  Returns false if `a-node` cannot have children.
  ``
  [a-node]
  (when-let [[head] a-node]
    (truthy? (get {:code true
                   :fn true
                   :quasiquote true
                   :quote true
                   :splice true
                   :unquote true
                   :array true
                   :tuple true
                   :bracket-array true
                   :bracket-tuple true
                   :table true
                   :struct true}
                  head))))

(comment

  (j/has-children?
    [:tuple @{}
     [:symbol @{} "+"] [:whitespace @{} " "]
     [:number @{} "1"] [:whitespace @{} " "]
     [:number @{} "2"]])
  # =>
  true

  (j/has-children? [:number @{} "8"])
  # =>
  false

  )

(defn j/zip
  ``
  Returns a zipper location (zloc or z-location) for a tree
  representing Janet code.
  ``
  [a-tree]
  (defn branch?_
    [a-node]
    (truthy? (and (indexed? a-node)
                  (not (empty? a-node))
                  (j/has-children? a-node))))
  #
  (defn children_
    [a-node]
    (if (branch?_ a-node)
      (slice a-node 2)
      (error "Called `children` on a non-branch node")))
  #
  (defn make-node_
    [a-node kids]
    [(first a-node) (get a-node 1) ;kids])
  #
  (j/zipper a-tree branch?_ children_ make-node_))

(comment

  (def root-node
    @[:code @{} [:number @{} "8"]])

  (def [the-node the-state]
    (j/zip root-node))

  the-node
  # =>
  root-node

  # merge is used to "remove" the prototype table of `st`
  (merge {} the-state)
  # =>
  @{}

  )

(defn j/attrs
  ``
  Return the attributes table for the node of a z-location.  The
  attributes table contains at least bounds of the node by 1-based line
  and column numbers.
  ``
  [zloc]
  (get (j/node zloc) 1))

(comment

  (-> (j/par "(+ 1 3)")
      j/zip
      j/down
      j/attrs)
  # =>
  @{:bc 1 :bl 1 :ec 8 :el 1}

  )

(defn j/zip-down
  ``
  Convenience function that returns a zipper which has
  already had `down` called on it.
  ``
  [a-tree]
  (-> (j/zip a-tree)
      j/down))

(comment

  (-> (j/par "(+ 1 3)")
      j/zip-down
      j/node)
  # =>
  [:tuple @{:bc 1 :bl 1 :ec 8 :el 1}
   [:symbol @{:bc 2 :bl 1 :ec 3 :el 1} "+"]
   [:whitespace @{:bc 3 :bl 1 :ec 4 :el 1} " "]
   [:number @{:bc 4 :bl 1 :ec 5 :el 1} "1"]
   [:whitespace @{:bc 5 :bl 1 :ec 6 :el 1} " "]
   [:number @{:bc 6 :bl 1 :ec 7 :el 1} "3"]]

  (-> (j/par "(/ 1 8)")
      j/zip-down
      j/root)
  # =>
  @[:code @{:bc 1 :bl 1 :ec 8 :el 1}
    [:tuple @{:bc 1 :bl 1 :ec 8 :el 1}
            [:symbol @{:bc 2 :bl 1 :ec 3 :el 1} "/"]
            [:whitespace @{:bc 3 :bl 1 :ec 4 :el 1} " "]
            [:number @{:bc 4 :bl 1 :ec 5 :el 1} "1"]
            [:whitespace @{:bc 5 :bl 1 :ec 6 :el 1} " "]
            [:number @{:bc 6 :bl 1 :ec 7 :el 1} "8"]]]

  )

# wsc == whitespace, comment
(defn j/right-skip-wsc
  ``
  Try to move right from `zloc`, skipping over whitespace
  and comment nodes.

  When at least one right move succeeds, return the z-location
  for the last successful right move destination.  Otherwise,
  return nil.
  ``
  [zloc]
  (j/right-until zloc
               |(match (j/node $)
                  [:whitespace]
                  false
                  #
                  [:comment]
                  false
                  #
                  true)))

(comment

  (-> (j/par (string "(# hi there\n"
                   "+ 1 2)"))
      j/zip-down
      j/down
      j/right-skip-wsc
      j/node)
  # =>
  [:symbol @{:bc 1 :bl 2 :ec 2 :el 2} "+"]

  (-> (j/par "(:a)")
      j/zip-down
      j/down
      j/right-skip-wsc)
  # =>
  nil

  )

(defn j/left-skip-wsc
  ``
  Try to move left from `zloc`, skipping over whitespace
  and comment nodes.

  When at least one left move succeeds, return the z-location
  for the last successful left move destination.  Otherwise,
  return nil.
  ``
  [zloc]
  (j/left-until zloc
              |(match (j/node $)
                 [:whitespace]
                 false
                 #
                 [:comment]
                 false
                 #
                 true)))

(comment

  (-> (j/par (string "(# hi there\n"
                   "+ 1 2)"))
      j/zip-down
      j/down
      j/right-skip-wsc
      j/right-skip-wsc
      j/left-skip-wsc
      j/node)
  # =>
  [:symbol @{:bc 1 :bl 2 :ec 2 :el 2} "+"]

  (-> (j/par "(:a)")
      j/zip-down
      j/down
      j/left-skip-wsc)
  # =>
  nil

  )

# ws == whitespace
(defn j/right-skip-ws
  ``
  Try to move right from `zloc`, skipping over whitespace
  nodes.

  When at least one right move succeeds, return the z-location
  for the last successful right move destination.  Otherwise,
  return nil.
  ``
  [zloc]
  (j/right-until zloc
               |(match (j/node $)
                  [:whitespace]
                  false
                  #
                  true)))

(comment

  (-> (j/par (string "( # hi there\n"
                   "+ 1 2)"))
      j/zip-down
      j/down
      j/right-skip-ws
      j/node)
  # =>
  [:comment @{:bc 3 :bl 1 :ec 13 :el 1} "# hi there"]

  (-> (j/par "(:a)")
      j/zip-down
      j/down
      j/right-skip-ws)
  # =>
  nil

  )

(defn j/left-skip-ws
  ``
  Try to move left from `zloc`, skipping over whitespace
  nodes.

  When at least one left move succeeds, return the z-location
  for the last successful left move destination.  Otherwise,
  return nil.
  ``
  [zloc]
  (j/left-until zloc
              |(match (j/node $)
                 [:whitespace]
                 false
                 #
                 true)))

(comment

  (-> (j/par (string "(# hi there\n"
                   "+ 1 2)"))
      j/zip-down
      j/down
      j/right
      j/right
      j/left-skip-ws
      j/node)
  # =>
  [:comment @{:bc 2 :bl 1 :ec 12 :el 1} "# hi there"]

  (-> (j/par "(:a)")
      j/zip-down
      j/down
      j/left-skip-ws)
  # =>
  nil

  )


(comment import ./verify :prefix "")
# XXX: try to put in file?  had trouble originally when working on
#      judge-gen.  may be will have more luck?
(def v/as-string
  ``
  # influenced by janet's tools/helper.janet

  (var _verify/start-time 0)
  (var _verify/end-time 0)
  (var _verify/test-results @[])

  (defmacro _verify/is
    [t-form e-form &opt name]
    (default name
      (string "test-" (inc (length _verify/test-results))))
    (with-syms [$ts $tr
                $es $er]
      ~(do
         (def [,$ts ,$tr] (protect (eval ',t-form)))
         (def [,$es ,$er] (protect (eval ',e-form)))
         (array/push _verify/test-results
                     {:expected-form ',e-form
                      :expected-value ,$er
                      :name ,name
                      :passed (if (and ,$ts ,$es)
                                (deep= ,$tr ,$er)
                                nil)
                      :test-form ',t-form
                      :test-value ,$tr
                      :type :is})
         ,name)))

  (defn _verify/start-tests
    []
    (set _verify/start-time (os/clock))
    (set _verify/test-results @[]))

  (defn _verify/end-tests
    []
    (set _verify/end-time (os/clock)))

  (defn _verify/print-color
    [msg color]
    # XXX: what if color doesn't match...
    (let [color-num (match color
                      :black 30
                      :blue 34
                      :cyan 36
                      :green 32
                      :magenta 35
                      :red 31
                      :white 37
                      :yellow 33)]
      (prin (string "\e[" color-num "m"
                    msg
                    "\e[0m"))))

  (defn _verify/dashes
    [&opt n]
    (default n 60)
    (string/repeat "-" n))

  (defn _verify/print-dashes
    [&opt n]
    (print (_verify/dashes n)))

  (defn _verify/print-form
    [form &opt color]
    (def buf @"")
    (with-dyns [:out buf]
      (printf "%m" form))
    (def msg (string/trimr buf))
    (print ":")
    (if color
      (_verify/print-color msg color)
      (prin msg))
    (print))

  (defn _verify/report
    []
    (var total-tests 0)
    (var total-passed 0)
    # analyze results
    (var passed 0)
    (var num-tests (length _verify/test-results))
    (var fails @[])
    (each test-result _verify/test-results
      (++ total-tests)
      (def {:passed test-passed} test-result)
      (if test-passed
        (do
          (++ passed)
          (++ total-passed))
        (array/push fails test-result)))
    # report any failures
    (var i 0)
    (each fail fails
      (def {:test-value test-value
            :expected-value expected-value
            :name test-name
            :passed test-passed
            :test-form test-form} fail)
      (++ i)
      (print)
      (prin "--(")
      (_verify/print-color i :cyan)
      (print ")--")
      (print)
      #
      (_verify/print-color "failed:" :yellow)
      (print)
      (_verify/print-color test-name :red)
      (print)
      #
      (print)
      (_verify/print-color "form" :yellow)
      (_verify/print-form test-form)
      #
      (print)
      (_verify/print-color "expected" :yellow)
      (_verify/print-form expected-value)
      #
      (print)
      (_verify/print-color "actual" :yellow)
      (_verify/print-form test-value :blue))
    (when (zero? (length fails))
      (print)
      (print "No tests failed."))
    # summarize totals
    (print)
    (_verify/print-dashes)
    (when (= 0 total-tests)
      (print "No tests found, so no judgements made.")
      (break true))
    (if (not= total-passed total-tests)
      (_verify/print-color total-passed :red)
      (_verify/print-color total-passed :green))
    (prin " of ")
    (_verify/print-color total-tests :green)
    (print " passed")
    (_verify/print-dashes)
    (when (not= total-passed total-tests)
      (os/exit 1)))
  ``)



# at its simplest, a test is expressed like:
#
# (comment
#
#   (+ 1 1)
#   # =>
#   2
#
#   )
#
# i.e. inside a comment form, a single test consists of:
#
# * a test expression        - `(+ 1 1)`
# * a test indicator         - `# =>`
# * an expected expression   - `2`
#
# there can be one or more tests within a comment form.

# ti == test indicator, which can look like any of:
#
# # =>
# # before =>
# # => after
# # before => after
#
# further constraint that neither `before` nor `after` should contain
# a hash character (#)

(defn r/find-test-indicator
  [zloc]
  (var label-left nil)
  (var label-right nil)
  [(j/right-until zloc
                  |(match (j/node $)
                     [:comment _ content]
                     (if-let [[l r]
                              (peg/match ~(sequence "#"
                                                    (capture (to "=>"))
                                                    "=>"
                                                    (capture (thru -1)))
                                         content)
                              no-hash-left (nil? (string/find "#" l))
                              no-hash-right (nil? (string/find "#" r))]
                       (do
                         (set label-left (string/trim l))
                         (set label-right (string/trim r))
                         true)
                       false)))
   label-left
   label-right])

(comment

  (def eol (if (= :windows (os/which)) "\r\n" "\n"))

  (def src
    (string "(+ 1 1)" eol
            "# =>"    eol
            "2"))

  (let [[zloc l r]
        (r/find-test-indicator (-> (j/par src)
                                 j/zip-down))]
    (and zloc
         (empty? l)
         (empty? r)))
  # =>
  true

  (def src
    (string "(+ 1 1)"     eol
            "# before =>" eol
            "2"))

  (let [[zloc l r]
        (r/find-test-indicator (-> (j/par src)
                                 j/zip-down))]
    (and zloc
         (= "before" l)
         (empty? r)))
  # =>
  true

  (def src
    (string "(+ 1 1)"    eol
            "# => after" eol
            "2"))

  (let [[zloc l r]
        (r/find-test-indicator (-> (j/par src)
                                 j/zip-down))]
    (and zloc
         (empty? l)
         (= "after" r)))
  # =>
  true

  )

(defn r/find-test-expr
  [ti-zloc]
  # check for appropriate conditions "before"
  (def before-zlocs @[])
  (var curr-zloc ti-zloc)
  (var found-before nil)
  # collect zlocs to the left of the test indicator up through the
  # first non-whitespace/comment one.  if there is a
  # non-whitespace/comment one, that is the test expression.
  (while curr-zloc
    (set curr-zloc (j/left curr-zloc))
    (when (nil? curr-zloc)
      (break))
    #
    (match (j/node curr-zloc)
      [:comment]
      (array/push before-zlocs curr-zloc)
      #
      [:whitespace]
      (array/push before-zlocs curr-zloc)
      #
      (do
        (set found-before true)
        (array/push before-zlocs curr-zloc)
        (break))))
  #
  (cond
    (nil? curr-zloc)
    :no-test-expression
    # if all collected zlocs (except the last one) are whitespace,
    # then the test expression has been located
    (and found-before
         (->> (slice before-zlocs 0 -2)
              (filter |(not (match (j/node $)
                              [:whitespace]
                              true)))
              length
              zero?))
    curr-zloc
    #
    :unexpected-result))

(comment

  (def eol (if (= :windows (os/which)) "\r\n" "\n"))

  (def src
    (string "(comment"         eol
            eol
            "  (def a 1)"      eol
            eol
            "  (put @{} :a 2)" eol
            "  # =>"           eol
            "  @{:a 2}"        eol
            eol
            "  )"))

  (def [ti-zloc _ _]
    (r/find-test-indicator (-> (j/par src)
                             j/zip-down
                             j/down)))

  (j/node ti-zloc)
  # =>
  [:comment @{:bc 3 :bl 6 :ec 7 :el 6} "# =>"]

  (def test-expr-zloc (r/find-test-expr ti-zloc))

  (j/node test-expr-zloc)
  # =>
  [:tuple @{:bc 3 :bl 5 :ec 17 :el 5}
   [:symbol @{:bc 4 :bl 5 :ec 7 :el 5} "put"]
   [:whitespace @{:bc 7 :bl 5 :ec 8 :el 5} " "]
   [:table @{:bc 8 :bl 5 :ec 11 :el 5}]
   [:whitespace @{:bc 11 :bl 5 :ec 12 :el 5} " "]
   [:keyword @{:bc 12 :bl 5 :ec 14 :el 5} ":a"]
   [:whitespace @{:bc 14 :bl 5 :ec 15 :el 5} " "]
   [:number @{:bc 15 :bl 5 :ec 16 :el 5} "2"]]

  (-> (j/left test-expr-zloc)
      j/node)
  # =>
  [:whitespace @{:bc 1 :bl 5 :ec 3 :el 5} "  "]

  )

(defn r/find-expected-expr
  [ti-zloc]
  (def after-zlocs @[])
  (var curr-zloc ti-zloc)
  (var found-comment nil)
  (var found-after nil)
  # collect zlocs to the right of the test indicator up through the
  # first non-whitespace/comment one.  if there is a
  # non-whitespace/comment one, that is the expression used to compute
  # the expected value.
  (while curr-zloc
    (set curr-zloc (j/right curr-zloc))
    (when (nil? curr-zloc)
      (break))
    #
    (match (j/node curr-zloc)
      [:comment]
      (do
        (set found-comment true)
        (break))
      #
      [:whitespace]
      (array/push after-zlocs curr-zloc)
      #
      (do
        (set found-after true)
        (array/push after-zlocs curr-zloc)
        (break))))
  #
  (cond
    (or (nil? curr-zloc)
        found-comment)
    :no-expected-expression
    # if there was a non-whitespace/comment zloc and the first zloc
    # "captured" represents eol (i.e. the first zloc to the right of
    # the test indicator), then there might be a an "expected
    # expression" that follows...
    (and found-after
         (match (j/node (first after-zlocs))
           [:whitespace _ "\n"]
           true
           [:whitespace _ "\r\n"]
           true))
    # starting on the line after the eol zloc, keep collected zlocs up
    # to (but not including) another eol zloc.  the first
    # non-whitespace zloc of the kept zlocs represents the "expected
    # expression".
    (if-let [from-next-line (drop 1 after-zlocs)
             before-eol-zloc (take-until |(match (j/node $)
                                            [:whitespace _ "\n"]
                                            true
                                            [:whitespace _ "\r\n"]
                                            true)
                                         from-next-line)
             target (->> before-eol-zloc
                         (filter |(match (j/node $)
                                    [:whitespace]
                                    false
                                    #
                                    true))
                         first)]
      target
      :no-expected-expression)
    #
    :unexpected-result))

(comment

  (def eol (if (= :windows (os/which)) "\r\n" "\n"))

  (def src
    (string "(comment"         eol
            eol
            "  (def a 1)"      eol
            eol
            "  (put @{} :a 2)" eol
            "  # =>"           eol
            "  @{:a 1"         eol
            "    :b 2}"        eol
            eol
            "  )"))

  (def [ti-zloc _ _]
    (r/find-test-indicator (-> (j/par src)
                             j/zip-down
                             j/down)))

  (j/node ti-zloc)
  # =>
  [:comment @{:bc 3 :bl 6 :ec 7 :el 6} "# =>"]

  (def expected-expr-zloc (r/find-expected-expr ti-zloc))

  (j/node expected-expr-zloc)
  # =>
  [:table @{:bc 3 :bl 7 :ec 10 :el 8}
   [:keyword @{:bc 5 :bl 7 :ec 7 :el 7} ":a"]
   [:whitespace @{:bc 7 :bl 7 :ec 8 :el 7} " "]
   [:number @{:bc 8 :bl 7 :ec 9 :el 7} "1"]
   [:whitespace @{:bc 9 :bl 7 :ec 1 :el 8} "\n"]
   [:whitespace @{:bc 1 :bl 8 :ec 5 :el 8} "    "]
   [:keyword @{:bc 5 :bl 8 :ec 7 :el 8} ":b"]
   [:whitespace @{:bc 7 :bl 8 :ec 8 :el 8} " "]
   [:number @{:bc 8 :bl 8 :ec 9 :el 8} "2"]]

  (-> (j/left expected-expr-zloc)
      j/node)
  # =>
  [:whitespace @{:bc 1 :bl 7 :ec 3 :el 7} "  "]

  (def src
    (string "(comment"                eol
            eol
            "  (butlast @[:a :b :c])" eol
            "  # => @[:a :b]"         eol
            eol
            "  (butlast [:a])"        eol
            "  # => []"               eol
            eol
            ")"))

  (def [ti-zloc _ _]
    (r/find-test-indicator (-> (j/par src)
                             j/zip-down
                             j/down)))

  (j/node ti-zloc)
  # =>
  [:comment @{:bc 3 :bl 4 :ec 16 :el 4} "# => @[:a :b]"]

  (r/find-expected-expr ti-zloc)
  # =>
  :no-expected-expression

  )

(defn r/make-label
  [left right]
  (string ""
          (when (not (empty? left))
            (string " " left))
          (when (or (not (empty? left))
                    (not (empty? right)))
            (string " =>"))
          (when (not (empty? right))
            (string " " right))))

(comment

  (r/make-label "hi" "there")
  # =>
  " hi => there"

  (r/make-label "hi" "")
  # =>
  " hi =>"

  (r/make-label "" "there")
  # =>
  " => there"

  (r/make-label "" "")
  # =>
  ""

  )

(defn r/find-exprs
  [ti-zloc]
  # look for a test expression
  (def test-expr-zloc (r/find-test-expr ti-zloc))
  (case test-expr-zloc
    :no-test-expression
    (break [nil nil])
    #
    :unexpected-result
    (errorf "unexpected result from `find-test-expr`: %p"
            test-expr-zloc))
  # look for an expected value expression
  (def expected-expr-zloc (r/find-expected-expr ti-zloc))
  (case expected-expr-zloc
    :no-expected-expression
    (break [test-expr-zloc nil])
    #
    :unexpected-result
    (errorf "unexpected result from `find-expected-expr`: %p"
            expected-expr-zloc))
  #
  [test-expr-zloc expected-expr-zloc])

(comment

  (def eol (if (= :windows (os/which)) "\r\n" "\n"))

  (def src
    (string "(+ 1 1)" eol
            "# =>"    eol
            "2"))

  (def [ti-zloc _ _]
    (r/find-test-indicator (-> (j/par src)
                             j/zip-down)))

  (def [t-zloc e-zloc] (r/find-exprs ti-zloc))

  (j/gen (j/node t-zloc))
  # =>
  "(+ 1 1)"

  (j/gen (j/node e-zloc))
  # =>
  "2"

  )

(defn r/wrap-as-test-call
  [start-zloc end-zloc test-label]
  # XXX: hack - not sure if robust enough
  (def eol-str (if (= :windows (os/which)) "\r\n" "\n"))
  (-> (j/wrap start-zloc [:tuple @{}] end-zloc)
      # newline important for preserving long strings
      (j/insert-child [:whitespace @{} eol-str])
      # name of test macro
      (j/insert-child [:symbol @{} "_verify/is"])
      # for column zero convention, insert leading whitespace
      # before the beginning of the tuple (_verify/is ...)
      (j/insert-left [:whitespace @{} "  "])
      # add location info argument
      (j/append-child [:whitespace @{} " "])
      (j/append-child [:string @{} test-label])))

(comment

  (def eol (if (= :windows (os/which)) "\r\n" "\n"))

  (def src
    (string "(+ 1 1)" eol
            "# =>"    eol
            "2"))

  (def [ti-zloc _ _]
    (r/find-test-indicator (-> (j/par src)
                             j/zip-down)))

  (def [t-zloc e-zloc] (r/find-exprs ti-zloc))

  (let [left-of-t-zloc (j/left t-zloc)
        start-zloc (match (j/node left-of-t-zloc)
                     [:whitespace]
                     left-of-t-zloc
                     #
                     t-zloc)
        w-zloc (r/wrap-as-test-call start-zloc e-zloc "\n\"hi!\"")]
    (j/gen (j/node w-zloc)))
  # =>
  (string "(_verify/is\n"
          "(+ 1 1)\n"
          "# =>\n"
          "2 \n"
          `"hi!")`)

  )

(defn r/rewrite-comment-zloc
  [comment-zloc]
  # move into comment block
  (var curr-zloc (j/down comment-zloc))
  (var found-test nil)
  # process comment block content
  (while (not (j/end? curr-zloc))
    (def [ti-zloc label-left label-right] (r/find-test-indicator curr-zloc))
    (when (not ti-zloc)
      (break))
    #
    (def [test-expr-zloc expected-expr-zloc] (r/find-exprs ti-zloc))
    (set curr-zloc
         (if (or (nil? test-expr-zloc)
                 (nil? expected-expr-zloc))
           (j/right curr-zloc) # next
           # found a complete test, work on rewriting
           (let [left-of-te-zloc (j/left test-expr-zloc)
                 start-zloc (match (j/node left-of-te-zloc)
                              [:whitespace]
                              left-of-te-zloc
                              #
                              test-expr-zloc)
                 end-zloc expected-expr-zloc
                 # XXX: use `attrs` here?
                 ti-line-no ((get (j/node ti-zloc) 1) :bl)
                 test-label (string `"`
                                    `line-` ti-line-no
                                    (r/make-label label-left label-right)
                                    `"`)]
             (set found-test true)
             (r/wrap-as-test-call start-zloc end-zloc test-label)))))
  # navigate back out to top of block
  (when found-test
    # morph comment block into plain tuple -- to be unwrapped later
    (-> curr-zloc
        j/up
        j/down
        (j/replace [:whitespace @{} " "])
        # begin hack to prevent trailing whitespace once unwrapping occurs
        j/rightmost
        (j/insert-right [:keyword @{} ":smile"])
        # end of hack
        j/up)))

(comment

  (def eol (if (= :windows (os/which)) "\r\n" "\n"))

  (def src
    (string "(comment"         eol
            eol
            "  (def a 1)"      eol
            eol
            "  (put @{} :a 2)" eol
            "  # left =>"      eol
            "  @{:a 2}"        eol
            eol
            "  (+ 1 1)"        eol
            "  # => right"     eol
            "  2"              eol
            eol
            "  )"))

  (-> (j/par src)
      j/zip-down
      r/rewrite-comment-zloc
      j/root
      j/gen)
  # =>
  (string "( "                          eol
          eol
          "  (def a 1)"                 eol
          eol
          "  (_verify/is"               eol
          "  (put @{} :a 2)"            eol
          "  # left =>"                 eol
          `  @{:a 2} "line-6 left =>")` eol
          eol
          "  (_verify/is"               eol
          "  (+ 1 1)"                   eol
          "  # => right"                eol
          `  2 "line-10 => right")`     eol
          eol
          "  :smile)")

  )

(defn r/rewrite-comment-block
  [comment-src]
  (-> (j/par comment-src)
      j/zip-down
      r/rewrite-comment-zloc
      j/root
      j/gen))

(comment

  (def eol (if (= :windows (os/which)) "\r\n" "\n"))

  (def src
    (string "(comment"          eol
            eol
            "  (def a 1)"       eol
            eol
            "  (put @{} :a 2)"  eol
            "  # =>"            eol
            "  @{:a 2}"         eol
            eol
            "  (+ 1 1)"         eol
            "  # left => right" eol
            "  2"               eol
            eol
            "  )"))

  (r/rewrite-comment-block src)
  # =>
  (string "( "                           eol
          eol
          "  (def a 1)"                  eol
          eol
          "  (_verify/is"                eol
          "  (put @{} :a 2)"             eol
          "  # =>"                       eol
          `  @{:a 2} "line-6")`          eol
          eol
          "  (_verify/is"                eol
          "  (+ 1 1)"                    eol
          "  # left => right"            eol
          `  2 "line-10 left => right")` eol
          eol
          "  :smile)")

  )

(defn r/rewrite
  [src]
  (var changed nil)
  # XXX: hack - not sure if robust enough
  (def eol-str (if (= :windows (os/which)) "\r\n" "\n"))
  (var curr-zloc
    (-> (j/par src)
        j/zip-down
        # XXX: leading newline is a hack to prevent very first thing
        #      from being a comment block
        (j/insert-left [:whitespace @{} eol-str])
        # XXX: once the newline is inserted, need to move to it
        j/left))
  #
  (while (not (j/end? curr-zloc))
    # try to find a top-level comment block
    (if-let [comment-zloc
             (j/right-until curr-zloc
                            |(match (j/node $)
                               [:tuple _ [:symbol _ "comment"]]
                               true))]
      # may be rewrite the located top-level comment block
      (set curr-zloc
           (if-let [rewritten-zloc
                    (r/rewrite-comment-zloc comment-zloc)]
             (do
               (set changed true)
               (j/unwrap rewritten-zloc))
             comment-zloc))
      (break)))
  #
  (when changed
    (-> curr-zloc
        j/root
        j/gen)))

(comment

  (def eol (if (= :windows (os/which)) "\r\n" "\n"))

  (def src
    (string `(require "json")` eol
            eol
            "(defn my-fn"      eol
            "  [x]"            eol
            "  (+ x 1))"       eol
            eol
            "(comment"         eol
            eol
            "  (def a 1)"      eol
            eol
            "  (put @{} :a 2)" eol
            "  # =>"           eol
            "  @{:a 2}"        eol
            eol
            "  (my-fn 1)"      eol
            "  # =>"           eol
            "  2"              eol
            eol
            "  )"              eol
            eol
            "(defn your-fn"    eol
            "  [y]"            eol
            "  (* y y))"       eol
            eol
            "(comment"         eol
            eol
            "  (your-fn 3)"    eol
            "  # =>"           eol
            "  9"              eol
            eol
            "  (def b 1)"      eol
            eol
            "  (+ b 1)"        eol
            "  # =>"           eol
            "  2"              eol
            eol
            "  (def c 2)"      eol
            eol
            "  )"              eol
            ))

  (r/rewrite src)
  # =>
  (string eol
          `(require "json")`     eol
          eol
          "(defn my-fn"          eol
          "  [x]"                eol
          "  (+ x 1))"           eol
          eol
          " "                    eol
          eol
          "  (def a 1)"          eol
          eol
          "  (_verify/is"        eol
          "  (put @{} :a 2)"     eol
          "  # =>"               eol
          `  @{:a 2} "line-12")` eol
          eol
          "  (_verify/is"        eol
          "  (my-fn 1)"          eol
          "  # =>"               eol
          `  2 "line-16")`       eol
          eol
          "  :smile"             eol
          eol
          "(defn your-fn"        eol
          "  [y]"                eol
          "  (* y y))"           eol
          eol
          " "                    eol
          eol
          "  (_verify/is"        eol
          "  (your-fn 3)"        eol
          "  # =>"               eol
          `  9 "line-28")`       eol
          eol
          "  (def b 1)"          eol
          eol
          "  (_verify/is"        eol
          "  (+ b 1)"            eol
          "  # =>"               eol
          `  2 "line-34")`       eol
          eol
          "  (def c 2)"          eol
          eol
          "  :smile"             eol)

  )

(comment

  (def eol (if (= :windows (os/which)) "\r\n" "\n"))

  # https://github.com/sogaiu/judge-gen/issues/1
  (def src
    (string "(comment"        eol
            eol
            "  (-> ``"        eol
            "      123456789" eol
            "      ``"        eol
            "      length)"   eol
            "  # =>"          eol
            "  9"             eol
            eol
            "  (->"           eol
            "    ``"          eol
            "    123456789"   eol
            "    ``"          eol
            "    length)"     eol
            "  # =>"          eol
            "  9"             eol
            eol
            "  )"))

  (r/rewrite src)
  # =>
  (string eol
          " "               eol
          eol
          "  (_verify/is"   eol
          "  (-> ``"        eol
          "      123456789" eol
          "      ``"        eol
          "      length)"   eol
          "  # =>"          eol
          `  9 "line-7")`   eol
          eol
          "  (_verify/is"   eol
          "  (->"           eol
          "    ``"          eol
          "    123456789"   eol
          "    ``"          eol
          "    length)"     eol
          "  # =>"          eol
          `  9 "line-15")`  eol
          eol
          "  :smile")

  )

(defn r/rewrite-as-test-file
  [src]
  (when (not (empty? src))
    (when-let [rewritten (r/rewrite src)]
      # XXX: hack - not sure if robust enough
      (def eol-str (if (= :windows (os/which)) "\r\n" "\n"))
      (string v/as-string
              eol-str
              "(_verify/start-tests)"
              eol-str
              rewritten
              eol-str
              "(_verify/end-tests)"
              eol-str
              "(_verify/report)"
              eol-str))))


(comment import ./utils :prefix "")
(defn u/parse-path
  [path]
  (def revcap-peg
    ~(sequence (capture (sequence (choice (to (choice "/" `\`))
                                          (thru -1))))
               (capture (thru -1))))
  (when-let [[rev-name rev-dir]
             (-?>> (string/reverse path)
                   (peg/match revcap-peg)
                   (map string/reverse))]
    [(or rev-dir "") rev-name]))

(comment

  (u/parse-path "/tmp/fun/my.fnl")
  # =>
  ["/tmp/fun/" "my.fnl"]

  (u/parse-path "/my.janet")
  # =>
  ["/" "my.janet"]

  (u/parse-path "pp.el")
  # =>
  ["" "pp.el"]

  (u/parse-path "/")
  # =>
  ["/" ""]

  (u/parse-path "")
  # =>
  ["" ""]

  )



###########################################################################

(def usage
  ``
  Usage: jtfm [<file-or-dir>...]
         jtfm [-h|--help]

  Create and run comment tests...just the facts, ma'am.

  Parameters:

    <file-or-dir>          path to file or directory

  Options:

    -h, --help             show this output

  Configuration:

    .jtfm.jdn              configuration file

  Examples:

    Create and run tests in `src/` directory:

    $ jtfm src

    `jtfm` can be used via `jpm`, `jeep`, etc. with
    some one-time setup.  Create a suitable `.jtfm.jdn`
    file in a project's root directory and a runner
    file in a project's `test/` subdirectory (see below
    for further details).

    Run via `jeep test`:

    $ jeep test

    Run via `jpm test`:

    $ jpm test

    Run using the configuration file via direct
    invocation:

    $ jtfm

  Example `.jtfm.jdn` content:

    {# describes what to test - file and dir paths
     :includes ["src" "bin/my-script"]
     # describes what to skip - file paths only
     :excludes ["src/sample.janet"]}

  Example runner file `test/trigger-jtfm.janet`:

    (import ../jtfm)

    (jtfm/main)
  ``)

(def test-file-ext ".jtfm")

(defn make-tests
  [filepath &opt opts]
  (def src (slurp filepath))
  (def test-src (r/rewrite-as-test-file src))
  (unless test-src
    (break :no-tests))
  #
  (def [fdir fname] (u/parse-path filepath))
  (def test-filepath (string fdir "_" fname test-file-ext))
  (when (and (not (get opts :overwrite))
             (os/stat test-filepath :mode))
    (eprintf "test file already exists for: %p" filepath)
    (break nil))
  #
  (spit test-filepath test-src)
  #
  test-filepath)

(defn run-tests
  [test-filepath]
  (try
    (with [of (file/temp)]
      (with [ef (file/temp)]
        (let [cmd
              # prevents any contained `main` functions from executing
              ["janet" "-e" (string "(dofile `" test-filepath "`)")]
              ecode (os/execute cmd :p {:out of :err ef})]
          (when (not (zero? ecode))
            (eprintf "non-zero exit code: %p" ecode))
          #
          (file/flush of)
          (file/flush ef)
          (file/seek of :set 0)
          (file/seek ef :set 0)
          #
          [(file/read of :all)
           (file/read ef :all)
           ecode])))
    ([e]
      (eprintf "problem executing tests: %p" e)
      [nil nil nil])))

(defn report
  [out err]
  (when (and out (pos? (length out)))
    (print out)
    (print))
  (when (and err (pos? (length err)))
    (print "------")
    (print "stderr")
    (print "------")
    (print err)
    (print))
  # XXX: kind of awkward
  (when (and (empty? out) (empty? err))
    (print "no test output...possibly no tests")
    (print)))

(defn make-run-report
  [filepath &opt opts]
  (default opts @{})
  # create test source
  (def result (make-tests filepath opts))
  (unless result
    (eprintf "failed to create test file for: %p" filepath)
    (break nil))
  #
  (when (= :no-tests result)
    (break :no-tests))
  #
  (def test-filepath result)
  # run tests and collect output
  (def [out err ecode] (run-tests test-filepath))
  # print out results
  (report out err)
  # finish off
  (when (zero? ecode)
    (os/rm test-filepath)
    true))

########################################################################

(defn main
  [& args]
  (def opts (a/parse-args (drop 1 args)))
  #
  (when (get opts :help)
    (print usage)
    (os/exit 0))
  #
  (def includes (get opts :includes))
  (def excludes (get opts :excludes))
  #
  (def src-filepaths
    (s/collect-paths includes |(or (string/has-suffix? ".janet" $)
                                   (s/has-janet-shebang? $))))
  # generate tests, run tests, and report
  (each path src-filepaths
    (when (and (not (has-value? excludes path))
               (= :file (os/stat path :mode)))
      (print path)
      (def result (make-run-report path opts))
      (cond
        (= :no-tests result)
        # XXX: the 2 newlines here are cosmetic
        (eprintf "* no tests detected for: %p\n\n" path)
        #
        (nil? result)
        (do
          (eprintf "failure in: %p" path)
          (os/exit 1))
        #
        (true? result)
        true
        #
        (do
          (eprintf "Unexpected result %p for: %p" result path)
          (os/exit 1)))))
  (printf "All tests completed successfully in %d file(s)."
          (length src-filepaths)))

