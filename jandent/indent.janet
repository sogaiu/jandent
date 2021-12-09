### based on spork/fmt by bakpakin

### hint for comprehension
###
### study the call to `flushwhite` in `fmt-1` and everything
### that exists to support it

(defn pnode
  "Make a capture function for a node."
  [tag]
  (fn [x] [tag x]))

# all nodes except one have the form:
#
#   [:tag-name ...]
#
# the sole exception is the node for a newline which is:
#
#   "\n"
#
# there are 3 kinds of non-newline whitespace:
#
# 1. :ws-bi - before indentation (leading whitespace)
# 2. :ws-tr - after last non-whitespace (trailing whitespace)
# 3. :ws    - there is non-whitespace somewhere to the left and right
#
# example:
#
# bol   (+ 1 1)   eol
#    ^^^  ^    ^^^
#     |   |     |
#     |  :ws    |
#     |      :ws-tr
#  :ws-bi
#
# bol stands for beginning of line
# eol stands for end of line
(defn make-tree
  "Turn a string of source code into a tree that will be printed"
  [source]
  (var bi true)
  #
  (def parse-peg
    "Peg to parse Janet with extra information, namely comments."
    (peg/compile
      ~{:nl "\n"
        :ws (cmt (sequence (capture (some (set " \t\r\f\0\v")))
                           (look 0 (choice -1
                                           (capture 1))))
                 ,(fn [& args]
                    (def ws (get args 0))
                    (cond
                      (and bi
                           (= 2 (length args))
                           (not= "\n" (get args 1)))
                      [:ws-bi ws]
                      #
                      (= 1 (length args))
                      [:ws-tr ws]
                      #
                      (= 2 (length args))
                      (if (= "\n" (get args 1))
                        [:ws-tr ws]
                        [:ws ws])
                      #
                      (do
                        (eprintf "unexpected ws info: %p %p"
                                 args (length args))
                        (error "unexpected input")))))
        :readermac (set "';~,|")
        :symchars (choice (range "09" "AZ" "az" "\x80\xFF")
                          (set "!$%&*+-./:<?=>@^_"))
        :token (some :symchars)
        :hex (range "09" "af" "AF")
        :escape
        (sequence "\\" (choice (set "ntrzfev0\"\\")
                               (sequence "x" (repeat 2 :hex))
                               (sequence "u" (repeat 4 :hex))
                               (sequence "U" (repeat 6 :hex))
                               (error (constant "bad hex escape"))))
        :comment (replace (sequence "#"
                                    (capture (to (choice "\n" -1))))
                          ,(pnode :comment))
        :span (replace (capture :token)
                       ,(pnode :span))
        :bytes (capture (sequence `"`
                                  (any (choice :escape
                                               (if-not `"` 1)))
                                  `"`))
        :string (replace :bytes ,(pnode :string))
        :buffer (replace (sequence "@" :bytes)
                         ,(pnode :buffer))
        :long-bytes
        (capture {:delim (some "`")
                  :open (capture :delim :n)
                  :close (cmt (sequence (not (look -1 "`"))
                                        (backref :n)
                                        (capture :delim))
                              ,=)
                  :main (drop (sequence :open
                                        (any (if-not :close 1))
                                        :close))})
        :long-string (replace :long-bytes ,(pnode :string))
        :long-buffer (replace (sequence "@" :long-bytes)
                              ,(pnode :buffer))
        :ptuple (replace (group (sequence "("
                                          (any :input)
                                          (choice ")" (error))))
                         ,(pnode :ptuple))
        :btuple (replace (group (sequence "["
                                          (any :input)
                                          (choice "]" (error))))
                         ,(pnode :btuple))
        :struct (replace (group (sequence "{"
                                          (any :input)
                                          (choice "}" (error))))
                         ,(pnode :struct))
        :parray (replace (group (sequence "@("
                                          (any :input)
                                          (choice ")" (error))))
                         ,(pnode :array))
        :barray (replace (group (sequence "@["
                                          (any :input)
                                          (choice "]" (error))))
                         ,(pnode :array))
        :table (replace (group (sequence "@{"
                                         (any :input)
                                         (choice "}" (error))))
                        ,(pnode :table))
        :rmform (replace (group (sequence ':readermac
                                          (group (any :non-form))
                                          :form))
                         ,(pnode :rmform))
        :form
        (sequence (drop (cmt (constant "smile")
                             ,(fn [& args]
                                (set bi false)
                                true)))
                  (choice :rmform
                          :parray :barray :ptuple :btuple :table :struct
                          :buffer :string :long-buffer :long-string
                          :span))
        :non-form
        (choice :ws
                # :nl
                (cmt (capture :nl)
                     ,(fn [& args]
                        (set bi true)
                        (first args)))
                # :comment
                (sequence (drop (cmt (constant "smile")
                                     ,(fn [& args]
                                        (set bi false)
                                        true)))
                          :comment))
        :input
        (choice :non-form
                (cmt (capture :form)
                     ,(fn [& args]
                        (set bi false)
                        (first args))))
        :main (sequence (any :input)
                        (choice -1 (error)))}))
  #
  [:top (peg/match parse-peg source)])

(comment

  (make-tree "# hello\n")
  # => '(:top @[(:comment " hello") "\n"])

  (deep=
    #
    (make-tree "(+ 1 1)")
    #
    '(:top
       @[(:ptuple
           @[(:span "+") (:ws " ")
             (:span "1") (:ws " ")
             (:span "1")])]))
  # => true

  (deep=
    #
    (make-tree " (+ 1 1) ")
    #
    '(:top
       @[(:ws-bi " ")
         (:ptuple
           @[(:span "+") (:ws " ")
             (:span "1") (:ws " ")
             (:span "1")])
         (:ws-tr " ")]))
  # => true

  (deep=
    #
    (make-tree
      (string "(comment\n"
              "\n"
              "  :hi\n"
              "  #\n"
              "  )"))
    #
    '(:top
       @[(:ptuple
           @[(:span "comment") "\n"
             "\n"
             (:ws-bi "  ") (:span ":hi") "\n"
             (:ws-bi "  ") (:comment "") "\n"
             (:ws-bi "  ")])]))
  # => true

  )

# added `comment`
(def indent-2-forms
  "A list of forms that are control forms and should be indented two spaces."
  (invert ["case" "comment" "compif" "compwhen" "cond" "coro"
           "def" "def-" "default" "defer" "defglobal" "defmacro" "defmacro-"
           "defn" "defn-" "do"
           "each" "eachp" "eachk" "eachy" "edefer" "ev/do-thread" "ev/spawn"
           "ev/with-deadline"
           "fn" "for" "forever" "forv"
           "generate"
           "if" "if-let" "if-not" "if-with"
           "label" "let" "loop"
           "match"
           "prompt"
           "repeat"
           "seq" "short-fn"
           "try"
           "unless"
           "var" "varfn" "varglobal" "var-"
           "when" "when-let" "when-with" "while" "with" "with-dyns"
           "with-syms" "with-vars"]))

(def indent-2-peg
  "Peg to use to fuzzy match certain forms."
  (peg/compile ~(choice "with-" "def" "if-" "when-")))

(defn first-non-ws-is-nl?
  [xs]
  (var result nil)
  (each x xs
    (when (= "\n" x)
      (set result true)
      (break))
    (when-let [[tag _] x]
      (when (and (not= :ws-bi tag)
                 (not= :ws tag)
                 (not= :ws-tr tag))
        (break false))))
  (truthy? result))

(comment

  (first-non-ws-is-nl? ["\n" [:ws " "]])
  # => true

  (first-non-ws-is-nl? [[:ws " "] "\n" [:ws " "]])
  # => true

  (first-non-ws-is-nl? [[:ws-bi " "] "\n" [:ws " "]])
  # => true

  (first-non-ws-is-nl? [[:ws-tr " "] "\n" [:ws " "]])
  # => true

  (first-non-ws-is-nl? [[:comment " hi"] "\n" [:ws " "]])
  # => false

  )

(defn check-indent-2
  "Check if a tuple needs a 2 space indent or not"
  [items]
  (if-let [[tag body] (get items 0)]
    (cond
      (first-non-ws-is-nl? (slice items 1)) true
      (not= tag :span) nil
      (in indent-2-forms body) true
      (peg/match indent-2-peg body) true)))

(defn check-data-indent
  "Check if a tuple should be formatted as data"
  [items]
  (when-let [[head-node] items]
    (match head-node
      "\n" true
      [:comment _] true
      [:ws _] true
      # XXX: can these happen?
      [:ws-bi _] true
      [:ws-tr _] true)))

(defn non-nls
  [xs]
  (filter |(not= $ "\n") xs))

(defn has-nl?
  [xs]
  (truthy? (find |(= $ "\n") xs)))

(defn fmt
  "Emit formatted."
  [tree]

  # tracks current column based on emitted content
  #   modified by `emit` and `newline`
  #   mostly used by `indent`
  #   also used by `emit-body` and `emit-funcall` for some edge cases
  (var col 0)

  # stack for saving / restoring `white`
  #   manipulated by `indent` and `dedent`
  (def ident-stack @[])

  # what `white` is reset to when `newline` is called
  #   also is saved/restored to/from `ident-stack`
  (var ident "")

  # whitespace that will be output via `flushwhite`
  #   manipulated directly by `flushwhite`, `dropwhite`, `newline`
  #   also indirectly via `dropwhite` by:
  #     `newline`
  #     `emit-body`
  #     `emit-funcall`
  #     `emit-string`
  (def white @"")

  # `emit` and `newline` are the only things that output directly
  # (`fmt-1` now directly outputs trailing whitespace)
  #
  # `emit` is called by:
  #   `flushwhite`
  #   `emit-body`
  #   `emit-funcall``
  #   `emit-string`
  #   `emit-rmform`
  #   `fmt-1`
  #
  # `newline` is called by:
  #   `emit-string`
  #   `fmt-1`
  #
  # `fmt-1` is the only caller of:
  #   `flushwhite`
  #   `emit-body`
  #   `emit-funcall`
  #   `emit-string`
  #   `emit-rmform`

  (defn emit [& xs]
    (each x xs
      # parsed content contains delimiters (e.g. strings include "s)
      (+= col (length x))
      (prin x)))

  (defn indent [&opt delta]
    (array/push ident-stack ident)
    (set ident
         (string/repeat " " (+ col (or delta 0)))))

  (defn dedent []
    (set ident
         (array/pop ident-stack)))

  (defn flushwhite []
    (emit white)
    (buffer/clear white))

  (defn dropwhite []
    (buffer/clear white))

  (defn newline []
    (dropwhite)
    (print)
    (buffer/push-string white ident)
    (set col 0))

  # Mutual recursion
  (var fmt-1-recur nil)

  (defn emit-body
    [open xs close &opt delta]
    (def od-col col)
    (emit open)
    #
    (indent delta)
    (each x xs (fmt-1-recur x))
    (dropwhite)
    (dedent)
    # XXX: messy part
    (cond
      # empty container case
      (and (has-nl? xs) (zero? (length (non-nls xs))))
      (emit (string/repeat " " (+ od-col 1)))
      # XXX: actually want alignment with opening delimiter
      #      of 2nd non-ws, non-comment child?
      (zero? col)
      (emit (string/repeat " "
                           (+ od-col 1 (or delta 0)))))
    #
    (emit close))

  (defn emit-funcall
    [xs]
    (def od-col col)
    (emit "(")
    #
    (def len (length xs))
    (when (pos? len)
      (fmt-1-recur (xs 0))
      (indent 1)
      (for i 1 len (fmt-1-recur (xs i)))
      (dropwhite)
      (dedent))
    # XXX: messy part
    (cond
      # empty tuple case
      (and (has-nl? xs) (zero? (length (non-nls xs))))
      (emit (string/repeat " " (+ od-col 1)))
      # XXX: actually want alignment with opening delimiter
      #      of 2nd non-ws, non-comment child?
      (zero? col)
      (emit (string/repeat " " (+ od-col 1))))
    #
    (emit ")"))

  (defn emit-string
    [x]
    (def parts
      (interpose "\n" (string/split "\n" x)))
    (each p parts
      (if (= p "\n")
        (do (newline) (dropwhite))
        (emit p))))

  (defn emit-rmform
    [rm nfs form]
    (emit rm)
    (each nf nfs
      (fmt-1-recur nf))
    (fmt-1-recur form))

  (var before-first-nl true)

  (defn fmt-1
    [node]
    # insert appropriate whitespace
    (when (and (not= "\n" node)
               (not= :ws-tr (get node 0)))
      (flushwhite))
    # node-specific "emission"
    (match node
      "\n" (newline)
      [:ws-bi x] (prin (if before-first-nl x ""))
      [:ws x] (emit x)
      [:ws-tr x] (prin x)
      [:comment x] (emit "#" x)
      [:span x] (emit x)
      [:string x] (emit-string x)
      [:buffer x] (do (emit "@") (emit-string x))
      [:array xs] (emit-body "@[" xs "]")
      [:btuple xs] (emit-body "[" xs "]")
      [:ptuple xs] (cond
                     (check-data-indent xs)
                     (emit-body "(" xs ")")
                     #
                     (check-indent-2 xs)
                     (emit-body "(" xs ")" 1)
                     #
                     (emit-funcall xs))
      [:struct xs] (emit-body "{" xs "}")
      [:table xs] (emit-body "@{" xs "}")
      [:rmform [rm nfs form]] (emit-rmform rm nfs form)
      [:top xs] (emit-body "" xs ""))
    (set before-first-nl false))

  (set fmt-1-recur fmt-1)
  (fmt-1 tree)
  (flush))

#
# Public API
#

(defn format-print
  "Format a string of source code and print the result."
  [source]
  (-> source make-tree fmt))

(defn format
  "Format a string of source code to a buffer."
  [source]
  (def out @"")
  (with-dyns [:out out]
    (format-print source))
  out)

(defn format-file
  "Format a file"
  [file]
  (def source (slurp file))
  (def out (format source))
  (spit file out))
