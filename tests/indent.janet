(import ../jandent/indent :prefix "")

# basic cases aiming to cover
(comment

  # `format` returns a buffer

  (def before
    @``
     [:a
     :b
     :c]
     ``)

  (def after
    @``
     [:a
      :b
      :c]
     ``)

  (format before)
  # =>
  after

  (def before
    @``
     @["1"
     "2"
     "3"]
     ``)

  (def after
    @``
     @["1"
       "2"
       "3"]
     ``)

  (format before)
  # =>
  after

  (def before
    @``
     {:a 1
     :b 2}
     ``)

  (def after
    @``
     {:a 1
      :b 2}
     ``)

  (format before)
  # =>
  after

  (def before
    @``
     {:x 9
     :y 0}
     ``)

  (def after
    @``
     {:x 9
      :y 0}
     ``)

  (format before)
  # =>
  after

  (def before
    @``
     [
     ]
     ``)

  (def after
    @``
     [
      ]
     ``)

  (format before)
  # =>
  after

  (def before
    @``
     []
     ``)

  (def after
    @``
     []
     ``)

  (format before)
  # =>
  after

  (def before
    @``
     (
     )
     ``)

  (def after
    @``
     (
      )
     ``)

  (format before)
  # =>
  after

  (def before
    @``
     ()
     ``)

  (def after
    @``
     ()
     ``)

  (format before)
  # =>
  after

  (def before
    @``
     (
     def a 1)
     ``)

  (def after
    @``
     (
      def a 1)
     ``)

  (format before)
  # =>
  after

  (def before
    @``
     (def
     a
     1)
     ``)

  (def after
    @``
     (def
       a
       1)
     ``)

  (format before)
  # =>
  after

  (def before
    @``
     (let [x 1]
     (set y 2)
     # a comment
     (+ x y))
     ``)

  (def after
    @``
     (let [x 1]
       (set y 2)
       # a comment
       (+ x y))
     ``)

  (format before)
  # =>
  after

  (def before
    @``
     (
     print "hello")
     ``)

  (def after
    @``
     (
      print "hello")
     ``)

  (format before)
  # =>
  after

  (def before
    @``
     (print
     "hello")
     ``)

  (def after
    @``
     (print
       "hello")
     ``)

  (format before)
  # =>
  after

  (def before
    @``
     (print "alpha"
     "beta")
     ``)

  (def after
    @``
     (print "alpha"
            "beta")
     ``)

  (format before)
  # =>
  after

  (def before
    @``
     (put @{:a 1}
     :b 2
     # fun comment
     :c 3)
     ``)

  (def after
    @``
     (put @{:a 1}
          :b 2
          # fun comment
          :c 3)
     ``)

  (format before)
  # =>
  after

  (def before
    @``
     '(# data
       :ant
       :bee
       :cat)
     ``)

  (def after
    @``
     '(# data
       :ant
       :bee
       :cat)
     ``)

  (format before)
  # =>
  after

  )

# additional illustrations
(comment

  (def before
    @``
     (defn hi
     [x]
     (+ x 1))
     ``)

  (def after
    @``
     (defn hi
       [x]
       (+ x 1))
     ``)

  (format before)
  # =>
  after

  (def before
    (buffer "(defn hi\n"
            "[x]\n"
            "(+ x 1))\n"
            "\n"
            "(def a\n"
            "2)"))

  (def after
    (buffer "(defn hi\n"
            "  [x]\n"
            "  (+ x 1))\n"
            "\n"
            "(def a\n"
            "  2)"))

  (format before)
  # =>
  after

  (def before
    @``
     (let [i   1
     al  10
     bee 100]
     (+ i al bee))
     ``)

  (def after
    @``
     (let [i   1
           al  10
           bee 100]
       (+ i al bee))
     ``)

  (format before)
  # =>
  after

  (def before
    @``
     (let [x   1
           y  10
           z 100]
       (+ x y z))
     ``)

  (def after
    @``
     (let [x   1
           y  10
           z 100]
       (+ x y z))
     ``)

  (format before)
  # =>
  after

  (def before
    @``
     (let [x   1
           y  10
           z 100]
       (+ x y z)
     )
     ``)

  (def after
    @``
     (let [x   1
           y  10
           z 100]
       (+ x y z)
       )
     ``)

  (format before)
  # =>
  after

  (def before
    @``
     (let [x   1
           y  10
           z 100]
       (+ x y z)
      )
     ``)

  (def after
    @``
     (let [x   1
           y  10
           z 100]
       (+ x y z)
       )
     ``)

  (format before)
  # =>
  after

  (def before
    (buffer "(comment\n"
            "\n"
            "  :hi\n"
            "\n"
            ")"))

  (def after
    (buffer "(comment\n"
            "\n"
            "  :hi\n"
            "\n"
            "  )"))

  (format before)
  # =>
  after

  (def before
     (buffer "(comment\n"
             "\n"
             "  :hi\n"
             "  #\n"
             ")"))

  (def after
    (buffer "(comment\n"
            "\n"
            "  :hi\n"
            "  #\n"
            "  )"))

  (format before)
  # =>
  after

  (def before
    @``
     {# fun
     :a 1
     :b 2}
     ``)

  (def after
    @``
     {# fun
      :a 1
      :b 2}
     ``)

  (format before)
  # =>
  after

  # differs from original spork/fmt

  (def before
    @``
     (# fun
     print "hi")
     ``)

  (def after
    @``
     (# fun
      print "hi")
     ``)

  (format before)
  # =>
  after

  # differs from original spork/fmt
  (def before
    @``
     (
      print
       "HOHOHO")
     ``)

  (def after
    @``
     (
      print
      "HOHOHO")
     ``)

  (format before)
  # =>
  after

  # differs from original spork/fmt

  (def before
    @``
     {:a 0
     :b 1 # test comment
     }
     ``)

  (def after
    @``
     {:a 0
      :b 1 # test comment
      }
    ``)

  (format before)
  # =>
  after

  # differs from original spork/fmt
  (def before
    @``
     [:a       0
     :b
     # test comment
     ]
     ``)

  (def after
    @``
     [:a       0
      :b
      # test comment
      ]
     ``)

  (format before)
  # =>
  after

  # differs from original spork/fmt
  (def before
    @``
     ( )
     ``)

  (def after
    @``
     ( )
     ``)

  (format before)
  # =>
  after

  # differs from original spork/fmt
  (def before
    @``
     # a comment
     ``)

  (def after
    @``
     # a comment
     ``)

  (format before)
  # =>
  after

  (def before
    @``
     (comment
     )
     ``)

  (def after
    @``
     (comment
       )
     ``)

  (format before)
  # =>
  after

  (def before
    @``
     (comment
       (comment
     )
     )
     ``)

  (def after
    @``
     (comment
       (comment
         )
       )
     ``)

  (format before)
  # =>
  after

  (def before
    @``
     (comment
      )
     ``)

  (def after
    @``
     (comment
       )
     ``)

  (format before)
  # =>
  after

  (def before
    @``
     (defn my-fn
     [x]
     (+ (/ 2 (- 3
     1))
     2))
     ``)

  (def after
    @``
     (defn my-fn
       [x]
       (+ (/ 2 (- 3
                  1))
          2))
     ``)

  (format before)
  # =>
  after

  (def before
    @``
     (printf "hello: %d"
     (+ 1 1))
     ``)

  (def after
    @``
     (printf "hello: %d"
             (+ 1 1))
     ``)

  (format before)
  # =>
  after

  # XXX: fwiw, this is not consistent with janet-editor-elf
  (def before
    @``
     (printf "hello: %d"
     (+ 1 1)
     )
     ``)

  (def after
    @``
     (printf "hello: %d"
             (+ 1 1)
      )
     ``)

  (format before)
  # =>
  after

  )

