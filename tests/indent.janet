(import ../jandent/indent :prefix "")

# basic cases aiming to cover
(comment

  # `format` returns a buffer

  (do
    (def before
      @``
       [:a
       :b
       :c]
       ``)
    #
    (def after
      @``
       [:a
        :b
        :c]
       ``)
    #
    (deep= (format before) after))
  # => true

  (do
    (def before
      @``
       @["1"
       "2"
       "3"]
       ``)
    #
    (def after
      @``
       @["1"
         "2"
         "3"]
       ``)
    #
    (deep= (format before) after))
  # => true

  (do
    (def before
      @``
       {:a 1
       :b 2}
       ``)
    #
    (def after
      @``
       {:a 1
        :b 2}
       ``)
    #
    (deep= (format before) after))
  # => true

  (do
    (def before
      @``
       {:x 9
       :y 0}
       ``)
    #
    (def after
      @``
       {:x 9
        :y 0}
       ``)
    #
    (deep= (format before) after))
  # => true

  (do
    (def before
      @``
       [
       ]
       ``)
    #
    (def after
      @``
       [
        ]
       ``)
    #
    (deep= (format before) after))
  # => true

  (do
    (def before
      @``
       []
       ``)
    #
    (def after
      @``
       []
       ``)
    #
    (deep= (format before) after))
  # => true

  (do
    (def before
      @``
       (
       )
       ``)
    #
    (def after
      @``
       (
        )
       ``)
    #
    (deep= (format before) after))
  # => true

  (do
    (def before
      @``
       ()
       ``)
    #
    (def after
      @``
       ()
       ``)
    #
    (deep= (format before) after))
  # => true

  (do
    (def before
      @``
       (
       def a 1)
       ``)
    #
    (def after
      @``
       (
        def a 1)
       ``)
    #
    (deep= (format before) after))
  # => true

  (do
    (def before
      @``
       (def
       a
       1)
       ``)
    #
    (def after
      @``
       (def
         a
         1)
       ``)
    #
    (deep= (format before) after))
  # => true

  (do
    (def before
      @``
       (let [x 1]
       (set y 2)
       # a comment
       (+ x y))
       ``)
    #
    (def after
      @``
       (let [x 1]
         (set y 2)
         # a comment
         (+ x y))
       ``)
    #
    (deep= (format before) after))
  # => true

  (do
    (def before
      @``
       (
       print "hello")
       ``)
    #
    (def after
      @``
       (
        print "hello")
       ``)
    #
    (deep= (format before) after))
  # => true

  (do
    (def before
      @``
       (print
       "hello")
       ``)
    #
    (def after
      @``
       (print
         "hello")
       ``)
    #
    (deep= (format before) after))
  # => true

  (do
    (def before
      @``
       (print "alpha"
       "beta")
       ``)
    #
    (def after
      @``
       (print "alpha"
              "beta")
       ``)
    #
    (deep= (format before) after))
  # => true

  (do
    (def before
      @``
       (put @{:a 1}
       :b 2
       # fun comment
       :c 3)
       ``)
    #
    (def after
      @``
       (put @{:a 1}
            :b 2
            # fun comment
            :c 3)
       ``)
    #
    (deep= (format before) after))
  # => true

  (do
    (def before
      @``
       '(# data
         :ant
         :bee
         :cat)
       ``)
    #
    (def after
      @``
       '(# data
         :ant
         :bee
         :cat)
       ``)
    #
    (deep= (format before) after))
  # => true

  )

# additional illustrations
(comment

  (do
    (def before
      @``
       (defn hi
       [x]
       (+ x 1))
       ``)
    #
    (def after
      @``
       (defn hi
         [x]
         (+ x 1))
       ``)
    (deep= (format before) after))
  # => true

  (do
    (def before
      @``
       (defn hi
       [x]
       (+ x 1))

       (def a
       2)
       ``)
    #
    (def after
      @``
       (defn hi
         [x]
         (+ x 1))

       (def a
         2)
       ``)
    (deep= (format before) after))
  # => true

  (do
    (def before
      @``
       (let [i   1
       al  10
       bee 100]
       (+ i al bee))
       ``)
    #
    (def after
      @``
       (let [i   1
             al  10
             bee 100]
         (+ i al bee))
       ``)
    (deep= (format before) after))
  # => true

  (do
    (def before
      @``
       (let [x   1
             y  10
             z 100]
         (+ x y z))
       ``)
    #
    (def after
      @``
       (let [x   1
             y  10
             z 100]
         (+ x y z))
       ``)
    (deep= (format before) after))
  # => true

  (do
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
    (deep= (format before) after))
  # => true

  (do
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
    (deep= (format before) after))
  # => true

  (do
    (def before
      @``
       (comment

         :hi

       )
       ``)
    #
    (def after
      @``
       (comment

         :hi

         )
       ``)
    (deep= (format before) after))
  # => true

  (do
    (def before
      @``
       (comment

         :hi
         #
       )
       ``)
    #
    (def after
      @``
       (comment

         :hi
         #
         )
       ``)
    (deep= (format before) after))
  # => true

  (do
    (def before
      @``
       {# fun
       :a 1
       :b 2}
       ``)
    #
    (def after
      @``
       {# fun
        :a 1
        :b 2}
       ``)
    (deep= (format before) after))
  # => true

  # differs from original spork/fmt
  (do
    (def before
      @``
       (# fun
       print "hi")
       ``)
    #
    (def after
      @``
       (# fun
        print "hi")
       ``)
    (deep= (format before) after))
  # => true

  # differs from original spork/fmt
  (do
    (def before
      @``
       (
        print
         "HOHOHO")
       ``)
    #
    (def after
      @``
       (
        print
        "HOHOHO")
       ``)
    (deep= (format before) after))
  # => true

  # differs from original spork/fmt
  (do
    (def before
      @``
       {:a 0
       :b 1 # test comment
       }
       ``)
    #
    (def after
      @``
       {:a 0
        :b 1 # test comment
        }
      ``)
    (deep= (format before) after))
  # => true

  # differs from original spork/fmt
  (do
    (def before
      @``
       [:a       0
       :b
       # test comment
       ]
       ``)
    #
    (def after
      @``
       [:a       0
        :b
        # test comment
        ]
       ``)
    (deep= (format before) after))
  # => true

  # differs from original spork/fmt
  (do
    (def before
      @``
       ( )
       ``)
    #
    (def after
      @``
       ( )
       ``)
    (deep= (format before) after))
  # => true

  # differs from original spork/fmt
  (do
    (def before
      @``
       # a comment
       ``)
    #
    (def after
      @``
       # a comment
       ``)
    (deep= (format before) after))
  # => true

  (do
    (def before
      @``
       (comment
       )
       ``)
    #
    (def after
      @``
       (comment
         )
       ``)
    (deep= (format before) after))
  # => true

  (do
    (def before
      @``
       (comment
         (comment
       )
       )
       ``)
    #
    (def after
      @``
       (comment
         (comment
           )
         )
       ``)
    (deep= (format before) after))
  # => true

  (do
    (def before
      @``
       (comment
        )
       ``)
    #
    (def after
      @``
       (comment
         )
       ``)
    (deep= (format before) after))
  # => true

  (do
    (def before
      @``
       (defn my-fn
       [x]
       (+ (/ 2 (- 3
       1))
       2))
       ``)
    #
    (def after
      @``
       (defn my-fn
         [x]
         (+ (/ 2 (- 3
                    1))
            2))
       ``)
    (deep= (format before) after))
  # => true

  (do
    (def before
      @``
       (printf "hello: %d"
       (+ 1 1))
       ``)
    #
    (def after
      @``
       (printf "hello: %d"
               (+ 1 1))
       ``)
    (deep= (format before) after))
  # => true

  # XXX: fwiw, this is not consistent with janet-editor-elf
  (do
    (def before
      @``
       (printf "hello: %d"
       (+ 1 1)
       )
       ``)
    #
    (def after
      @``
       (printf "hello: %d"
               (+ 1 1)
        )
       ``)
    (deep= (format before) after))
  ## => true

  )

