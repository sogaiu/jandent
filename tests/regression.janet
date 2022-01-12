(import ../jandent/indent :prefix "")

# regression tests
(comment

  # ) ks turned into )ks
  (def before
    @``
     (defn zipvec [ks vs]
     (if (or (= nil ks)
     (= nil vs))
     []
     (do
     (def res @[])
     (map (fn [k v]
     (array/push res k)
     (array/push res v)
     ) ks vs)
     res)))
     ``)

  (def after
    @``
     (defn zipvec [ks vs]
       (if (or (= nil ks)
               (= nil vs))
         []
         (do
           (def res @[])
           (map (fn [k v]
                  (array/push res k)
                  (array/push res v)
                  ) ks vs)
           res)))
     ``)

  (format before)
  # =>
  after

  # smaller version
  (def before
    @``
     (- 1
     ) 8
     ``)

  (def after
    @``
     (- 1
      ) 8
     ``)

  (format before)
  # =>
  after

  )

(comment

  # trailing whitespace on util/render line
  (def before
    (buffer/push-string
      @""
      "(when (game :won?)\n"
      "  (while (not (c/is-key-released key/select))\n"
      "    (util/render            \n"
      `      (c/draw-text (game :font) "Win" 140 100 20 1 color/red))))`))

  (def after
    (buffer/push-string
      @""
      "(when (game :won?)\n"
      "  (while (not (c/is-key-released key/select))\n"
      "    (util/render            \n"
      `      (c/draw-text (game :font) "Win" 140 100 20 1 color/red))))`))

  (format before)
  # =>
  after

  # smaller version
  (def before
    (buffer/push-string
      @""
      "(render            \n"
      `  (draw-text "You Win"))`))

  (def after
    (buffer/push-string
      @""
      "(render            \n"
      `  (draw-text "You Win"))`))

  (format before)
  # =>
  after

  )

# found in: 3c44171fd410974cdb4eba2a1f33c31ed3523ec5
(comment

  (def before
    (buffer/push-string
      @""
      "  " "\n"
      "(def a 1)"))

  (def after
    (buffer/push-string
      @""
      "  " "\n"
      "(def a 1)"))

  (format before)
  # =>
  after

  )

# found in: fec46566e73dccb88efd7ccbeb6ab04229a76dce
(comment

  (def before
    (buffer/push-string
      @""
      `(defn my-fn`  "\n"
      `  []`         "\n"
      ""             "\n"
      ` `            "\n" # the single whitespace here should be preserved
      `  (when true` "\n"
      `    (+ 1 1)))`))

  (def after
    (buffer/push-string
      @""
      `(defn my-fn`  "\n"
      `  []`         "\n"
      ""             "\n"
      ` `            "\n"
      `  (when true` "\n"
      `    (+ 1 1)))`))

  (format before)
  # =>
  after

  )

# found in: 6840d92da9a6911ff937df561a817ae27d8620df
(comment

    (def before
      (buffer/push-string
        @""
        "  (def code" "\n"
        "  ``"        "\n"
        "  (defn a"   "\n"
        "    [x]"     "\n"
        "    [)"      "\n"
        "  ``"        "\n"
        "  )"
        ))

    (def after
      (buffer/push-string
        @""
        "(def code"   "\n"
        "  ``"        "\n"
        "  (defn a"   "\n"
        "    [x]"     "\n"
        "    [)"      "\n"
        "  ``"        "\n"
        "  )"
        ))

  (format before)
  # =>
  after

  )

# found in: a25565fadc730f3f39bb7061115085984f497802
(comment

  (def before
    (buffer/push-string
      @""
      "(def code 1)" "\n"))

  (def after
    (buffer/push-string
      @""
      "(def code 1)" "\n"))

  (format before)
  # =>
  after

  )

