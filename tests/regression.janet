(import ../jandent/indent :prefix "")

# regression tests
(comment

  # ) ks turned into )ks
  (do
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
    #
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
    #
    (deep= (format before) after))
  # => true

  # smaller version
  (do
    (def before
      @``
       (- 1
       ) 8
       ``)
    #
    (def after
      @``
       (- 1
        ) 8
       ``)
    #
    (deep= (format before) after))
  # => true

  # trailing whitespace on util/render line
  (do
    (def before
      (buffer/push-string
        @""
        "(when (game :won?)\n"
        "  (while (not (c/is-key-released key/select))\n"
        "    (util/render            \n"
        `      (c/draw-text (game :font) "Win" 140 100 20 1 color/red))))`))
    #
    (def after
      (buffer/push-string
        @""
        "(when (game :won?)\n"
        "  (while (not (c/is-key-released key/select))\n"
        "    (util/render            \n"
        `      (c/draw-text (game :font) "Win" 140 100 20 1 color/red))))`))
    #
    (deep= (format before) after))
  # => true

  # smaller version
  (do
    (def before
      (buffer/push-string
        @""
        "(render            \n"
        `  (draw-text "You Win"))`))
    #
    (def after
      (buffer/push-string
        @""
        "(render            \n"
        `  (draw-text "You Win"))`))
    #
    (deep= (format before) after))
  # => true

  )
