# based heavily on andrewchambers' jfmt -- thanks!

(import ./jandent)

(defn main
  [& args]
  (if (one? (length args))
    (prin (jandent/format (file/read stdin :all)))
    (each f (tuple/slice args 1)
      (jandent/format-file f))))

