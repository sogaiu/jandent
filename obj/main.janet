#! /usr/bin/env janet

# based heavily on andrewchambers' jfmt -- thanks!

(import ./indent :prefix "")

(defn main
  [& args]
  (if (one? (length args))
    (prin (indent/format (file/read stdin :all)))
    (each f (tuple/slice args 1)
      (indent/format-file f))))

