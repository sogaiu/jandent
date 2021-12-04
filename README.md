# jandent

Indenter for Janet

## Background

[spork/fmt](https://github.com/janet-lang/spork/blob/master/spork/fmt.janet)
is a compact elegant formatter for Janet by bakpakin.

It works by parsing source while discarding whitespace (except
newlines) and then emits newly formatted source while removing some
newlines and inserting whitespace in appropriate locations.

It's fast, but it might discard some whitespace-related information
that one might prefer preserved.

jandent is based on spork/fmt but it attempts to preserve whitespace
information to the right of indentation, e.g.
```
   (+ 1 1)   # a comment
   ^      ^^^
   |       |
   |       ----- whitespace is preserved (right of indentation)
   |
   ---- indentation point
```

## Usage

The API is the same as spork/fmt:

* format-print
* format
* format-file

There is also a standalone executable named `jindt`, which is
essentially [jfmt](https://github.com/andrewchambers/jfmt) for `jandent`.

## Features

* Leave whitespace to the right of indentation alone
* Handle "data" tuples
* Doesn't remove newlines
* Doesn't add newline at end
* Treat `comment` as an indent-2 item

## Details

spork/fmt's approach is fast but it can adversely impact some
situations involving contiguous whitespace.

Some prefer to have columnar alignment like:

```
(let [ant   1
      tiger 2]
  (+ ant tiger))
```

spork/fmt will turn this into:

```
(let [ant 1
      tiger 2]
  (+ ant tiger))
```

Another situation has to do with whitespace before comments, like:

```
{:a 1   # this nice comment
 :b 20  # another comment
 :c 3}
```

This will become:

```
{:a 1 # this nice comment
 :b 20 # another comment
 :c 3}
```

jandent should leave such whitespace intact.

## Internal Things

The following changes were made to aid comprehension and exploration:

* Use longer names in pegs
* Some code reformatting
* No top-level private defs or defns
* Has a bit more commentary

## Credits

* andrewchambers - jfmt
* bakpakin - spork/fmt
* llmII - discussion

## License

Code from spork/fmt was modified and included, thus spork's license
applies.

