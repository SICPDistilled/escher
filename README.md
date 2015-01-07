# escher

[![Gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/SICPDistilled/escher?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)

## Getting started
Look over the [slides](http://www.sicpdistilled.com/slides/2.2.4/#1) (Video and ASCIIcast soon!)

## Completing the project
* Complete the vector functions

(you should be able to draw `(frame-painter frame1)` then)

* Complete the `path` function

* Define `box`, `x`, `diamond` and `george` (using `segment-painter`, you might want to use `path` as well)

Check by drawing them either with `(draw <picture>)` or `(<picture> <frame>)`

* Complete `flip-horiz`, `rotate`, `below`

Check again that they each work and can be combined by (for instance)

```clojure
(draw (below (beside george (flip-horiz george))
             (beside (rotate george) (flip-vert george))))
```

* Complete `up-split`

* Do the suggested refactor of `right-split` and `up-split` in terms of the `split` function

* Complete `image-painter`

You should check out the docs for [Quil transform](http://quil.info/api/transform)

### Extensions

* Find some other escher tiles and see if you can capture the pattern of the orignal work with the functions we have

* Check out [this page](http://www.frank-buss.de/lisp/functional.html) from Frank Buss and see if you can use his tile shape for a perfect `segment-painter` Square Limit

## Advanced, by which I mean I have not done it myself :-)
* Can you do Circle Limit?

![alt tag](https://raw.github.com/SICPDistilled/escher/master/doc/CircleLimit4.jpg)

(Might want to search for 'escher hyperbolic')
