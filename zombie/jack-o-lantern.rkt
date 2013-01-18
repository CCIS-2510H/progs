#lang class/1
(require (prefix-in pict: slideshow/pict))
(provide jack-o-lantern)
(define (jack-o-lantern r color)
  (pict:pict->bitmap (pict:jack-o-lantern (* 2 r) color)))