\paper {
  indent = 0

  #(define fonts
    (make-pango-font-tree
     "Trebuchet MS"
     "Nimbus Sans"
     "Luxi Mono"
     (/ staff-height pt 20)))

  ragged-right = ##f
  ragged-bottom = ##t
  ragged-last-bottom = ##t
  markup-markup-spacing = #'((basic-distance . 15) (padding . 0.5))
  score-markup-spacing = #'((basic-distance . 15) (padding . 0.5) (stretchability . 60))
}
