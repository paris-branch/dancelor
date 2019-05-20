\paper {
  ragged-right = ##f
  ragged-bottom = ##t
  ragged-last-bottom = ##t
  markup-markup-spacing = #'((basic-distance . 15) (padding . 0.5))
  score-markup-spacing = #'((basic-distance . 15) (padding . 0.5) (stretchability . 60))

  oddHeaderMarkup = \markup {
    \on-the-fly \not-first-page {
      \dir-column {
	\fill-line {
	  \bold \fromproperty #'header:title
	  \fromproperty #'header:instrument
	}

	" "
      }
    }
  }
  evenHeaderMarkup = \oddHeaderMarkup

  oddFooterMarkup = \markup {
    \on-the-fly \not-first-page {
      \fill-line {
	" "
	\concat { "Page " \fromproperty #'page:page-number-string }
      }
    }
  }
  evenFooterMarkup = \oddFooterMarkup
}
