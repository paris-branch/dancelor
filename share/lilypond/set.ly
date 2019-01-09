\version "2.19.82"

\header {
  title = "{{{name}}}"
  subtitle = "{{{kind}}}"
  {{#transpose}}
    instrument = "{{instrument}} Instruments"
  {{/transpose}}
  tagline = ""
}

\layout {
  indent = 0
  \context {
    \Score
    \override NonMusicalPaperColumn.page-break-permission = ##f
  }
}

\paper {
  ragged-right = ##f
  ragged-bottom = ##t
  ragged-last-bottom = ##t
  markup-markup-spacing = #'((basic-distance . 15) (padding . 0.5))
  score-markup-spacing = #'((basic-distance . 15) (padding . 0.5) (stretchability . 60))
}

{{#tunes}}
\score {
  \header {
    piece = "{{{tune.name}}}"
    opus = "{{#tune.author}}{{{line}}}{{/tune.author}}"
  }

  {{#transpose}}\transpose {{{target}}} c { {{/transpose}}
    {{{version.content}}}
  {{#transpose}} } {{/transpose}}
}
{{/tunes}}
