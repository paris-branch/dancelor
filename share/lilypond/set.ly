\version "2.19.82"

\header {
  title = "{{name}}"
  subtitle = "{{kind}}"
  tagline = ""
}

\paper {
  indent = 0
}

{{#tunes}}
\score {
  \header {
    piece = "{{tune.name}}"
    opus = "{{#tune.author}}{{line}}{{/tune.author}}"
  }

  {{{version.content}}}
}
{{/tunes}}
