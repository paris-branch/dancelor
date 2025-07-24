\relative c' <<
  {
    \clef treble
    \key d \minor
    \time 4/4

    \repeat volta 2 {
      \partial 8 r8 |
      a4 d8 a f' a, d a |
      bes4 d8 bes f' bes, d bes |
      c4 e8 c g' c, e g |
      f8 e d c d c a g |
      \break

      a4 d8 a f' a, d a |
      bes4 d8 bes f' bes, d bes |
      c4 e8 c g' c, e g |
      f8 e d c d4.
    } \break

    \repeat volta 2 {
      a'8 |
      d8 a a a f a d, a' |
      d8 a a a f a d, a' |
      c8 g g g c g e' g, |
      c8 g g g c[ r c cis] |
      \break

      d8 a a a f a d, a' |
      d8 a a a f[ a] d, r |
      bes8 a bes c d c d e |
      f8 e d c a[ d] d
    }
  }

  \new ChordNames {
    \chordmode {
      s8 |
      d1:m | bes | c | d2:m a:m |
      d1:m | bes | c | a2:m d4.:m

      s8 |
      d1:m | s | c | s |
      d1:m | s | g:m | a2:m d4.:m
    }
  }

  \new ChordNames {
    \chordmode {
      s8 |
      s1 | \parenthesize g:m | s | s |
      s1 | \parenthesize g:m | s | s2 s4.

      s8 |
      s1 | s | s | s |
      \parenthesize bes1 | s | s | s2 s4.
    }
  }
>>
