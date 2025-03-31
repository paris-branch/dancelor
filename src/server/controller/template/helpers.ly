twoVoices =
  #(define-music-function (one two) (ly:music? ly:music?)
   #{ << { \voiceOne #one } \new Voice { \voiceTwo #two } >> \oneVoice #})
