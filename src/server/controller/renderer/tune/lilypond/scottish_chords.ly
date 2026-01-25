jigChords =
  #(define-music-function (music) (ly:music?)
    (jig-chords music))

reelChords =
  #(define-music-function (music) (ly:music?)
    (reel-chords music))

strathspeyChords =
  #(define-music-function (music) (ly:music?)
    (reel-chords music)) %% we just use the reel chords

waltzChords =
  #(define-music-function (music) (ly:music?)
    (waltz-chords music))

otherChords =
  #(define-music-function (music) (ly:music?)
    music) %% we do nothing
