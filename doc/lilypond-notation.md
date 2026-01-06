# LilyPond notation

Dancelor uses [LilyPond](https://lilypond.org) notation to write music. However, we only use a small, practical subset of LilyPond's features—just enough to write tunes clearly and efficiently.

This guide covers what you need to know to enter music into Dancelor, particularly for destructured versions.

## The basics

This should be enough to let you enter destructured versions.

### Basic notes

Notes are written with letters: `a b c d e f g`

By default, Dancelor uses **relative mode**, which means each note is assumed to be close to the previous one. For example:

- `a b c` gives you A, B, C going up
- `g f e` gives you G, F, E going down
- `g b` gives you G and the B above it (not the B below)

This makes writing melodies intuitive—you write what you hear.

### Accidentals

Accidentals are written after the note name:

- **Sharp**: add `is` (e.g., `fis` for F♯, `cis` for C♯)
- **Flat**: add `es` (e.g., `bes` for B♭, `aes` for A♭)

**Important**: You must always write the accidental, even if it appears in the key signature. For example, in D major, you still need to write `fis` for every F♯—it won't be assumed from the key signature.

### Octaves

If a note is far from the previous one, use octave modifiers:

- `'` (apostrophe) moves up an octave
- `,` (comma) moves down an octave

Examples:

- `c c' c c,` = C, C an octave higher, then that same C, then back to the original C
- `c g c g'` = C, G below, C again, G above

### Note durations

Durations are written after the note:

- `4` = quarter note (crotchet)
- `8` = eighth note (quaver)
- `16` = sixteenth note (semiquaver)
- `2` = half note (minim)
- `1` = whole note (semibreve)

Add a dot for dotted notes: `8.` for a dotted quaver.

Examples:

- `a4 b8 c16` = A quarter, B eighth, C sixteenth
- `d16 e8.` = sixteenth D, dotted eighth E (the scotch snap!)

Once you specify a duration, it continues for subsequent notes until you change it:

```
a4 b c8 d e f
```

This gives: A quarter, B quarter, C eighth, D eighth, E eighth, F eighth.

### Bar checks

Use `|` to mark where bars should be. This helps LilyPond (and you!) catch counting errors:

```
a4 b c d | e f g a | b c' d' e' |
```

The `|` doesn't affect how the music sounds—it's just a safety check. Do use it!

### Ties and slurs

- **Tie** (same note held across): use `~` after the note

  ```
  a4~ a8 b
  ```

  The two A's are played as one note lasting a dotted quarter.

- **Slur** or **phrasing**: use parentheses `( )`
  ```
  a8( b c d)
  ```
  The notes are slurred together (played smoothly).

### Chords

For chord accompaniment, write the chord after the note:

- `a4:m` = A minor chord, spanning a quarter
- `d:7` = D dominant 7th
- `g:maj7` = G major 7th
- `b8.:dim` = B diminished, spanning a dotted eighth

Examples:

```
a4:m d:m e:7 a:m
```

You may also use `s` (for skip) which will span time but print nothing. This is
typically used to indicate that the chord does not change, eg. every other bar.

### Multiple voices

When you need two voices in the same staff (like a melody with harmony), use the `\twoVoices` helper:

```
\twoVoices
  { a4 b c d }  % Upper voice
  { a4 g f e }  % Lower voice
```

### Repeats

Traditional tunes often have repeated sections. Use `\repeat volta` followed by the number of repetitions to indicate repeats:

```
\repeat volta 2 {
  a4 b c d | e f g a |
}
```

This plays the section twice (with repeat bars in the notation). Using the wrong number will print the same but will mess up the bar numbers.

## Putting it together

Here's a simple example of a tune part in A major:

```
a8 b | cis'4 b8 a fis e | fis a a b | cis'4 b8 a fis e | d4. cis8 |
```

This shows:

- Pickup notes before the first bar
- Quarter and eighth notes
- Accidentals (cis' for C♯)
- Bar checks with `|`
- Dotted rhythms (d4.)

## Monolithic versions (advanced)

When entering a complete tune as a single block (rather than a destructured version), you may need to specify additional musical information at the beginning:

### Key signature

Use `\key` to set the key signature:

```
\key d \major
\key g \minor
\key a \mixolydian
```

### Time signature

Use `\time` to set the time signature:

```
\time 4/4
\time 3/4
\time 6/8
\time 9/8
```

### Clef

Use `\clef` to set the clef:

```
\clef treble
\clef bass
\clef alto
```

These commands typically appear at the very beginning of a monolithic version:

```
\clef treble
\key d \major
\time 4/4
a4 b cis' d' | ...
```

For destructured versions, you usually don't need these—Dancelor handles key and time signatures automatically based on the tune's metadata.

### Relative mode

Note: Unlike destructured versions where relative mode is the default, in monolithic versions you need to enable it manually. We recommend using relative mode everywhere:

```
\relative f' {
  a b c d
}
```

Inside the braces, the notes will be taken to be relative to each other. The
first reference is `f'`, which is the F4 on the piano.

### Structure markers

For longer pieces, you can mark sections:

- `\section` marks a new section, which is typically 8 bars in a Scottish tune
- `\fine` marks the end of the piece

Example:

```
\repeat volta 2 {
  a4 b c d |
}
\section
\repeat volta 2 {
  e4 f g a |
}
\fine
```

### First and second time repeat bars

When the ending differs on a repeat, use `\alternative`:

```
\repeat volta 2 {
  a4 b c d | e f g a |
}
\alternative {
  { b4 c d e | }     % First time ending
  { b4 c d c | }     % Second time ending
}
```

The syntax is admittedly a bit odd—note the nested curly braces `{ { } { } }`—but this is how LilyPond structures it. The outer braces contain the `\alternative` block, and each inner pair of braces contains one ending.

## Learning more

This covers the essentials for writing tunes in Dancelor. If you're already familiar with LilyPond, note that we're using a simplified subset focused on traditional dance music notation. Full LilyPond has many more features, but these basics are all you need for most traditional tunes.
