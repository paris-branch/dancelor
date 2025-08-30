\defineBarLine "[|:-||" #'("||" "[|:" "")

\layout {
  \context {
    \Score

    startRepeatType = #"[|:-||"
    endRepeatType = #":|]"
    doubleRepeatType = #":|][|:"
  }
}
