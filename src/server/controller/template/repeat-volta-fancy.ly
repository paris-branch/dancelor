\defineBarLine "[|:-||" #'("||" "[|:" "")

\layout {
  \context {
    \Score

    startRepeatBarType = #"[|:-||"
    endRepeatBarType = #":|]"
    doubleRepeatBarType = #":|][|:"
  }
}
