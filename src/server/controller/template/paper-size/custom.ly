%% FIXME: with LilyPond 2.23, one can directly pass a custom format to
%% `set-default-paper-size`.
%%
%% cf https://lilypond.org/doc/v2.23/Documentation/notation/paper-size-and-automatic-scaling

#(set! paper-alist
  (cons '("custom format" . (cons (* %f %s) (* %f %s))) paper-alist))

#(set-default-paper-size "custom format")
