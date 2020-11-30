tocSet = #(define-music-function (parser location text) (markup?)
           (add-toc-item! 'tocSetMarkup text))

tocVersion = #(define-music-function (parser location text) (markup?)
            (add-toc-item! 'tocVersionMarkup text))

#(define-markup-command (my-wordwrap-string layout props align strg)
  (number? string?)
  #:properties ((baseline-skip))
  #:category align
  "Same as @code{wordwrap-field}, but internally a stencil-list is produced
  first, which will aligned according to @var{align}, putting out a single
  stencil."

  ;; c/p from define-markup-commands.scm, because it's not public
  (define (general-column align-dir baseline mols)
   "Stack @var{mols} vertically, aligned to  @var{align-dir} horizontally."
   (let* ((aligned-mols
    (map (lambda (x) (ly:stencil-aligned-to x X align-dir)) mols))
    (stacked-stencil (stack-lines -1 0.0 baseline aligned-mols))
    (stacked-extent (ly:stencil-extent stacked-stencil X)))
    (ly:stencil-translate-axis stacked-stencil (- (car stacked-extent)) X)))

  (general-column
   align
   baseline-skip
   (wordwrap-string-internal-markup-list layout props #f strg)))
