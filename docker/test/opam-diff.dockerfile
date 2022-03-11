FROM dancelor_build

RUN find . -name '*.opam' | while read -r file; do \
      if ! git diff --quiet "$file"; then \
        echo "/home/opam/$file: differs before and after build:"; \
        git diff --color "$file"; \
        exit 1; \
      fi; \
    done
