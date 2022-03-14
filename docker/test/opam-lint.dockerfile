FROM dancelor_files

RUN if [ -n "$(opam lint --short)" ]; then \
      opam lint; \
      exit 1; \
    fi
