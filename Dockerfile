FROM registry.fedoraproject.org/fedora-minimal

EXPOSE 3000
COPY . /build
WORKDIR /build
RUN microdnf install tar wget ghc cabal-install && \
  microdnf clean all && \
  cabal update && \
  cabal install --only-dependencies && \
  cabal configure && cabal build && \
  ./pull-latest-geoip-data.sh
USER 1001
CMD ["cabal", "run"]
