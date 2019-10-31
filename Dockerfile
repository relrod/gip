FROM registry.fedoraproject.org/fedora-minimal

EXPOSE 3000
COPY . /build
WORKDIR /build
RUN microdnf install ghc cabal-install && \
  microdnf clean all && \
  cabal update && \
  cabal install --only-dependencies && \
  cabal configure && cabal build && \
  ./pull-latest-geoip-data.sh
CMD ["cabal", "run"]