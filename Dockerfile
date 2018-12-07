ARG RUST_VERSION=stable
FROM clux/muslrust:$RUST_VERSION as builder
WORKDIR /volume
COPY . /volume/
ARG CRATE
RUN cargo build --package $CRATE --release

FROM scratch
ARG CRATE
COPY --from=builder /volume/target/x86_64-unknown-linux-musl/release/$CRATE ./app
ENTRYPOINT ["./app"]
