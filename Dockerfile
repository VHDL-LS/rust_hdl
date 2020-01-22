ARG RUST_VERSION=stable
FROM clux/muslrust:$RUST_VERSION as builder
WORKDIR /volume
COPY . /volume/
ARG CRATE
RUN cargo build --manifest-path $CRATE/Cargo.toml --release --features "packaged"

FROM scratch
ARG CRATE
COPY --from=builder /volume/target/x86_64-unknown-linux-musl/release/$CRATE /app/bin/$CRATE
COPY --from=builder /volume/vhdl_libraries /app/vhdl_libraries
ENTRYPOINT /app/bin/$CRATE
