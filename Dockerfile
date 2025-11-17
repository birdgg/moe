ARG GHC_VERSION=9.12.2

FROM docker.io/benz0li/ghc-musl:${GHC_VERSION} AS builder

WORKDIR /mnt
COPY . .
RUN chmod +x build.sh && ./build.sh

FROM alpine:3.19 AS runtime
RUN adduser -D moe
WORKDIR /app
RUN mkdir -p static
COPY --from=builder /mnt/out/moe /app/moe

USER moe
EXPOSE 4000
CMD ["./moe"]
