FROM alpine:latest
RUN apk add --no-cache git
RUN mkdir /app
WORKDIR /app
RUN ["git", "clone", "https://github.com/scheme-requests-for-implementation/srfi-222"]
RUN mkdir /app/conditions
COPY conditions* /app/conditions/

FROM schemers/gauche:latest
COPY --from=0 /app /app
CMD ["gosh", "-I", "/app/conditions", "-I", "/app/srfi-222", "/app/conditions/conditions-test.scm"]
