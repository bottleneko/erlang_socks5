FROM erlang:22.0 AS builder

COPY . /opt/erlang_socks5

RUN set -x \
  && cd /opt/erlang_socks5 \
  && make release

FROM buildpack-deps:stretch

RUN set -x \
  && groupadd -g 1000 erlang_socks5 \
  && useradd -g erlang_socks5 -m -u 1000 erlang_socks5

COPY --from=builder --chown=erlang_socks5:erlang_socks5 /opt/erlang_socks5/_build/default/rel/* /opt/erlang_socks5/

WORKDIR /opt/erlang_socks5

USER erlang_socks5:erlang_socks5

CMD ["/opt/erlang_socks5/bin/erlang_socks5", "foreground"]
