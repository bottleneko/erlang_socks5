release-docker:
	docker build -t bottleneko/erlang_socks5 .

release: clean-release
	rebar3 release

clean-release:
	rm -rf _build/default/rel
