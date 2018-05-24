#!/bin/sh

curl -O -L https://s3.amazonaws.com/rebar3/rebar3
chmod +x rebar3
./rebar3 update

rm config/config_socks5_test.config
mv config/config_socks5_travis.config config/config_socks5_test.config