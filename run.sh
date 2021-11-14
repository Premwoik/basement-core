#!/bin/sh
cd ~/Coding/basement_core/
rebar3 shell --name heating@192.168.2.112 --setcookie COOKIE
