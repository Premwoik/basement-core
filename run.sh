#!/bin/sh

clean_up() {
  # circuts pomp
  echo "18" > /sys/class/gpio/unexport
  # circut higher bathroom
  echo "23" > /sys/class/gpio/unexport
  # circut lower bathroom
  echo "24" > /sys/class/gpio/unexport
  # floor heating lower bathroom
  echo "25" > /sys/class/gpio/unexport
  # leds lower bathroom
  echo "8" > /sys/class/gpio/unexport
  echo "GPIO 18, 23, 24, 25, 8 unexported"
  exit 0
}

echo "18" > /sys/class/gpio/export
sudo sh -c 'echo "high" > /sys/class/gpio/gpio18/direction'
echo "23" > /sys/class/gpio/export
sudo sh -c 'echo "high" > /sys/class/gpio/gpio23/direction'
echo "24" > /sys/class/gpio/export
sudo sh -c 'echo "high" > /sys/class/gpio/gpio24/direction'
echo "25" > /sys/class/gpio/export
sudo sh -c 'echo "high" > /sys/class/gpio/gpio25/direction'
echo "8" > /sys/class/gpio/export
sudo sh -c 'echo "high" > /sys/class/gpio/gpio8/direction'
echo "GPIO 18, 23, 24, 25, 8 exported"

trap clean_up INT

cd ~/Coding/basement-core
rebar3 shell --name heating@192.168.2.142 --setcookie COOKIE

clean_up
