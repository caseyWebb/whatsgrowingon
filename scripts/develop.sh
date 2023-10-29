#!/usr/bin/env bash

mkdir -p .run

lamdera_live_pid_file="./.run/lamdera_live.pid"
passkey_service_pid_file="./.run/passkey_service.pid"
ports_compiler_pid_file="./.run/ports_compiler.pid"

cleanup() {
  echo "Terminating lamdera..."
  kill $(cat $lamdera_live_pid_file)
  rm $lamdera_live_pid_file

  echo "Terminating passkey service..."
  kill $(cat $passkey_service_pid_file)
  rm $passkey_service_pid_file
  
  echo "Terminating ports compiler..."
  kill $(cat $ports_compiler_pid_file)
  rm $ports_compiler_pid_file
  
  exit 0
}

trap 'cleanup' INT

echo "Running generate script..."
./scripts/generate.sh

echo "Starting lamdera..."
lamdera live &
echo $! > $lamdera_live_pid_file

echo "Starting passkey service..."
(
  cd workers/passkey
  npm run dev &
  echo $! > ../../$passkey_service_pid_file
)

echo "Starting ports compiler with --watch=forever..."
esbuild ports/WebAuthn.js --bundle --format=cjs --watch=forever --outfile=elm-pkg-js/webauthn.js &
echo $! > $ports_compiler_pid_file

echo "Monitoring ports compiler output directory 'elm-pkg-js' for changes..."
fswatch -o elm-pkg-js | while read f; do
  echo "Output in 'elm-pkg-js' changed, restarting lamdera..."
  kill $(cat $lamdera_live_pid_file)
  lamdera live &
  echo $! > $lamdera_live_pid_file
done &

wait
