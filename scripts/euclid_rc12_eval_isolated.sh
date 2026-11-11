#!/usr/bin/env bash
set -u

mode=${1:?usage: euclid_rc12_eval_isolated.sh perturbed|from-start}
root=/home/joe/code/futon0
out=$root/holes/excursions/E-euclid-tutor-data/spiel/rc12/stage-2
runner=$root/scripts/euclid_open_spiel_alpha_zero.py
python=/home/joe/.venvs/openspiel/bin/python

if [[ $mode == perturbed ]]; then
  prefix=$out/prefix.json
  move_cap=4
  perturb=2
  seed=2026111202
elif [[ $mode == from-start ]]; then
  prefix=$root/holes/excursions/E-euclid-tutor-data/spiel/rc12/initial-prefix.json
  move_cap=10
  perturb=0
  seed=2026111300
else
  echo "unknown mode: $mode" >&2
  exit 2
fi

for index in {0..9}; do
  game_seed=$((seed + index))
  log=$out/eval-$mode-game-$index.log
  record=$out/eval-$mode-game-$index-wins.jsonl
  env PYTHONHASHSEED=0 OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 \
    MKL_NUM_THREADS=1 XLA_FLAGS=--xla_cpu_multi_thread_eigen=false \
    taskset -c 20-31 timeout --signal=TERM --kill-after=5s 45s \
    "$python" "$runner" --proposition 2 --out "$out" \
    --simulations 32 --eval-step 0 --eval-games 1 --eval-game-seconds 30 \
    --start-file "$prefix" --move-cap "$move_cap" \
    --start-perturb "$perturb" --start-seed "$game_seed" \
    --eval-record "$record" --win-in-one >"$log" 2>&1
  code=$?
  printf '%s\n' "$code" >"$out/eval-$mode-game-$index.status"
done
