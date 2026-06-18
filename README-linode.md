# Linode GPU boxes

This note records the current `mark4` Linode provisioning path so the next GPU
box starts with NVIDIA drivers installed instead of requiring manual driver
repair and a power cycle.

## Current provisioning hook

Use the private Linode StackScript:

- label: `mark4-ubuntu2404-gpu-bootstrap`
- id: `2142757`
- image: `linode/ubuntu24.04`
- repo source: `~/code/futon6/scripts/linode-gpu-bootstrap-stackscript.sh`
- registration helper: `~/code/futon6/scripts/linode-register-gpu-stackscript.sh`
- committed in `futon6`: `60ec1d012436320fbd6a0f037831921b19a3708e`

The StackScript runs at first boot, installs the recommended NVIDIA driver with
`ubuntu-drivers autoinstall`, optionally installs `nvidia-cuda-toolkit` and
`linode-cli`, then reboots. After that reboot, `nvidia-smi` should work before
the mark4 vLLM setup starts.

## Create the next 4-GPU box

Replace the root password and region/type as needed:

```bash
linode-cli linodes create \
  --region us-ord \
  --type g2-gpu-rtx4000a4-s \
  --image linode/ubuntu24.04 \
  --stackscript_id 2142757 \
  --label mark4-70b-$(date +%Y%m%d) \
  --root_pass 'REPLACE-ME' \
  --authorized_keys "$(cat ~/.ssh/id_ed25519.pub)"
```

Known 4-GPU types from `linode-cli linodes types --gpus 4`:

- `g2-gpu-rtx4000a4-s` — RTX4000 Ada x4 Small, 128 GB RAM, 32 vCPU
- `g2-gpu-rtx4000a4-m` — RTX4000 Ada x4 Medium, 196 GB RAM, 48 vCPU
- `g1-gpu-rtx6000-4` — RTX6000 x4, 128 GB RAM, 24 vCPU

## After the StackScript reboot

SSH to the box and verify:

```bash
nvidia-smi
```

Then clone or rsync `futon6`, and run:

```bash
cd ~/futon6
scripts/linode-postsetup-deps.sh    # CPU-side gate deps (babashka); idempotent
scripts/linode-4gpu-setup.sh        # vLLM 70B serve
scripts/linode-4gpu-run.sh
```

`linode-postsetup-deps.sh` installs the dependencies the IATC pipeline shells out
to (currently babashka `bb`, for the per-paper repair/argcheck/semcheck gates) so
the staged runner doesn't die mid-run on a missing binary. It's idempotent — run
it on every fresh box. `linode-4gpu-setup.sh` creates `$HOME/mark4-venv` and serves
vLLM with the preregistered mark4 flags. `linode-4gpu-run.sh` uses the same venv by
default, rebuilds or validates enriched IATC candidates, and refuses to call the
model on pre-enrichment candidates.

## Updating the StackScript

After editing `~/code/futon6/scripts/linode-gpu-bootstrap-stackscript.sh`, run:

```bash
cd ~/code/futon6
scripts/linode-register-gpu-stackscript.sh
```

That helper updates StackScript `2142757` in place if it already exists, or
creates it if missing.

## Notes

- There is no Linode-wide "always install GPU drivers" toggle in `linode-cli`;
  the reusable provisioning mechanism is `--stackscript_id`.
- If the StackScript installs drivers and reboots successfully, claude-1 should
  not need to do manual driver repair before the mark4 run.
- If `nvidia-smi` fails after reboot, inspect `/var/log/mark4-gpu-bootstrap.log`
  on the Linode.
- **flashinfer sampler (fixed in `linode-4gpu-setup.sh` 2026-06-18):** on the
  first real run, vLLM's engine core died at startup because the flashinfer
  *sampler* JIT-compiles CUDA kernels that don't build against the StackScript's
  CUDA 12.0 toolchain (flashinfer 0.6.12 cub: `BlockAdjacentDifference ... has no
  member "FlagHeads"`). The setup script now exports `VLLM_USE_FLASHINFER_SAMPLER=0`
  unconditionally (native PyTorch sampling, proven faithful), instead of only when
  `nvcc` is absent — `nvcc` present does not imply the flashinfer sampler compiles.
  If vLLM ever fails engine init with a `ninja`/flashinfer error, this is the knob.
