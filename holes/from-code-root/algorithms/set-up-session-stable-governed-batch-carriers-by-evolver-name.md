# Algorithm: Set Up Session-Stable Governed Batch Carriers By Evolver Name

Date: 2026-03-24
Status: active

## Purpose

Use this when a live Codex/Desktop session needs recurring governed `.bat`
execution and the operator wants:

- one stable approved helper target for the rest of the session
- one stable approved writer target for the rest of the session
- old tmp artifacts available later for mining and world-model updates
- no repeated approval churn from pointing the permitted helper at a different
  leaf batch on every run

This algorithm sets up evolver-local carrier files and the stable write lane
for those carriers. It does not own the domain-semantic work performed by the
leaf batches.

Opening-header gate:
- the resulting carrier setup must make it obvious which evolver owns it
- the chosen evolver name must be visible in the stable carrier filenames
- the operator should not have to reverse-engineer whether a given `current`
  batch belongs to Bob, Alice, or some other evolver
- the evolver-named tmp directory is that evolver's personal session workspace;
  if lane separation is useful, put the lane underneath that personal root
- the algorithm must not confuse "kept in tmp for mining" with "still the
  rightful durable owner"

## Inputs

- the current evolver identity, named with a short stable stem such as:
  - `bob`
  - `alice`
- whether the evolver needs:
  - command carriers
  - read/probe carriers
  - Python helper carriers
- whether the evolver needs a stable writer, which is the normal case
- the current work root, usually `gh`

## Preconditions

1. The work involves governed general `.bat` files rather than an actual
   `agent_skill` surface.
2. The operator wants one stable helper target for the remainder of the
   session.
3. The evolver can be named honestly with a short stable stem.
4. The temp/artifact neighborhood will live under `mfuton/data/tmp/...`
   instead of OS temp.
5. Everyone involved understands that repeated reuse or obvious future value
   means the helper should be promoted out of `tmp`.

## Procedure

1. Decide whether this is the right surface.
   - If the thing being invoked is an actual `agent_skill`, do not use this
     algorithm.
   - Use:
     - `agent_skills/mfuton-agent-skill.bat`
   - Stop this algorithm after that classification.
2. Choose one short evolver name and keep it for the rest of the session.
   - Examples:
     - `bob`
     - `alice`
   - Do not keep inventing new carrier names mid-session.
3. Create or reuse an evolver-local tmp neighborhood.
   - Preferred personal-root shape:
     - `mfuton/data/tmp/<evolver-name>/`
   - This directory is the evolver's personal workspace for the session.
   - If lane separation is useful, prefer:
     - `mfuton/data/tmp/<evolver-name>/<lane>/`
   - This tmp neighborhood may contain both:
     - active current carriers
     - older mineable artifacts from the same evolver or lane
4. Create or reuse stable evolver-local run/read carrier files under that
   neighborhood.
   - Preferred shapes:
     - `mfuton/data/tmp/<evolver-name>/<evolver-name>-command-current.bat`
     - `mfuton/data/tmp/<evolver-name>/<evolver-name>-read-current.bat`
     - `mfuton/data/tmp/<evolver-name>/<evolver-name>-python-current.bat`
   - Optional lane-separated shapes:
     - `mfuton/data/tmp/<evolver-name>/<lane>/<evolver-name>-command-current.bat`
     - `mfuton/data/tmp/<evolver-name>/<lane>/<evolver-name>-read-current.bat`
     - `mfuton/data/tmp/<evolver-name>/<lane>/<evolver-name>-python-current.bat`
   - Create only the carrier types the evolver actually needs.
5. Create or reuse the stable write lane for the same neighborhood.
   - Preferred shapes:
     - `mfuton/data/tmp/<evolver-name>/<evolver-name>-write-current.bat`
     - `mfuton/data/tmp/<evolver-name>/<evolver-name>-write-target.tmp`
     - `mfuton/data/tmp/<evolver-name>/<evolver-name>-write-body.tmp`
   - Optional lane-separated shapes:
     - `mfuton/data/tmp/<evolver-name>/<lane>/<evolver-name>-write-current.bat`
     - `mfuton/data/tmp/<evolver-name>/<lane>/<evolver-name>-write-target.tmp`
     - `mfuton/data/tmp/<evolver-name>/<lane>/<evolver-name>-write-body.tmp`
- The stable write carrier should call one currently intended mineable leaf
  writer batch.
   - The fixed tmp inputs should provide:
     - the target file path to write
     - the body text to write
6. Keep mineable leaf workflow batches beside the carriers.
   - Use descriptive or date-stamped names for leaf batches, for example:
     - `<evolver-name>-2026-03-24-frontiermath-live-launch-and-probe.bat`
     - `<evolver-name>-2026-03-24-direct-invoke.bat`
     - `<evolver-name>-2026-03-24-write-file-from-tmp.bat`
   - Do not delete those leaf batches by default; keep them for later mining
     unless the operator explicitly asks for deletion.
   - But do not mistake that retention for durable ownership.
   - If repeated reuse shows the leaf batch has ongoing future purpose, or if
     that durable value is already obvious, promote it out of `tmp` into a
     real owner neighborhood instead of leaving `tmp` as the long-term owner.
7. Use the stable write lane to create or update mineable leaf batches.
   - Do not hand-edit the mineable leaf batch on the live shell path.
   - Write the target leaf path into:
     - `<evolver-name>-write-target.tmp`
   - Write the desired batch body into:
     - `<evolver-name>-write-body.tmp`
   - Run the stable write carrier so the mineable leaf batch is created or
     updated through the same approved path each time.
8. Use the stable write lane to retarget the stable run/read carrier.
   - The stable `...-current.bat` should call the leaf batch that is currently
     wanted.
   - Put the carrier path itself into `<evolver-name>-write-target.tmp`.
   - Put the small "call the mineable leaf batch" wrapper body into
     `<evolver-name>-write-body.tmp`.
   - Run the same stable write carrier again.
   - This keeps the historical leaf batch available for later mining while
     only the stable carrier pointer changes.
9. Invoke governed general `.bat` work through the permitted helper using the
   stable carrier path.
   - Preferred shape:
     - `cmd /c "mfuton\agent_skills\permissions\mfuton_agent_skill_bat_permitted.bat <work-root> mfuton/data/tmp/<evolver-name>/<evolver-name>-command-current.bat"`
   - Optional lane-separated shape:
     - `cmd /c "mfuton\agent_skills\permissions\mfuton_agent_skill_bat_permitted.bat <work-root> mfuton/data/tmp/<evolver-name>/<lane>/<evolver-name>-command-current.bat"`
   - Keep that helper target stable for the remainder of the session.
10. Use the same evolver-named carrier files for the rest of the session.
   - Do not switch to a different stable carrier name mid-session unless the
     evolver itself changes.
   - If a different evolver takes over, run this setup again with that
     evolver's honest name.

## Decision Rule

Default rule:
1. if the invoked thing is an actual `agent_skill`, use
   `agent_skills/mfuton-agent-skill.bat` instead
2. if the work is governed general `.bat` execution, use evolver-named stable
   run/read carriers plus a stable writer under
   `mfuton/data/tmp/<evolver-name>/`
3. keep the run helper target path stable for the remainder of the session
4. keep the write helper target path stable for the remainder of the session
5. keep old tmp batches for mining unless the operator explicitly asks for
   deletion
6. if repeated reuse or obvious future value appears, promote the helper out of
   `tmp` into a real owner neighborhood
7. create or retarget those leaf batches and carriers through the stable write
   lane rather than direct ad hoc shell edits

## Outputs

- one evolver-local tmp neighborhood under `mfuton/data/tmp/<evolver-name>/`,
  which is the evolver's personal session workspace
- optional lane-separated subdirectories under that personal root when
  frontiermath/irc/etc separation is useful
- stable run/read `...-current.bat` carrier files named with the chosen
  evolver stem
- one stable write carrier plus fixed write tmp inputs
- older mineable `.bat` artifacts beside those carriers when the session has
  already produced them
- a stable permitted-helper run command shape that can be reused for the rest
  of the session
- a stable permitted-helper write command shape that can be reused for the
  rest of the session

## Stop Conditions

Stop and reclassify if:
- the invoked thing is actually an `agent_skill`
- the evolver name is still unclear or keeps shifting
- the work should instead be a typed command surface or a real `agent_skill`
  rather than a governed general `.bat`
- the helper has become repeatedly useful enough that it should now be
  promoted out of `tmp` into a proper owner

## References

- `agent_skills/README.md`
- `agent_skills/development/README.md`
- `agent_skills/permissions/README.md`
- `agent_skills/permissions/mfuton_agent_skill_bat_permitted.bat`
- `agent_skills/mfuton-agent-skill.bat`
- `holes/missions/M-mfuton-familiar-typed-surface-world-and-memory/M-evolver-session-environment-and-worktree-fabric/M-development-agent-skills-surface/M-stable-evolver-live-read-and-command-carriers.md`
- `algorithms/create-algorithm.md`
