# Algorithm: Onboard Additional Evolver Work Root

Date: 2026-03-28
Status: active

## Purpose

Use this when a folder outside the currently ordinary local roots should become
a stable evolver-visible work root for `My Familiar`.

Typical examples:

- a sibling repo such as `redistrict`
- a host-local project root such as `I:/xi`
- a durable working corpus that is not naturally owned by `mfuton`

This algorithm exists so "we can see the folder" is not mistaken for "the
folder is now an admitted work root."

## Inputs

- the candidate folder path
- the current motivating lane or child mission
- the folder's likely semantic class:
  - sibling repo
  - loose project root
  - durable working corpus
  - something else
- the intended evolver posture:
  - read-only
  - read/write
- whether the folder is expected to stay host-specific
- one or more concrete artifacts inside the folder that justify admission

## Preconditions

1. The folder is not already an ordinary admitted work root with clear local
   policy and citation posture.
2. The folder matters to real ongoing work, not only to speculative browsing.
3. The operator wants evolvers to treat the folder as a real working surface.
4. At least one concrete motivating artifact inside the folder is known.

## Procedure

1. Classify the candidate root honestly.
   - Decide whether it is:
     - a sibling repo
     - a loose project root
     - a durable working corpus
     - or merely scratch/cache/download residue
   - If it is only scratch/cache/download residue, stop and do not admit it.
2. Inspect the root contents.
   - Identify the concrete motivating artifacts.
   - Identify whether the root already has its own project structure,
     notebooks, papers, source, chats, or other durable organization.
3. Decide the access posture.
   - Record whether evolvers need:
     - read-only access
     - or read/write access
   - Record whether the root should count as trusted local write surface.
4. Decide the ownership split.
   - Identify the broad familiar-level owner for the recurring pattern.
   - Identify the narrower current child or mode/object lane that is the
     immediate consumer of the new root.
   - Do not let the narrower consumer silently own the whole recurring pattern.
5. Decide durable citation/path posture.
   - If the root is genuinely host-specific, record the absolute host path
     explicitly.
   - If the root has a stronger semantic project name or repo name, prefer
     that semantic form in durable artifacts whenever host specificity is not
     required.
6. Create or update the general owner for recurring root admission.
   - If no honest owner exists yet, create one.
   - Record that future admissions should reuse this owner and this algorithm.
7. Create or update the bounded consumer child mission.
   - Record the concrete local reason this root is being admitted now.
   - Keep the child bounded to the current project/object/use case.
8. Wire minimal discoverability.
   - Update mission indexes and lane references enough that a later evolver can
     rediscover the admitted root and the owning pattern.
9. Prove access before declaring success.
   - Read at least one concrete motivating artifact from the new root.
   - Read enough of the surrounding root layout to verify the root is real and
     not merely remembered.
   - Record what was actually observed.
10. Only after the proof step, treat the root as admitted.

## Decision Rule

Default rule:
1. if the root is merely scratch/cache/provider-placeholder residue, do not
   admit it
2. if the root matters only to one bounded current lane, still create or reuse
   a general owner when the admission pattern will recur
3. keep the recurring admission pattern owned by a broad familiar-level
   component
4. keep the current consumer bounded in its own child mission
5. do not claim admission until at least one motivating artifact has been read
   from the root in the current live session

## Outputs

- one honest classification of the candidate root
- one explicit access posture
- one general owner mission for recurring project/work-root admission
- one bounded current child mission for the specific root/use case
- minimal mission/index wiring so later evolvers can rediscover the result
- one concrete access proof based on observed root contents

## Stop Conditions

Stop and reclassify if:

- the candidate root is really temp/cache/download residue
- the root is a broken cloud-placeholder lane rather than a usable local
  working surface
- no concrete motivating artifact can be identified
- the work is actually about a command surface or transport boundary rather
  than folder/work-root admission

## References

- `holes/missions/M-mfuton-familiar-typed-surface-world-and-memory/M-project-work-root-admission.md`
- `holes/missions/M-mfuton-familiar-typed-surface-world-and-memory/M-evolvers-as-first-class-local-actors.md`
- `holes/missions/M-mfuton-familiar-typed-surface-world-and-memory/M-my-familiar-research-modality/M-research-mathematician-mode/M-riemann-xi-function-research.md`
- `algorithms/create-mission.md`
- `docs/mission-creation-checklist.md`
