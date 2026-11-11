# Threads 1 — bounded JAX actors and stage-2 move split

Date: 2026-11-11.

## Source of the 2,048-task limit

Durable `bg.py` jobs are children of `futon3c-zone.service`, whose cgroup is

```
/user.slice/user-1000.slice/user@1000.service/futon.slice/
  futon-services.slice/futon3c-zone.service
```

`systemctl --user show futon3c-zone.service` reports `TasksMax=2048`; the
matching cgroup `pids.max` is also 2048.  This is the systemd service's task
limit, not a limit imposed by `bg.py` or the Euclid runner.  No systemd,
futon3c, or cgroup setting was changed.

Before this change, a JAX actor initialized with affinity to CPUs 20–31 held
67 tasks.  Six surviving Probe 13 actors plus the learner brought the scope to
1,947/2,048; other actors failed in `pthread_create`.  Fifteen Probe 13 actors
had also survived their reported parent exit after being reparented to the
service.  This packet identified them by the exact 18:56 and 19:13 process
cohorts from its own earlier runs, terminated only those owned PIDs, and
verified their disappearance.  That corrected the background scope baseline
from 1,825 tasks to about 824.

## Process-local cap

The runner's new `--thread-cpus` option creates each JAX CPU client while its
process has one-CPU affinity.  That fixes the JAX pool at the small size, after
which the process regains the full named CPU pool for compilation and play.
Actors are striped across the named CPUs during initialization.  The learner
uses the same create-small-then-widen sequence.  BLAS, OpenMP, TensorFlow, and
XLA environment caps remain set to one where the curriculum driver launches
the process.

| process | before | after |
|---|---:|---:|
| one actor | 67 tasks | 8 tasks |
| learner | approximately 67 tasks | 11 tasks |
| 12 actors + learner (steady process sum) | approximately 871 tasks | 107 tasks |

The acceptance run used all twelve actors, 32 simulations, root-only mate
checking, the rc13 stage-2 checkpoint and clean stage-2 prefix.  It used
one-ply diagnostic episodes so the five-minute liveness check exercised
repeated inference, MCTS, learner checkpoints, and carried-weight updates
without being confounded by a separate legal-action defect: in a full-cap
diagnostic, action enumeration offered a `superposition` action which the
transactional contradiction guard then refused during MCTS application.

After roughly 30 seconds of model/process startup, all twelve actors ran
together for five minutes.  The whole `futon3c-zone` scope peaked at
**1,206/2,048 tasks** (58.9%), including unrelated services.  No actor logged
an exception, and every actor completed games:

| actor | games | actor | games |
|---:|---:|---:|---:|
| 0 | 89 | 6 | 91 |
| 1 | 92 | 7 | 93 |
| 2 | 91 | 8 | 92 |
| 3 | 90 | 9 | 89 |
| 4 | 91 | 10 | 91 |
| 5 | 91 | 11 | 91 |

All owned diagnostic and profiling workers were absent at completion.

## One clean stage-2 move

`scripts/euclid_threads_profile.py` restores rc13 stage-2 checkpoint 1 and
profiles one 32-simulation root-mode move from the clean 532-action prefix.
It times mutually exclusive phases directly; legal enumeration nested inside
OpenSpiel's application callback is assigned to enumeration, not counted a
second time as application.  `PYTHONHASHSEED=0` and the production thread
settings were used.

| phase | calls | seconds | share |
|---|---:|---:|---:|
| root win-in-one scan | 1 (532 successors) | 2.910 | 14.1% |
| new-state legal enumeration | 84 | 10.494 | 50.9% |
| network inference | 24 | 3.878 | 18.8% |
| state cloning/application | 32 clones, 104 applies | 0.739 | 3.6% |
| other MCTS work | — | 2.588 | 12.6% |
| **total** | — | **20.610** | **100%** |

Mean legal enumeration time was **124.9 ms**.  The total is close to Probe
13's uninstrumented 22.865-second measurement; ordinary run-to-run variation
accounts for the difference.
