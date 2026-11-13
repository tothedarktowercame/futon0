#!/usr/bin/env bash
set -euo pipefail

ROOT_DEFAULT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
ROOT="${FUTON_STACK_ROOT:-$ROOT_DEFAULT}"

DEFAULT_REPOS=(
  futon0
  futon1
  futon1a
  futon2
  futon3
  futon3a
  futon3b
  futon3c
  futon4
  futon5
  futon5a
  futon6
  futon7
)

usage() {
  cat <<USAGE
Usage: $(basename "$0") <command>

Commands:
  audit      Print git hygiene report across futon repos.
  enforce    Set conservative per-repo defaults:
             - pull.ff=only
             - fetch.prune=true
             - pull.rebase=false
             - rebase.autoStash=false
  safe-pull  Fetch + fast-forward pull only when safe.
             Skips dirty/ahead/diverged repos with explicit reasons.

Environment:
  FUTON_STACK_ROOT   Root containing futon repos (default: ${ROOT_DEFAULT})
  FUTON_STACK_REPOS  Space-separated repo names override default set.

Examples:
  $(basename "$0") audit
  $(basename "$0") enforce
  $(basename "$0") safe-pull
  FUTON_STACK_REPOS="futon3c futon6" $(basename "$0") safe-pull
USAGE
}

repo_list() {
  local repos=()
  local name

  if [[ -n "${FUTON_STACK_REPOS:-}" ]]; then
    # shellcheck disable=SC2206
    repos=(${FUTON_STACK_REPOS})
  else
    repos=("${DEFAULT_REPOS[@]}")
  fi

  for name in "${repos[@]}"; do
    if [[ -d "${ROOT}/${name}/.git" ]]; then
      printf '%s\n' "${name}"
    fi
  done
}

branch_of() {
  local repo="$1"
  git -C "${ROOT}/${repo}" rev-parse --abbrev-ref HEAD 2>/dev/null || echo "?"
}

upstream_of() {
  local repo="$1"
  git -C "${ROOT}/${repo}" rev-parse --abbrev-ref --symbolic-full-name '@{u}' 2>/dev/null || true
}

ahead_behind() {
  local repo="$1"
  local upstream="$2"
  if [[ -z "$upstream" ]]; then
    echo "0 0"
    return 0
  fi
  git -C "${ROOT}/${repo}" rev-list --left-right --count "${upstream}...HEAD" 2>/dev/null || echo "0 0"
}

status_counts() {
  local repo="$1"
  local status
  status="$(git -C "${ROOT}/${repo}" status --porcelain=1)"
  if [[ -z "$status" ]]; then
    echo "0 0"
    return 0
  fi

  local tracked untracked
  tracked="$(awk 'BEGIN{n=0} $0 !~ /^\?\?/ {n++} END{print n}' <<<"$status")"
  untracked="$(awk 'BEGIN{n=0} $0 ~ /^\?\?/ {n++} END{print n}' <<<"$status")"
  echo "${tracked} ${untracked}"
}

run_audit() {
  local repos
  mapfile -t repos < <(repo_list)

  if [[ ${#repos[@]} -eq 0 ]]; then
    echo "No futon repos found under ${ROOT}" >&2
    exit 1
  fi

  printf '%-9s %-12s %-24s %6s %6s %8s %10s %8s\n' \
    "repo" "branch" "upstream" "ahead" "behind" "tracked" "untracked" "state"
  printf '%-9s %-12s %-24s %6s %6s %8s %10s %8s\n' \
    "--------" "-----------" "-----------------------" "-----" "------" "-------" "---------" "-------"

  local repo branch upstream behind ahead tracked untracked state
  for repo in "${repos[@]}"; do
    branch="$(branch_of "$repo")"
    upstream="$(upstream_of "$repo")"
    read -r behind ahead < <(ahead_behind "$repo" "$upstream")
    read -r tracked untracked < <(status_counts "$repo")

    state="clean"
    if (( tracked > 0 || untracked > 0 )); then
      state="dirty"
    fi

    printf '%-9s %-12s %-24s %6d %6d %8d %10d %8s\n' \
      "$repo" "$branch" "${upstream:--}" "$ahead" "$behind" "$tracked" "$untracked" "$state"
  done
}

run_enforce() {
  local repos
  mapfile -t repos < <(repo_list)

  if [[ ${#repos[@]} -eq 0 ]]; then
    echo "No futon repos found under ${ROOT}" >&2
    exit 1
  fi

  local repo
  for repo in "${repos[@]}"; do
    git -C "${ROOT}/${repo}" config pull.ff only
    git -C "${ROOT}/${repo}" config fetch.prune true
    git -C "${ROOT}/${repo}" config pull.rebase false
    git -C "${ROOT}/${repo}" config rebase.autoStash false
    echo "[enforce] ${repo}: pull.ff=only fetch.prune=true pull.rebase=false rebase.autoStash=false"
  done
}

run_safe_pull() {
  local repos
  mapfile -t repos < <(repo_list)

  if [[ ${#repos[@]} -eq 0 ]]; then
    echo "No futon repos found under ${ROOT}" >&2
    exit 1
  fi

  local repo branch upstream behind ahead tracked untracked
  local pulled=0 uptodate=0 dirty=0 ahead_only=0 diverged=0 no_upstream=0 failed=0

  for repo in "${repos[@]}"; do
    echo "[safe-pull] ${repo}"

    git -C "${ROOT}/${repo}" fetch --prune --tags >/dev/null

    upstream="$(upstream_of "$repo")"
    if [[ -z "$upstream" ]]; then
      echo "  skip: no upstream configured"
      ((no_upstream+=1))
      continue
    fi

    read -r tracked untracked < <(status_counts "$repo")
    if (( tracked > 0 || untracked > 0 )); then
      echo "  skip: dirty working tree (tracked=${tracked}, untracked=${untracked})"
      ((dirty+=1))
      continue
    fi

    read -r behind ahead < <(ahead_behind "$repo" "$upstream")

    if (( ahead > 0 && behind > 0 )); then
      echo "  skip: diverged (ahead=${ahead}, behind=${behind})"
      ((diverged+=1))
      continue
    fi

    if (( ahead > 0 )); then
      echo "  skip: local commits ahead of upstream (ahead=${ahead})"
      ((ahead_only+=1))
      continue
    fi

    if (( behind == 0 )); then
      echo "  ok: already up to date"
      ((uptodate+=1))
      continue
    fi

    if git -C "${ROOT}/${repo}" pull --ff-only >/dev/null; then
      echo "  pulled: fast-forwarded ${behind} commit(s)"
      ((pulled+=1))
    else
      echo "  fail: pull --ff-only failed"
      ((failed+=1))
    fi
  done

  echo
  echo "Summary: pulled=${pulled} up-to-date=${uptodate} dirty=${dirty} ahead=${ahead_only} diverged=${diverged} no-upstream=${no_upstream} failed=${failed}"

  if (( failed > 0 )); then
    exit 1
  fi
}

main() {
  local cmd="${1:-audit}"
  case "$cmd" in
    audit) run_audit ;;
    enforce) run_enforce ;;
    safe-pull) run_safe_pull ;;
    -h|--help|help) usage ;;
    *)
      echo "Unknown command: $cmd" >&2
      echo >&2
      usage >&2
      exit 2
      ;;
  esac
}

main "$@"
