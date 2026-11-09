#!/usr/bin/env bash

set -euo pipefail

repo_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
if (( $# )); then
  echo "Usage: $0" >&2
  exit 2
fi
exec bash "$repo_dir/sync_to_dropbox.sh" --dry-run
