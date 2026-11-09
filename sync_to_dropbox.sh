#!/usr/bin/env bash

set -euo pipefail

repo_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_name="$(basename "$repo_dir")"
dropbox_path="dropbox:${repo_name}"
common_args=(--filter-from "$repo_dir/config/dropbox_filter.txt" --ignore-case --copy-links -P)

case "${1:-}" in
  "") ;;
  --dry-run) common_args+=(--dry-run) ;;
  --help|-h)
    echo "Usage: $0 [--dry-run]"
    echo "Sync the filtered project backup to $dropbox_path."
    exit 0 ;;
  *) echo "Usage: $0 [--dry-run]" >&2; exit 2 ;;
esac
if (( $# > 1 )); then
  echo "Usage: $0 [--dry-run]" >&2
  exit 2
fi

echo "Syncing filtered backup to $dropbox_path${1:+ ($1)}"
# sync creates a missing destination. Excluded remote files are preserved:
# deliberately do not use --delete-excluded.
exec rclone sync "$repo_dir" "$dropbox_path" "${common_args[@]}"
