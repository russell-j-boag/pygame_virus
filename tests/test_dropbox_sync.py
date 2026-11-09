"""Exercise the backup wrappers using rclone's local alias backend, never Dropbox."""
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[1]


@unittest.skipUnless(shutil.which("rclone"), "rclone is required")
class DropboxSyncTests(unittest.TestCase):
    def test_filtered_backup_and_dry_runs(self):
        with tempfile.TemporaryDirectory(prefix="dropbox-filter-") as temporary:
            base = Path(temporary)
            source = base / "project with spaces"
            source.mkdir()
            remote = base / "local remote"
            remote.mkdir()
            destination = remote / source.name
            config = base / "rclone.conf"
            config.write_text(f"[dropbox]\ntype = alias\nremote = {remote}\n")
            env = dict(os.environ, RCLONE_CONFIG=str(config))

            kept = {
                "data/raw.CSV", "data/archive/old.csv", "analysis.R",
                "analysis_outputs/validation/build_document.py",
                "analysis_outputs/validation/table.csv",
                "plots/archive/nested/results.csv",
                "plots/semester2_2026_behavioural/archive/old/fit.R",
                "plots/semester2_2026_behavioural/.presentation_build_123/table.csv",
                "plots/semester2_2026_behavioural/presentation/current.PNG",
                "plots/semester2_2026_behavioural/overview.pdf",
                "plots/semester2_2026_data/individual/p001/current.jpg",
                "analysis_outputs/semester2_2026_qc/individual/current.pdf",
                "analysis_outputs/semester2_2026_behavioural/primary/diagnostic.svg",
                "manuscript/archive/manuscript.pdf", "python/assets/stimulus.png",
                "source_archives/semester2_2026/frozen.zip",
                "analysis_outputs/validation/manifest.json",
            }
            excluded = {
                "model.rds", "data/model.rDs", "linear_models/model.RData",
                "analysis_outputs/semester2_2026_behavioural/primary/bundle.rda",
                "analysis_outputs/semester2_2026_behavioural/primary/contrast_audit.rds",
                "plots/semester2_2026_behavioural/archive/old/figure.pdf",
                "plots/semester2_2026_behavioural/nested/archive/old/figure.PNG",
                "plots/semester2_2026_behavioural/.presentation_build_123/figure.png",
                "analysis_outputs/.semester2_2026_qc.build-123/figure.png",
                "analysis_outputs/semester2_2026_qc/nested/.build-123/figure.webp",
                "plots/semester2_2026_data.previous_123/figure.png",
                "analysis_outputs/semester2_2026_behavioural.previous_123/figure.pdf",
                "analysis_outputs/semester2_2026_behavioural/nested.previous_123/figure.svg",
                "plots/archive/old/figure.png", "plots/pilot.png",
                "analysis_outputs/validation/figure.pdf",
                "analysis_outputs/figure_exports/figure.tiff", "tmp/figure.gif",
                ".git/objects/object", ".Rproj.user/state", "tests/__pycache__/a.pyc",
                ".DS_Store", "manuscript/~$manuscript.docx", "Rplots.pdf",
            }
            for name in kept | excluded:
                path = source / name
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_text(name + "\n")
            for name in ("sync_to_dropbox.sh", "sync_to_dropbox_dry_run.sh", "config/dropbox_filter.txt"):
                (source / name).parent.mkdir(parents=True, exist_ok=True)
                shutil.copy2(ROOT / name, source / name)
                kept.add(name)
            linked = source / "analysis_outputs/validation/linked.csv"
            linked.symlink_to(source / "data/raw.CSV")
            kept.add(str(linked.relative_to(source)))

            def run(*args, check=True):
                return subprocess.run(args, cwd=base, env=env, capture_output=True,
                                      text=True, check=check, timeout=60)

            for command in (("bash", str(source / "sync_to_dropbox_dry_run.sh")),
                            ("bash", str(source / "sync_to_dropbox.sh"), "--dry-run")):
                run(*command)
                self.assertFalse(destination.exists(), "Dry-run created a destination")

            # Included stale files follow normal sync semantics; excluded files survive.
            destination.mkdir()
            (destination / "old_model.rds").write_text("preserve remote fit")
            (destination / "stale.csv").write_text("remove by normal sync")
            run("bash", str(source / "sync_to_dropbox_dry_run.sh"))
            self.assertEqual({p.name for p in destination.iterdir()}, {"old_model.rds", "stale.csv"})
            self.assertNotEqual(run("bash", str(source / "sync_to_dropbox.sh"),
                                    "--delete-excluded", check=False).returncode, 0)
            run("bash", str(source / "sync_to_dropbox.sh"))
            actual = {str(p.relative_to(destination)) for p in destination.rglob("*") if p.is_file()}
            self.assertEqual(actual, kept | {"old_model.rds"})
            self.assertEqual((destination / "old_model.rds").read_text(), "preserve remote fit")
            for name in kept:
                self.assertEqual((source / name).read_bytes(), (destination / name).read_bytes(), name)
            self.assertFalse((destination / linked.relative_to(source)).is_symlink())


if __name__ == "__main__":
    unittest.main()
