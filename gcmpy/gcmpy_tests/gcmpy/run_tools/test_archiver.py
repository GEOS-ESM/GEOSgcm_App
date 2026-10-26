import shutil
import tarfile
from pathlib import Path

import pytest

from gcmpy.run_tools.archiver import (
    _checkpoint_extension,
    archive_checkpoints,
    rename_final_checkpoints,
)

EXPID = "myexp"
GCMVER = "Jcorrect-v12.3"
BCTAG = "NL3_Reynolds"
BCRSLV = "C90_DE0360xPE0180"
SUFFIX = f"e20000301_21z.{GCMVER}.{BCTAG}_{BCRSLV}"

needs_file = pytest.mark.skipif(
    shutil.which("file") is None, reason="file(1) not available"
)


def _write_binary(path: Path):
    # Ascending bytes can be recognized as a Targa image by file(1).
    path.write_bytes(b"\x00\xff" * 1024)


def _write_netcdf(path: Path):
    import netCDF4

    with netCDF4.Dataset(path, "w") as dataset:
        dataset.createDimension("x", 4)
        dataset.createVariable("v", "f4", ("x",))


@needs_file
def test_checkpoint_extension_binary(tmp_path):
    binary = tmp_path / "fvcore_checkpoint"
    _write_binary(binary)

    assert _checkpoint_extension(binary) == "bin"


@needs_file
def test_checkpoint_extension_netcdf(tmp_path):
    nc = tmp_path / "moist_checkpoint"
    _write_netcdf(nc)

    assert _checkpoint_extension(nc) == "nc4"


@needs_file
def test_checkpoint_extension_text_is_nc4(tmp_path):
    # Mirrors the original: anything that is not application/octet-stream
    # (including text) gets the .nc4 extension.
    text = tmp_path / "text_checkpoint"
    text.write_text("plain text\n")

    assert _checkpoint_extension(text) == "nc4"


@needs_file
def test_rename_final_checkpoints_uses_sniffed_extensions(tmp_path):
    _write_binary(tmp_path / "fvcore_checkpoint")
    _write_netcdf(tmp_path / "moist_checkpoint")

    renamed = rename_final_checkpoints(
        tmp_path, EXPID, "e20000301_21z", GCMVER, BCTAG, BCRSLV
    )

    names = sorted(p.name for p in renamed)
    assert names == sorted(
        [
            f"{EXPID}.fvcore_rst.{SUFFIX}.bin",
            f"{EXPID}.moist_rst.{SUFFIX}.nc4",
        ]
    )
    assert not list(tmp_path.glob("*_checkpoint"))


def _segment_fixture(exp_dir: Path):
    scratch = exp_dir / "scratch"
    scratch.mkdir(parents=True)
    (scratch / "cap_restart").write_text("20000301 210000")
    _write_binary(scratch / "fvcore_checkpoint")
    _write_netcdf(scratch / "moist_checkpoint")
    (scratch / "fvcore_checkpoint.0001").write_text("intermediate")
    (scratch / "fvcore_internal_rst").write_text("initial restart")
    return scratch


@needs_file
def test_archive_checkpoints_full_segment(tmp_path):
    exp_dir = tmp_path / "exp"
    scratch = _segment_fixture(exp_dir)

    archive_checkpoints(
        scratch,
        exp_dir,
        scratch / "cap_restart",
        EXPID,
        GCMVER,
        BCTAG,
        BCRSLV,
        fsegment="00000000",
        coupled=False,
        wavewatch=False,
    )

    restarts = exp_dir / "restarts"
    # Intermediate checkpoint moved straight to restarts/.
    assert (restarts / "fvcore_checkpoint.0001").exists()

    # Full segment -> tarball with the stamped names, originals removed.
    tar_path = restarts / f"restarts.e20000301_21z.tar"
    assert tar_path.exists()
    with tarfile.open(tar_path) as tar:
        members = set(tar.getnames())
    assert members == {
        f"{EXPID}.fvcore_rst.{SUFFIX}.bin",
        f"{EXPID}.moist_rst.{SUFFIX}.nc4",
    }
    assert not list(restarts.glob(f"{EXPID}.*.{SUFFIX}.*"))

    # Scratch holds clean restarts for the next segment.
    assert sorted(p.name for p in scratch.glob("*_rst")) == [
        "fvcore_rst",
        "moist_rst",
    ]


@needs_file
def test_archive_checkpoints_forecast_segment_copies_stamps(tmp_path):
    exp_dir = tmp_path / "exp"
    scratch = _segment_fixture(exp_dir)

    archive_checkpoints(
        scratch,
        exp_dir,
        scratch / "cap_restart",
        EXPID,
        GCMVER,
        BCTAG,
        BCRSLV,
        fsegment="00000001",
        coupled=False,
        wavewatch=False,
    )

    restarts = exp_dir / "restarts"
    # Forecast segments skip the tar but still leave the stamped copies
    # in restarts/ (gcm_run.j:1266-1272 runs unconditionally).
    assert not list(restarts.glob("*.tar"))
    assert (restarts / f"{EXPID}.fvcore_rst.{SUFFIX}.bin").exists()
    assert (restarts / f"{EXPID}.moist_rst.{SUFFIX}.nc4").exists()

    assert sorted(p.name for p in scratch.glob("*_rst")) == [
        "fvcore_rst",
        "moist_rst",
    ]


@needs_file
def test_archive_wavewatch_keeps_clean_name_in_scratch(tmp_path):
    exp_dir = tmp_path / "exp"
    scratch = _segment_fixture(exp_dir)
    (scratch / "restart001.ww3").write_text("old")
    (scratch / "restart002.ww3").write_text("newest")

    archive_checkpoints(
        scratch,
        exp_dir,
        scratch / "cap_restart",
        EXPID,
        GCMVER,
        BCTAG,
        BCRSLV,
        fsegment="00000001",
        coupled=False,
        wavewatch=True,
    )

    # Scratch keeps the canonical name (what the next segment and
    # copy_restarts_to_exp_dir() expect), intermediates are gone.
    assert (scratch / "restart.ww3").read_text() == "newest"
    assert not list(scratch.glob("restart[0-9][0-9][0-9].ww3"))

    # The stamped archive copy lives in restarts/.
    assert (
        exp_dir / "restarts" / f"{EXPID}.restart.ww3.{SUFFIX}.bin"
    ).exists()


@needs_file
def test_archive_coupled_copies_restart_dir_and_inputs(tmp_path):
    exp_dir = tmp_path / "exp"
    scratch = _segment_fixture(exp_dir)
    (scratch / "INPUT").mkdir()
    (scratch / "RESTART").mkdir()
    (scratch / "RESTART" / "ocean_internal_rst").write_text("ocean")

    archive_checkpoints(
        scratch,
        exp_dir,
        scratch / "cap_restart",
        EXPID,
        GCMVER,
        BCTAG,
        BCRSLV,
        fsegment="00000001",
        coupled=True,
        wavewatch=False,
    )

    # cp -r RESTART -> restarts/RESTART.<edate> and cp RESTART/* INPUT
    assert (
        exp_dir / "restarts" / "RESTART.e20000301_21z" / "ocean_internal_rst"
    ).exists()
    assert (scratch / "INPUT" / "ocean_internal_rst").exists()
