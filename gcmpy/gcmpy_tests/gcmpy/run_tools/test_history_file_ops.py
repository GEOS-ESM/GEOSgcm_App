from pathlib import Path

from gcmpy.run_tools.history_file_ops import strip_gcmrun_cnfrootc_lines

HISTORY_RC = """\
COLLECTIONS: 'inst1_2d_asm_Nx'
             ::
inst1_2d_asm_Nx.template: '%y4%m2%d2_%h2%n2z.nc4',
inst1_2d_asm_Nx.fields: 'PS' , 'DYN' ,
             ::
gcmrun_stub: 'CNFROOTC'
bare_line: CNFROOTC
"""


def _write(tmp_path: Path, lsm_choice: str | None):
    history = tmp_path / "HISTORY.rc"
    agcm = tmp_path / "AGCM.rc"
    history.write_text(HISTORY_RC)
    agcm_text = ""
    if lsm_choice is not None:
        agcm_text = f"LSM_CHOICE: {lsm_choice}\n"
    agcm.write_text(agcm_text)
    return history, agcm


def test_strip_removes_quoted_lines_when_lsm_choice_2(tmp_path):
    history, agcm = _write(tmp_path, "2")

    changed = strip_gcmrun_cnfrootc_lines(history, agcm)

    assert changed is True
    text = history.read_text()
    # gcm_run.j's `grep -v "'CNFROOTC'"` includes the single quotes...
    assert "'CNFROOTC'" not in text
    # ...so an unquoted CNFROOTC line must survive...
    assert "bare_line: CNFROOTC" in text
    # ...and unrelated lines stay intact.
    assert "COLLECTIONS: 'inst1_2d_asm_Nx'" in text
    assert "inst1_2d_asm_Nx.fields" in text


def test_noop_when_lsm_choice_is_not_2(tmp_path):
    history, agcm = _write(tmp_path, "1")

    changed = strip_gcmrun_cnfrootc_lines(history, agcm)

    assert changed is False
    assert history.read_text() == HISTORY_RC


def test_noop_when_lsm_choice_missing(tmp_path):
    history, agcm = _write(tmp_path, None)

    changed = strip_gcmrun_cnfrootc_lines(history, agcm)

    assert changed is False
    assert history.read_text() == HISTORY_RC


def test_noop_when_no_cnfrootc_lines(tmp_path):
    history, agcm = _write(tmp_path, "2")
    history.write_text("COLLECTIONS: 'inst1_2d_asm_Nx'\n             ::\n")

    changed = strip_gcmrun_cnfrootc_lines(history, agcm)

    assert changed is False
    assert "'CNFROOTC'" not in history.read_text()


def test_strip_preserves_trailing_newline_shape(tmp_path):
    history, agcm = _write(tmp_path, "2")
    history.write_text("a: 1\ngcmrun_stub: 'CNFROOTC'\n")  # no trailing blank

    strip_gcmrun_cnfrootc_lines(history, agcm)

    assert history.read_text() == "a: 1\n"
