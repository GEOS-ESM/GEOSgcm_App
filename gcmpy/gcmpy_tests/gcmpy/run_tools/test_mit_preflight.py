"""
MIT/CICE4 run-time input preflight (csh PR #931: check_mit_inputs).
Builds a fake scratch directory and install tree, no model needed.
"""

import pytest

from gcmpy.run_tools.mit_preflight import MITInputError, check_mit_inputs

REMAP = 'DC0360xPC0181_LL5400x15-LL.bin'
RESOURCES = {'AGCM_IM': 90, 'AGCM_JM': 540, 'OGCM_IM': 5400, 'OGCM_JM': 15}


def _setup(tmp_path, *, remap=True, lib=True, staged=True, tile_ogcm_jm=15):
    scratch = tmp_path / 'scratch'
    scratch.mkdir()
    for name in ['runoff.bin', 'mit.ascii', 'SEAWIFS_KPAR_mon_clim.data',
                 'grid_cice.bin', 'kmt_cice.bin']:
        (scratch / name).write_text('x')
    (scratch / 'INPUT').mkdir()
    if remap:
        (scratch / REMAP).write_text('x')
    (scratch / 'tile.data').write_text(
        'tile file\nheader\n'
        'PE90x540-CF  gridname\n90  im\n540  jm\n'
        f'LL5400x15-LL  ogrid\n5400  oim\n{tile_ogcm_jm}  ojm\n')

    install = tmp_path / 'install'
    geosbin = install / 'bin'
    geosbin.mkdir(parents=True)
    if lib:
        (install / 'lib').mkdir()
        (install / 'lib' / 'libCICE4.so').write_text('x')

    exp = tmp_path / 'exp'
    if staged:
        (exp / 'mit_input').mkdir(parents=True)
        (exp / 'mit_input' / 'data').write_text('x')
        (exp / 'mit_input' / 'data.exch2').write_text('x')
    else:
        exp.mkdir(exist_ok=True)
    return scratch, exp, geosbin


def test_complete_inputs_pass(tmp_path):
    scratch, exp, geosbin = _setup(tmp_path)
    check_mit_inputs(scratch, exp, geosbin, REMAP, True, RESOURCES)


def test_remap_is_required_only_for_data_atmosphere(tmp_path):
    scratch, exp, geosbin = _setup(tmp_path, remap=False)
    check_mit_inputs(scratch, exp, geosbin, REMAP, False, RESOURCES)
    with pytest.raises(MITInputError, match=REMAP):
        check_mit_inputs(scratch, exp, geosbin, REMAP, True, RESOURCES)


def test_missing_scratch_link_stops_the_run(tmp_path):
    scratch, exp, geosbin = _setup(tmp_path)
    (scratch / 'kmt_cice.bin').unlink()
    with pytest.raises(MITInputError, match='kmt_cice.bin'):
        check_mit_inputs(scratch, exp, geosbin, REMAP, True, RESOURCES)


def test_missing_cice4_library_stops_the_run(tmp_path):
    scratch, exp, geosbin = _setup(tmp_path, lib=False)
    with pytest.raises(MITInputError, match='libCICE4'):
        check_mit_inputs(scratch, exp, geosbin, REMAP, True, RESOURCES)


def test_missing_staged_mit_files_stop_the_run(tmp_path):
    scratch, exp, geosbin = _setup(tmp_path, staged=False)
    with pytest.raises(MITInputError, match='mit_input'):
        check_mit_inputs(scratch, exp, geosbin, REMAP, True, RESOURCES)


def test_tile_must_match_the_ocean_grid(tmp_path):
    scratch, exp, geosbin = _setup(tmp_path, tile_ogcm_jm=30)
    with pytest.raises(MITInputError, match='do not match'):
        check_mit_inputs(scratch, exp, geosbin, REMAP, True, RESOURCES)
