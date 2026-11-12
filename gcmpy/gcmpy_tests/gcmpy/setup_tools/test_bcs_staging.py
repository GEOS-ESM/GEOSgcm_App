"""
User-supplied ``cap_restart`` preservation and site-aware
``sst_dir``/``chem_dir`` propagation into the stagebcs/linkbcs pipeline.

Users supply ``cap_restart`` with their restarts; setup must not invent a
restart date. ``stagebcs.py`` composed SST/ExtData roots from ``boundary_dir`` plus
catalog relative names, which misses NCCS's ``share/gmao_ops`` prefix.
These tests pin the fixed behavior, including the o1 (Reynolds) data-ocean
``bcrslv`` that matches the on-disk land-tile directories.
"""

import importlib.util
from pathlib import Path

import pytest
import yaml

import gcmpy.setup_tools.setup_main as setup_main
from gcmpy.setup_tools.setup_main import SetupGCM

REPO_ROOT = Path(__file__).resolve().parents[4]
STAGEBCS = REPO_ROOT / "stagebcs.py"


def _load_stagebcs():
    spec = importlib.util.spec_from_file_location("stagebcs_under_test", STAGEBCS)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def _bare_setup(tmp_path):
    inst = SetupGCM.__new__(SetupGCM)
    inst.expConfig = {'exp_dir': str(tmp_path)}
    inst.ocean = {'history_template': 'HISTORY.AGCM.rc.tmpl'}
    for name in [
        'gcm_archive.j', 'gcm_forecast.setup', 'gcm_forecast.tmpl',
        'gcm_moveplot.j', 'gcm_plot.tmpl', 'gcm_quickplot.csh', 'plot.rc',
        'gcm_post.j', 'post.rc', 'gcm_regress.j',
        'CAP.rc.tmpl', 'AGCM.rc.tmpl', 'HISTORY.AGCM.rc.tmpl',
        'fvcore_layout.rc', 'logging.yaml',
    ]:
        (tmp_path / name).write_text('x')
    return inst


def test_organize_exp_dir_does_not_create_cap_restart(tmp_path, monkeypatch):
    inst = _bare_setup(tmp_path)
    monkeypatch.setattr(setup_main, 'copy_src_tarfile', lambda *a, **k: None)
    monkeypatch.setattr(inst, 'make_RESTART_dir', lambda: None)

    inst.organize_exp_dir()

    assert not (tmp_path / "cap_restart").exists()


def test_organize_exp_dir_preserves_existing_cap_restart(tmp_path, monkeypatch):
    (tmp_path / "cap_restart").write_text('20000414 210000\n')
    inst = _bare_setup(tmp_path)
    monkeypatch.setattr(setup_main, 'copy_src_tarfile', lambda *a, **k: None)
    monkeypatch.setattr(inst, 'make_RESTART_dir', lambda: None)

    inst.organize_exp_dir()

    assert (tmp_path / "cap_restart").read_text() == '20000414 210000\n'


def test_stage_bcs_config_includes_sst_and_chem_dirs(tmp_path, monkeypatch):
    inst = SetupGCM.__new__(SetupGCM)
    inst.expConfig = {
        'exp_dir': str(tmp_path),
        'DATAOCEAN_resolution': 'o1',
    }
    inst.boundary_dir = '/discover/nobackup/projects/gmao'
    inst.atmos = {'im': 12}
    inst.land = {'bcs': 'NL3', 'model': 'Catchment'}
    inst.ocean = {'seaice_model': None, 'running_ocean': False}
    inst.sst_dir = '/discover/nobackup/x/share/gmao_ops/fvInput/g5gcm/bcs/realtime/SST/360x180'
    inst.chem_dir = '/discover/nobackup/x/share/gmao_ops/fvInput_nc3'

    monkeypatch.setitem(setup_main.pathdict, 'install', '/fake/install')
    monkeypatch.setitem(setup_main.pathdict, 'bin', '/fake/bin')
    monkeypatch.setattr(setup_main.subprocess, 'run', lambda *a, **k: None)

    inst.stage_bcs()

    with open(tmp_path / "linkbcs_config.yaml") as f:
        config = yaml.safe_load(f)

    assert config['sst_dir'] == inst.sst_dir
    assert config['chem_dir'] == inst.chem_dir
    assert config['ogcm_grid'] == 'reynolds'
    assert config['ocean_model'] == 'data'


def test_stagebcs_prefers_config_sst_and_chem_dirs(tmp_path):
    stagebcs = _load_stagebcs()
    config_path = _write_stagebcs_fixture(
        tmp_path,
        overrides={
            'sst_dir': '/custom/sst',
            'chem_dir': '/custom/chem',
        },
    )

    creator = stagebcs.SymlinkCreator(stagebcs.CatalogManager(config_path))

    assert creator.sst_dir == Path('/custom/sst')
    assert creator.chem_dir == Path('/custom/chem')
    paths = creator.dataocean_paths()
    assert paths['sst.data'] == Path(
        '/custom/sst/dataoceanfile_MERRA_sst_1971-current.360x180.LE'
    )
    assert creator.bcrslv == 'CF0012x6C_DE0360xPE0180'


def test_stagebcs_falls_back_to_catalog_composition(tmp_path):
    stagebcs = _load_stagebcs()
    config_path = _write_stagebcs_fixture(tmp_path)

    creator = stagebcs.SymlinkCreator(stagebcs.CatalogManager(config_path))

    boundary = Path(config_path).parent / "boundary"
    assert creator.chem_dir == boundary / "fvInput_nc3"
    assert creator.sst_dir == boundary / "fvInput/g5gcm/bcs/realtime/SST/360x180"


def _write_stagebcs_fixture(tmp_path, overrides=None):
    install = tmp_path / "install"
    (install / "etc").mkdir(parents=True)
    catalog_src = REPO_ROOT / "bcs_catalog.yaml"
    (install / "etc" / "bcs_catalog.yaml").write_text(catalog_src.read_text())
    (install / "etc" / "SITE.rc").write_text("SITE: NCCS\n")

    config = {
        'install_dir': str(install),
        'boundary_dir': str(tmp_path / "boundary"),
        'agcm_grid': 'c12',
        'land_version': 'nl3',
        'seaice_model': 'cice',
        'catchcn': False,
        'precip_correction': False,
        'dual_ocean': False,
        'experiment_type': 'GCM',
        'pchem_species': 'merra-2',
        'ocean_model': 'data',
        'ogcm_grid': 'reynolds',
    }
    if overrides:
        config.update(overrides)

    config_path = tmp_path / "linkbcs_config.yaml"
    config_path.write_text(yaml.safe_dump(config))
    return config_path


# --- MIT (csh gcm_setup PR #931) -------------------------------------------

def _mit_stagebcs_config(tmp_path, grid='c90', llc='llc90', **mit_overrides):
    mit_cfg = {
        'profile': 'standard',
        'coupled_dir': str(tmp_path / 'ocean/MIT'),
        'bcs_dir': None,
        'land_bcrslv': None,
        'tile': None,
        'runoff': None,
        'gridspec': None,
        'kpar': '/ocean/MIT/5400x15/SEAWIFS_KPAR_mon_clim.5400x15',
        'input_dir': '/ocean/MIT/5400x15/INPUT',
        'cice_grid': '/ocean/MIT/5400x15/cice/grid_cice.bin',
        'cice_kmt': '/ocean/MIT/5400x15/cice/kmt_cice.bin',
        'remap_name': 'DC0360xPC0181_LL5400x15-LL.bin',
        'remap': '/ocean/MIT/DC0360xPC0181_LL5400x15-LL.bin',
        'expect': {'agcm_gridname': 'PE90x540-CF', 'agcm_im': 90, 'agcm_jm': 540,
                   'ogcm_im': 5400, 'ogcm_jm': 15},
    }
    mit_cfg.update(mit_overrides)
    return _write_stagebcs_fixture(tmp_path, overrides={
        'agcm_grid': grid,
        'ocean_model': 'mit',
        'ocean_resolution': llc,
        'ogcm_grid': 'coupled',
        'seaice_model': 'cice',
        'mit': mit_cfg,
    })


def test_stagebcs_mit_grid_and_bcrslv(tmp_path):
    stagebcs = _load_stagebcs()
    creator = stagebcs.SymlinkCreator(stagebcs.CatalogManager(_mit_stagebcs_config(tmp_path)))

    assert creator.coupled is True
    assert (creator.ogcm_IM, creator.ogcm_JM) == (5400, 15)
    assert creator.bcrslv == 'CF0090x6C_MITLLC5400x0015'


def test_stagebcs_mit_links_use_explicit_inputs(tmp_path):
    stagebcs = _load_stagebcs()
    creator = stagebcs.SymlinkCreator(stagebcs.CatalogManager(_mit_stagebcs_config(tmp_path)))
    links = creator.compile_links()

    assert links['mit.ascii'] == creator.bcs_dir / f"geometry/{creator.bcrslv}/mit.ascii"
    assert links['DC0360xPC0181_LL5400x15-LL.bin'] == Path('/ocean/MIT/DC0360xPC0181_LL5400x15-LL.bin')
    assert links['SEAWIFS_KPAR_mon_clim.data'] == Path('/ocean/MIT/5400x15/SEAWIFS_KPAR_mon_clim.5400x15')
    assert links['grid_cice.bin'] == Path('/ocean/MIT/5400x15/cice/grid_cice.bin')
    assert links['kmt_cice.bin'] == Path('/ocean/MIT/5400x15/cice/kmt_cice.bin')
    assert links['tile.data'] == creator.bcs_dir / f"geometry/{creator.bcrslv}/{creator.bcrslv}-Pfafstetter.til"
    assert links['runoff.bin'] == creator.bcs_dir / f"geometry/{creator.bcrslv}/{creator.bcrslv}-Pfafstetter.TRN"


def test_stagebcs_mit_explicit_tile_and_input_dir(tmp_path):
    stagebcs = _load_stagebcs()
    config_path = _mit_stagebcs_config(tmp_path, tile='/custom/tile.data', input_dir='/custom/INPUT')
    creator = stagebcs.SymlinkCreator(stagebcs.CatalogManager(config_path))

    assert creator.compile_links()['tile.data'] == Path('/custom/tile.data')
    assert creator.input_dir() == {'INPUT': Path('/custom/INPUT')}


def test_stagebcs_mit_bcs_dir_and_land_tag_override(tmp_path):
    stagebcs = _load_stagebcs()
    config_path = _mit_stagebcs_config(
        tmp_path, bcs_dir='/bcs/root', land_bcrslv='CF0090x6C_M6TP1440x1080')
    creator = stagebcs.SymlinkCreator(stagebcs.CatalogManager(config_path))

    assert creator.bcs_dir == Path('/bcs/root')
    assert creator.bcrslv == 'CF0090x6C_M6TP1440x1080'
    assert creator.compile_links()['runoff.bin'] == Path(
        '/bcs/root/geometry/CF0090x6C_M6TP1440x1080/CF0090x6C_M6TP1440x1080-Pfafstetter.TRN')


def test_seaice_cice4_paths_for_mom6(tmp_path):
    stagebcs = _load_stagebcs()
    config_path = _write_stagebcs_fixture(tmp_path, overrides={
        'agcm_grid': 'c180', 'ocean_model': 'mom6', 'ocean_resolution': 'o1440',
        'ogcm_grid': 'coupled', 'seaice_model': 'cice',
    })
    creator = stagebcs.SymlinkCreator(stagebcs.CatalogManager(config_path))

    paths = creator.seaice_paths()
    assert paths['kmt_cice.bin'] == creator.coupled_dir / '1440x1080' / 'cice/kmt_cice.bin'
    assert paths['grid_cice.bin'] == creator.coupled_dir / '1440x1080' / 'cice/grid_cice.bin'


def _write_tile(path, agcm, ocean):
    lines = ['tile file', 'header',
             f'{agcm[0]}  gridname', f'{agcm[1]}  im', f'{agcm[2]}  jm',
             f'{ocean[0]}  ogrid', f'{ocean[1]}  oim', f'{ocean[2]}  ojm']
    path.write_text('\n'.join(lines) + '\n')


def test_mit_tile_header_must_match_grids(tmp_path):
    stagebcs = _load_stagebcs()
    tile = tmp_path / 'tile.data'
    config_path = _mit_stagebcs_config(tmp_path, tile=str(tile))
    creator = stagebcs.SymlinkCreator(stagebcs.CatalogManager(config_path))

    _write_tile(tile, ('PE90x540-CF', 90, 540), ('LL5400x15-LL', 5400, 15))
    creator.check_mit_tile()

    _write_tile(tile, ('PE90x540-CF', 90, 540), ('LL5400x15-LL', 5400, 30))
    with pytest.raises(SystemExit, match='do not match'):
        creator.check_mit_tile()


def test_stage_bcs_mit_writes_inputs_and_summary(tmp_path, monkeypatch):
    monkeypatch.setitem(setup_main.envdict, 'site', 'NAS')
    monkeypatch.setitem(setup_main.pathdict, 'install', '/fake/install')
    monkeypatch.setitem(setup_main.pathdict, 'bin', '/fake/bin')
    monkeypatch.setattr(setup_main.subprocess, 'run', lambda *a, **k: None)
    monkeypatch.setenv('GEOS_MIT_TILE', '/mine/tile.data')

    inst = SetupGCM.__new__(SetupGCM)
    inst.expConfig = {'exp_dir': str(tmp_path)}
    inst.boundary_dir = '/nobackup/gmao'
    inst.coupled_dir = Path('/nobackup/gmao/bcs_shared/make_bcs_inputs/ocean')
    inst.atmos = {'im': 90, 'jm': 540, 'gridname': 'PE90x540-CF', 'res': 'CF0090x6C',
                  'nx': 10, 'ny': 36}
    inst.land = {'bcs': 'NL3', 'model': 'Catchment'}
    inst.ocean = {
        'running_ocean': True, 'model': 'MIT', 'seaice_model': 'CICE4', 'llc': 'llc90',
        'IM': 5400, 'JM': 15, 'LM': 50, 'NX': 360, 'NY': 1, 'nprocs': 360,
        'kpar_file': 'SEAWIFS_KPAR_mon_clim.5400x15', 'mit_config_id': 'c90_llc90_02',
        'res': 'MITLLC5400x0015', 'gridname': 'LL5400x15-LL',
    }
    inst.sst_dir = '/sst'
    inst.chem_dir = '/chem'

    inst.stage_bcs()

    config = yaml.safe_load((tmp_path / 'linkbcs_config.yaml').read_text())
    assert config['ocean_model'] == 'mit'
    assert config['ocean_resolution'] == 'llc90'
    assert config['mit']['tile'] == '/mine/tile.data'
    assert config['mit']['cice_kmt'] == '/nobackup/gmao/bcs_shared/make_bcs_inputs/ocean/MIT/5400x15/cice/kmt_cice.bin'
    assert config['mit']['expect']['ogcm_im'] == 5400
    assert (tmp_path / 'mit_input').is_dir()
    summary = (tmp_path / 'MIT_SETUP.txt').read_text()
    assert 'MIT_CONFIG_ID=c90_llc90_02' in summary
    assert 'Tile: /mine/tile.data' in summary
