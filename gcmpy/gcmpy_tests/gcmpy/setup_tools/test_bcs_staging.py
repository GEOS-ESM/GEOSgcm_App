"""
BCS-staging fixes: initial ``cap_restart`` creation and site-aware
``sst_dir``/``chem_dir`` propagation into the stagebcs/linkbcs pipeline.

gcmpy setup previously never wrote ``$EXPDIR/cap_restart`` (the run-side
``read_edate`` requires it for initial-restart archiving), and
``stagebcs.py`` composed SST/ExtData roots from ``boundary_dir`` plus
catalog relative names, which misses NCCS's ``share/gmao_ops`` prefix.
These tests pin the fixed behavior, including the o1 (Reynolds) data-ocean
``bcrslv`` that matches the on-disk land-tile directories.
"""

import importlib.util
from pathlib import Path

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
    inst.begin_date = '18910301 000000'
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


def test_organize_exp_dir_writes_initial_cap_restart(tmp_path, monkeypatch):
    inst = _bare_setup(tmp_path)
    monkeypatch.setattr(setup_main, 'copy_src_tarfile', lambda *a, **k: None)
    monkeypatch.setattr(inst, 'make_RESTART_dir', lambda: None)

    inst.organize_exp_dir()

    cap_restart = tmp_path / "cap_restart"
    assert cap_restart.is_file()
    assert cap_restart.read_text() == '18910301 000000\n'


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
