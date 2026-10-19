"""
Coupled-setup timestep patching (gcm_setup:2631-2656 parity).

``config_heartbeat`` must write **HEARTBEAT_DT** (``atmos['dt']``) into
``ice_in``'s active ``dt =`` line and into ``MOM_override``'s DT/DT_THERM
entries, exactly like upstream's ``set OCEAN_DT = $HEARTBEAT`` for coupled
runs ("the ocean model must have the same time step as the atmosphere").

It previously used ``atmos['dt_ocean']`` -- the OGCM_RUN_DT catalog value,
an int -- which (a) raised ``TypeError`` on string concatenation, so the
first coupled setup crashed, (b) would have written a timestep that is
neither the atmosphere step nor what csh writes, and (c) skipped ``ice_in``
anyway because the pattern's ``^`` never matched mid-file without
re.MULTILINE.
"""

from gcmpy.setup_tools.setup_main import SetupGCM


def _setup(tmp_path, seaice_model='CICE6', model='MOM6',
           dt='450', dt_ocean=3600):
    inst = SetupGCM.__new__(SetupGCM)
    inst.expConfig = {'exp_dir': str(tmp_path)}
    inst.atmos = {'dt': dt, 'dt_ocean': dt_ocean}
    inst.ocean = {'seaice_model': seaice_model, 'model': model}
    return inst


def test_config_heartbeat_patches_ice_in_and_mom_override(tmp_path):
    ice_in = tmp_path / "ice_in"
    ice_in.write_text(
        "&setup_nml\n"
        "    days_per_year  = 365\n"
        "    dt             = 900.0\n"
        "    npt            = 99999\n"
        "&end\n"
    )
    mom_override = tmp_path / "MOM_override"
    mom_override.write_text(
        "! Blank file in which we can put \"overrides\" for parameters\n"
        "#override DT = 900.0\n"
        "#override DT_THERM = 900.\n"
        "TOPO_FILE = \"ocean_topog.nc\"\n"
    )

    _setup(tmp_path, dt='450', dt_ocean=3600).config_heartbeat()

    ice_text = ice_in.read_text()
    assert "dt             = 450\n" in ice_text
    assert "900.0" not in ice_text
    # Non-target namelist entries stay untouched.
    assert "npt            = 99999" in ice_text

    override_text = mom_override.read_text()
    assert "#override DT = 450\n" in override_text
    assert "#override DT_THERM = 450\n" in override_text
    assert "TOPO_FILE = \"ocean_topog.nc\"" in override_text


def test_config_heartbeat_follows_heartbeat_not_dt_ocean(tmp_path):
    # atmos['dt'] drives the result; dt_ocean (OGCM_RUN_DT) must be ignored.
    ice_in = tmp_path / "ice_in"
    ice_in.write_text("    dt             = 900.0\n")
    mom_override = tmp_path / "MOM_override"
    mom_override.write_text("#override DT = 900.0\n#override DT_THERM = 900.\n")

    _setup(tmp_path, dt='450', dt_ocean=3600).config_heartbeat()

    assert "dt             = 450\n" in ice_in.read_text()
    assert "3600" not in ice_in.read_text()
    assert "#override DT = 450\n" in mom_override.read_text()
    assert "3600" not in mom_override.read_text()


def test_config_heartbeat_tolerates_integer_heartbeat(tmp_path):
    # A driver yaml with an unquoted `heartbeat: 450` yields an int.
    ice_in = tmp_path / "ice_in"
    ice_in.write_text("    dt             = 900.0\n")
    mom_override = tmp_path / "MOM_override"
    mom_override.write_text("#override DT = 900.0\n#override DT_THERM = 900.\n")

    _setup(tmp_path, dt=450, dt_ocean=3600).config_heartbeat()

    assert "dt             = 450\n" in ice_in.read_text()


def test_config_heartbeat_is_noop_for_uncoupled_config(tmp_path):
    # Data ocean: neither ice_in nor MOM_override exists, nothing to patch.
    inst = _setup(tmp_path, seaice_model=None, model='Data Ocean (o1)')

    inst.config_heartbeat()

    assert list(tmp_path.iterdir()) == []
