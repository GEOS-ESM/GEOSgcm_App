"""
Config-key coverage audit.

``SetupGCM.config_surfaceGridComp`` blew up in a live run with
``KeyError: 'bcs'``: the merged hand-merge referenced
``self.atmos['bcs']``, a key no config builder ever produces (the value
lives in ``self.land['bcs']``, per upstream gcm_setup's ``$LSM_BCS``).
This test cross-checks every ``self.<atmos|ocean|land|gocart>['key']``
literal in setup_main.py against the keys ``config_models()`` actually
builds, so the whole class of missing-key bugs fails here instead of in
a live setup run.
"""

import re
from pathlib import Path

import pytest

import gcmpy.setup_tools.setup_main as setup_main
from gcmpy.setup_tools.setup_main import SetupGCM
from gcmpy_tests.gcmpy.setup_tools.test_template_vars import _exp_config

CONFIG_NAMES = ('atmos', 'ocean', 'land', 'gocart')

_REF_RE = re.compile(
    r"self\.(atmos|ocean|land|gocart)\[\s*['\"]([^'\"]+)['\"]\s*\]"
)


@pytest.fixture
def built_configs(tmp_path, monkeypatch):
    etc = tmp_path / 'etc'
    etc.mkdir()
    (etc / '.AGCM_VERSION').write_text('v-test\n')
    monkeypatch.setitem(setup_main.pathdict, 'etc', etc)

    setup = SetupGCM(_exp_config(tmp_path))
    setup.config_models()
    return {name: set(getattr(setup, name)) for name in CONFIG_NAMES}


def test_setup_main_config_key_refs_all_resolve(built_configs):
    source = Path(setup_main.__file__).read_text()

    refs = {name: set() for name in CONFIG_NAMES}
    for name, key in _REF_RE.findall(source):
        refs[name].add(key)

    total = sum(len(keys) for keys in refs.values())
    assert total >= 20, (
        f'only {total} config-key refs matched in setup_main.py -- '
        'the audit regex has probably gone stale'
    )

    missing = {
        name: sorted(refs[name] - built_configs[name])
        for name in CONFIG_NAMES
        if refs[name] - built_configs[name]
    }
    assert not missing, (
        'setup_main.py references config keys that config_models() never '
        f'builds (would raise KeyError at runtime): {missing}'
    )
