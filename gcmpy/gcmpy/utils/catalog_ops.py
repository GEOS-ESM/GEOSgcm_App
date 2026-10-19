import yaml
from pathlib import Path
from typing import Any


# Sentinel to distinguish "found with value None" from "key not found".
_NOT_FOUND = object()


class CatalogSearch:
    """
    Provides config-driven recursive catalog lookups for YAML catalog files.
    Used by catalog-driven setup scripts (ocean.py, atmosphere.py, land.py, etc.)

    The recursive search navigates nested catalog dicts using expConfig values
    as navigation keys. Two special cases are handled:

    1. Value-lookup dicts: when a target key is found but its value is a dict
       (e.g. nx: {MOM6: 5, MIT: 10, data_ocean: 10}), the value is resolved by
       matching a config value against the sub-dict keys, with 'data_ocean' as
       a fallback when no config value matches.

    2. Structural pass-through: grouping keys that are not config values
       (e.g. 'resolutions', 'resolution') are descended into transparently in a
       second pass, allowing navigation to reach config-matched sub-dicts within.
    """

    def __init__(self, catalog: dict, expConfig: dict):
        self.catalog = catalog
        self.config  = expConfig

    def get_value(self, target: str, section: str) -> Any:
        """
        Return the value for target by searching recursively through catalog[section].

        Parameters
        ----------
        target : str
            The key to search for.
        section : str
            The top-level catalog section to search within.

        Returns
        -------
        Any
            The resolved value, or None if not found.
        """
        result = self._search_recursive(target, self.catalog[section])
        return None if result is _NOT_FOUND else result

    def _search_recursive(self, target: str, catalog_section: dict):
        config_values = set(str(v) for v in self.config.values())

        if target in catalog_section:
            value = catalog_section[target]
            # Plain scalar -- return directly.
            if not isinstance(value, dict):
                return value
            # Value-lookup dict (e.g. nx: {MOM6: 5, MIT: 10, data_ocean: 10}):
            # resolve via a matching config value, fall back to data_ocean.
            for k, v in value.items():
                if str(k) in config_values:
                    return v
            if 'data_ocean' in value:
                return value['data_ocean']
            return _NOT_FOUND

        # First pass: descend into sub-dicts whose key matches a config value.
        matched_config_section = False
        for section_name, section_data in catalog_section.items():
            if not isinstance(section_data, dict):
                continue
            if section_name not in config_values:
                continue
            matched_config_section = True
            result = self._search_recursive(target, section_data)
            if result is not _NOT_FOUND:
                return result

        # Second pass: transparent pass-through for structural grouping keys
        # (e.g. 'resolutions', 'resolution') that are not config values.
        # Only runs when NO config-matched section exists at this level; if a
        # config-matched section was found (e.g. resolutions -> c12) but the
        # target is absent within it, the correct result is "unset" -- falling
        # through into sibling sections (e.g. a stretched c270 defining conus)
        # would leak another resolution's settings.
        if not matched_config_section:
            for section_name, section_data in catalog_section.items():
                if not isinstance(section_data, dict):
                    continue
                if section_name in config_values:
                    continue
                result = self._search_recursive(target, section_data)
                if result is not _NOT_FOUND:
                    return result

        return _NOT_FOUND


def load_catalog(path: Path) -> dict:
    """
    Load a YAML catalog file and return its contents as a dict.

    Parameters
    ----------
    path : Path
        Path to the catalog YAML file.

    Returns
    -------
    dict
        Parsed catalog dictionary.
    """
    with open(path, 'r') as f:
        return yaml.safe_load(f)
