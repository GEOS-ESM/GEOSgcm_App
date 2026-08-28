#!/usr/bin/env python3

import yaml
import argparse
import logging
import os
import sys
import shutil
from pathlib import Path
from datetime import datetime


class ProvenanceRecorder:
    def __init__(self):
        self.working_directory = self._working_directory()
        self.log_path = self.working_directory / "linkbcs.log"
        self.manifest_path = self.working_directory / "linkbcs_manifest.yaml"
        self.actions = []
        self.config = None
        self.timestamp = None
        self.success = False
        self.error = None

        self.logger = logging.getLogger("linkbcs")
        self.logger.setLevel(logging.DEBUG)
        self.logger.handlers.clear()
        formatter = logging.Formatter("%(name)s %(levelname)s %(message)s")
        for handler in (logging.FileHandler(self.log_path, mode="w"), logging.StreamHandler()):
            handler.setFormatter(formatter)
            self.logger.addHandler(handler)

    def _working_directory(self):
        logical_directory = Path(os.environ.get("PWD", Path.cwd()))
        if logical_directory.exists() and os.path.samefile(logical_directory, Path.cwd()):
            return logical_directory
        return Path.cwd()

    def absolute_path(self, path):
        path = Path(path)
        if path.is_absolute():
            return path
        return self.working_directory / path

    def record(self, action, destination=None, source=None, **details):
        entry = {"action": action}
        if destination is not None:
            entry["destination"] = str(self.absolute_path(destination))
        if source is not None:
            entry["source"] = str(self.absolute_path(source))
        entry.update({key: str(value) if isinstance(value, Path) else value for key, value in details.items()})
        self.actions.append(entry)

        description = action
        if destination is not None:
            description += f" {entry['destination']}"
        if source is not None:
            description += f" -> {entry['source']}"
        if details:
            description += " " + ", ".join(f"{key}={value}" for key, value in details.items())
        self.logger.debug(description)

    def write_manifest(self):
        manifest = {
            "generated_at": datetime.now().astimezone().isoformat(),
            "working_directory": str(self.working_directory),
            "config": self.config,
            "timestamp": self.timestamp,
            "success": self.success,
            "error": self.error,
            "actions": self.actions,
        }
        with open(self.manifest_path, "w") as f:
            yaml.safe_dump(manifest, f, sort_keys=False)

def validate_iso_datetime(datetime_string):
    try:
        date = datetime.fromisoformat(datetime_string)
        return date
    except ValueError:
        raise argparse.ArgumentTypeError(
            f"Invalid datetime format: '{datetime_string}'. "
            f"Expected ISO format (YYYY-MM-DDTHH:MM:SS), e.g., '2025-04-04T00:00:00'"
        )

def validate_yaml_file(file_path):
    path = Path(file_path)

    # Check if file exists
    if not path.exists():
        raise argparse.ArgumentTypeError(f"File does not exist: {file_path}")

    # Check file extension
    if path.suffix.lower() not in ['.yaml', '.yml']:
        raise argparse.ArgumentTypeError(f"File must have .yaml or .yml extension: {file_path}")

    return file_path

def capture_arguments():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--config",
        required=True,
        type=validate_yaml_file,
        help="User-provided YAML configuration file"
    )
    parser.add_argument(
        "--timestamp",
        required=True,
        type=validate_iso_datetime,
        help="ISO format date"
    )

    args = parser.parse_args()
    return args


class CatalogManager:
    def __init__(self, config_path: Path):
        self.config = self.import_yaml(config_path)
        self.catalog = self.import_yaml(Path(self.config['install_dir']) / "etc" / "bcs_catalog.yaml")

    def import_yaml(self, yaml_file: Path) -> dict:
        with open(yaml_file, 'r') as f:
            return yaml.safe_load(f)

    # pulls value from catalog using user config to navigate hierarchy
    def get_value(self, target: str, section: str):
        catalog_section = self.catalog[section]
        return self._search_recursive(target, catalog_section)

    # Helper function for recursively searching through catalog
    def _search_recursive(self, target: str, catalog_section: dict):
        # Found the target we're looking for
        if target in catalog_section:
            return catalog_section[target]

        config_values = set(self.config.values())

        # Search each subdictionary that matches a config value
        for section_name, section_data in catalog_section.items():

            # Skip if not a dictionary
            if not isinstance(section_data, dict):
                continue

            # Skip if this section name doesn't match any config value
            if section_name not in config_values:
                continue

            # Recursively search this matching section
            result = self._search_recursive(target, section_data)

            # Return immediately if we found something
            if result is not None:
                return result

        return None


class SymlinkCreator:
    def __init__(self, catalog: CatalogManager, year: int, recorder: ProvenanceRecorder):
        self.catalog = catalog
        self.config = catalog.config
        self.year = year
        self.recorder = recorder

        # platform
        try:
            with open(f"{self.config['install_dir']}/etc/SITE.rc") as f:
                self.site = f.readline().partition(":")[2].strip()
        except FileNotFoundError:
            self.site = None
        self.boundary_dir = Path(self.config['boundary_dir'])
        self.atmos_bcs = catalog.get_value('atmos_bcs', 'platform')
        self.gwdrs_dir = self.boundary_dir / catalog.get_value('gwdrs_dir', 'platform')

        # experiment type
        self.extdata_files = catalog.get_value('extdata_files', 'experiment_type')
        self.chem_dir = self.boundary_dir / catalog.get_value('chem_dir', 'experiment_type')
        self.fvInput_dir = catalog.get_value('fvInput_dir', 'experiment_type')

        # land
        self.stream = catalog.get_value('stream', 'land_version')
        self.lsm_bcs = catalog.get_value('lsm_bcs', 'land_version')
        self.bcs_dir = self.boundary_dir / "bcs_shared/fvInput/ExtData/esm/tiles" / self.lsm_bcs

        # atmos
        self.atmos_tag = catalog.get_value('tag', 'agcm_grid')
        self.agcm_IM = catalog.get_value('IM', 'agcm_grid')
        self.agcm_JM = catalog.get_value('JM', 'agcm_grid')

        # ocean
        if self.config["ocean_model"] != "data":
            self.coupled = True
        else:
            self.coupled = False
        self.ogrid_type = catalog.get_value('ogrid_type', 'ocean_model')
        self.ogcm_IM = catalog.get_value('IM', 'ocean_model')
        self.ogcm_JM = catalog.get_value('JM', 'ocean_model')
        if self.config['ogcm_grid'] == 'cubed_sphere_ostia':
            self.ogcm_IM = self.agcm_IM
            self.ogcm_JM = self.agcm_JM
        self.ocean_res = f"{self.ogcm_IM}x{self.ogcm_JM}"
        self.coupled_dir = self.boundary_dir / f"bcs_shared/make_bcs_inputs/ocean/{self.config['ocean_model'].upper()}"
        self.tables = catalog.get_value('tables', 'ocean_model')
        self.sst_name = catalog.get_value('sst_name', 'ocean_model')
        self.sst_file = catalog.get_value('sst_file', 'ocean_model')
        self.ice_file = catalog.get_value('ice_file', 'ocean_model')
        self.kpar_file = catalog.get_value('kpar_file', 'ocean_model')

        # seaice
        self.kmt_cice = catalog.get_value('kmt', 'seaice_model')
        self.grid_cice = catalog.get_value('grid', 'seaice_model')
        self.global_bathy = catalog.get_value('global_bathy', 'seaice_model')

        # pchem_species
        self.species_data_dir = self.bcs_dir / catalog.get_value('species_data', 'pchem_species')

        # precip correction
        self.m21c_dir = Path(catalog.get_value('m21c', 'precip_correction'))
        self.merra2_dir = Path(catalog.get_value('merra-2', 'precip_correction'))

        # misc
        self.bcrslv = f"{self.atmos_tag}_{self.ogrid_type}{str(self.ogcm_IM).zfill(4)}x{str(self.ogcm_JM).zfill(4)}"
        if self.config['ogcm_grid'] == "cubed_sphere_ostia":
            self.bcrslv = f"{self.atmos_tag}_CF{str(self.ogcm_IM).zfill(4)}x6C"
        elif self.config['ocean_model'] == "data":
            self.bcrslv = f"{self.atmos_tag}_DE{str(self.ogcm_IM).zfill(4)}xPE{str(self.ogcm_JM).zfill(4)}"
        self.topo_src_dir = self.boundary_dir / self.atmos_bcs / "TOPO" / self.stream / self.atmos_tag / "smoothed"

        # sst_dir
        if not self.coupled and self.ocean_res == "1440x720":
            self.sst_dir = self.boundary_dir / self.fvInput_dir / f"g5gcm/bcs/SST/{self.ocean_res}"
        elif not self.coupled:
            self.sst_dir = self.boundary_dir / self.fvInput_dir / f"g5gcm/bcs/realtime/{self.sst_name}/{self.ocean_res}"

        if self.site != "NAS" and self.site != "NCCS" and not self.coupled:
            self.sst_dir = self.boundary_dir / "SST" / self.ocean_res
        elif self.coupled:
            self.sst_dir = self.coupled_dir / f"SST/MERRA2/{self.ocean_res}/v1"
        else:
            #exception
            pass

    def create_symlink(self, symlink_name: Path, file_path: Path):
        # remove existing link if it exists (equivalent of -f flag)
        if symlink_name.is_symlink():
            symlink_name.unlink()
            self.recorder.record("remove_symlink", symlink_name)

        if file_path.exists():
            symlink_name.symlink_to(file_path)
            self.recorder.record("create_symlink", symlink_name, file_path)
        else:
            self.recorder.record("skip_missing_source", symlink_name, file_path)

    def topo_paths(self) -> dict:
        paths = {
            "topo_dynave.data": self.topo_src_dir / f"topo_DYN_ave_{self.agcm_IM}x{self.agcm_JM}.data",
            "topo_gwdvar.data": self.topo_src_dir / f"topo_GWD_var_{self.agcm_IM}x{self.agcm_JM}.data",
            "topo_trbvar.data": self.topo_src_dir / f"topo_TRB_var_{self.agcm_IM}x{self.agcm_JM}.data"
        }

        return paths

    def land_paths(self) -> dict:
        land_src_dir = self.bcs_dir / "land" / self.bcrslv
        paths = {
            "visdf.dat": land_src_dir / f"visdf_{self.agcm_IM}x{self.agcm_JM}.dat",
            "nirdf.dat": land_src_dir / f"nirdf_{self.agcm_IM}x{self.agcm_JM}.dat",
            "vegdyn.data": land_src_dir / f"vegdyn_{self.agcm_IM}x{self.agcm_JM}.dat",
            "lai.data": land_src_dir / f"lai_clim_{self.agcm_IM}x{self.agcm_JM}.data",
            "green.data": land_src_dir / f"green_clim_{self.agcm_IM}x{self.agcm_JM}.data",
            "ndvi.data": land_src_dir / f"ndvi_clim_{self.agcm_IM}x{self.agcm_JM}.data"
        }

        return paths

    def make_restart_dir(self):
        if self.coupled != "data":
            restart_dir = Path("RESTART")
            existed = restart_dir.exists()
            restart_dir.mkdir(parents=True, exist_ok=True)
            self.recorder.record("create_directory", restart_dir, existed=existed)

    def make_extdata_dir(self):
        extdata = Path("ExtData")
        existed = extdata.exists()
        extdata.mkdir(parents=True, exist_ok=True)
        self.recorder.record("create_directory", extdata, existed=existed)

        for file in self.extdata_files:
            self.create_symlink(extdata / file, self.chem_dir / file)

        # exit here if not coupled ocean
        if not self.coupled:
            return
        dataatm_dir = self.boundary_dir / "bcs_shared/make_bcs_inputs/ocean/dataatm"
        for item in dataatm_dir.glob("*"):
            self.create_symlink(extdata / item.name, dataatm_dir / item.name)


    def seawifs_path(self) -> dict:
        if not self.coupled:
            return {}
        paths = {"SEAWIFS_KPAR_mon_clim.data": self.coupled_dir / f"{self.ogcm_IM}x{self.ogcm_JM}/SEAWIFS_KPAR_mon_clim.{self.ogcm_IM}x{self.ogcm_JM}"}
        return paths

    def tile_paths(self) -> dict:
        tile_data = self.bcs_dir / "geometry" / self.bcrslv / f"{self.bcrslv}-Pfafstetter.til"
        tile_bin = self.bcs_dir / "geometry" / self.bcrslv / f"{self.bcrslv}-Pfafstetter.til.bin"
        tile_nc4 = self.bcs_dir / "geometry" / self.bcrslv / f"{self.bcrslv}-Pfafstetter.nc4"
        paths = {"tile.data": tile_data, "tile.bin": tile_bin}
        
        if tile_nc4.exists():
            paths["tile.nc4"] = tile_nc4

        return paths

    def runoff_path(self) -> dict:
        if not self.coupled:
            return {}
        paths = {"runoff.bin": self.bcs_dir / "geometry" / self.bcrslv / f"{self.bcrslv}-Pfafstetter.TRN"}
        return paths

    def mapl_tripolar_path(self) -> dict:
        if self.config["ocean_model"] != "mom6":
            return {}
        paths = {"MAPL_Tripolar.nc": self.coupled_dir / f"{self.ogcm_IM}x{self.ogcm_JM}/MAPL_Tripolar.nc"}
        return paths

    def vgrid_path(self) -> dict:
        if self.config["ocean_model"] != "mom6":
            return {}

        if self.agcm_IM == 12 or self.agcm_IM == 90:
            ogcm_LM = 50
        elif self.agcm_IM ==  180:
            ogcm_LM = 75
        else:
            sys.exit("ERROR: must use c12, c90, or c180 with MOM6!")

        paths = {"vgrid.ascii": self.coupled_dir / f"{self.ogcm_IM}x{self.ogcm_JM}/vgrid{ogcm_LM}.ascii"}
        return paths

    def MIT_paths(self) -> dict:
        if self.config["ocean_model"] != "MIT":
            return {}

        paths = {
            "mit.ascii": self.bcs_dir / f"geometry/{self.bcrslv}/mit.ascii",
            "DC0360xPC0181_LL5400x15-LL.bin": self.coupled_dir / "DC0360xPC0181_LL5400x15-LL.bin"
        }

        return paths

    def precip_path(self) -> dict:
        if not self.config["precip_correction"]:
            return {}
        path = {"ExtData/PCP": self.precip_dir}
        return path

    def species_path(self) -> dict:
        path = {"species.data": self.species_data_dir}

        return path

    def catchcn_paths(self) -> dict:
        if not self.config["catchcn"]:
            return {}

        paths = {}
        lnfm_data = self.bcs_dir / "land" / self.bcrslv / f"lnfm_clim_{self.agcm_IM}x{self.agcm_JM}.data"
        if lnfm_data.exists():
            paths["lnfm.data"] = lnfm_data

        paths["CO2_MonthlyMean_DiurnalCycle.nc4"] = self.bcs_dir / "land/shared/CO2_MonthlyMean_DiurnalCycle.nc4"
        return paths

    # Optional internal restart
    # COPY (not symlinking these)
    def copy_internal_restart(self):
        gwd_rst = self.topo_src_dir / "gwd_internal_rst"
        gwd_agcm = self.gwdrs_dir / f"gwd_internal_c{self.agcm_IM}"
        if gwd_rst.exists():
            shutil.copy(gwd_rst, Path.cwd())
            self.recorder.record("copy_file", Path.cwd() / gwd_rst.name, gwd_rst)
        elif gwd_agcm.exists():
            # We need to copy the gwd_internal_c{IM} file to the current working directory as gwd_internal_rst for the model to find it
            shutil.copy(gwd_agcm, Path.cwd() / "gwd_internal_rst")
            self.recorder.record("copy_file", Path.cwd() / "gwd_internal_rst", gwd_agcm)
        else:
            self.recorder.record("skip_missing_internal_restart", source=gwd_rst, fallback_source=gwd_agcm)


    def table_paths(self) -> dict:
        if self.config["ocean_model"] != "mom6":
            return {}
        paths = {
            "diag_table": Path(self.config["install_dir"]) / f"etc/MOM6/mom6_app/{self.ogcm_IM}x{self.ogcm_JM}/diag_table",
            "data_table": Path(self.config["install_dir"]) / f"etc/MOM6/mom6_app/{self.ogcm_IM}x{self.ogcm_JM}/data_table"
        }
        return paths

    def make_input_dir(self):
        if not self.coupled:
            return {}

        src_dir = self.coupled_dir / f"{self.ogcm_IM}x{self.ogcm_JM}/INPUT"
        target_dir = Path("INPUT")

        # make input dir if it doesn't already exist
        existed = target_dir.exists()
        target_dir.mkdir(parents=True, exist_ok=True)
        self.recorder.record("create_directory", target_dir, existed=existed)

        for file_path in src_dir.glob("*"):
            if file_path.is_file():
                # copy2 preserves file metadata
                shutil.copy2(file_path, target_dir / file_path.name)
                self.recorder.record("copy_file", target_dir / file_path.name, file_path)

    def seaice_paths(self) -> dict:
        if not self.coupled:
            return {}
        if self.config["seaice_model"] == "cice4":
            paths = {
                "kmt_cice.bin": self.coupled_dir / f"{self.ogcm_IM}x{self.ogcm_JM}" / self.kmt_cice,
                "grid_cice.bin": self.coupled_dir / f"{self.ogcm_IM}x{self.ogcm_JM}" / self.grid_cice
            }
        elif self.config["seaice_model"] == "cice6":
            paths = {
                "cice6_grid.nc": self.coupled_dir / f"{self.ogcm_IM}x{self.ogcm_JM}" / self.grid_cice,
                "cice6_kmt.nc": self.coupled_dir / f"{self.ogcm_IM}x{self.ogcm_JM}" / self.kmt_cice,
                "cice6_global.bathy.nc": self.coupled_dir / f"{self.ogcm_IM}x{self.ogcm_JM}" / self.global_bathy
            }
        return paths

    def dataocean_paths(self) -> dict:
        if self.coupled:
            return {}

        if self.config["ogcm_grid"] == "reynolds":
            paths = {
                "sst.data": self.sst_dir / f"{self.sst_file}.{self.ocean_res}.LE",
                "fraci.data": self.sst_dir / f"{self.ice_file}.{self.ocean_res}.LE"
            }
        else:
            paths = {
                "sst.data": self.sst_dir / f"{self.sst_file}.{self.ocean_res}.{self.year}.data",
                "fraci.data": self.sst_dir / f"{self.ice_file}.{self.ocean_res}.{self.year}.data"
            }
        paths["SEAWIFS_KPAR_mon_clim.data"] = self.sst_dir / f"{self.kpar_file}.{self.ocean_res}"

        return paths

    def dualocean_paths(self) -> dict:
        if 'dual_ocean' not in self.config or not self.config['dual_ocean']:
            return {}
        paths = {
            "sst.data": self.sst_dir / f"dataoceanfile.MERRA2_SST.{self.ogcm_IM}x{self.ogcm_JM}.{self.year}.data",
            "fraci.data": self.sst_dir / f"dataoceanfile.MERRA2_ICE.{self.ogcm_IM}x{self.ogcm_JM}.{self.year}.data"
        }

        return paths

    def make_symlinks(self):
        paths = {}
        paths.update(self.topo_paths())
        paths.update(self.land_paths())
        paths.update(self.seawifs_path())
        paths.update(self.tile_paths())
        paths.update(self.runoff_path())
        paths.update(self.mapl_tripolar_path())
        paths.update(self.vgrid_path())
        paths.update(self.MIT_paths())
        paths.update(self.precip_path())
        paths.update(self.species_path())
        paths.update(self.catchcn_paths())
        paths.update(self.table_paths())
        paths.update(self.seaice_paths())
        paths.update(self.dataocean_paths())
        paths.update(self.dualocean_paths())

        self.print_paths(paths)

        for items in paths:
            self.create_symlink(Path(items), paths[items])

        self.make_restart_dir()
        self.make_extdata_dir()
        self.copy_internal_restart()
        self.make_input_dir()

    # returns broken paths and prints warnings for each broken path and exit
    def print_paths(self, paths):
        missing_files = []
        for i in paths:
            if not paths[i].exists():
                missing_files.append(i)
                # We print an error if the missing file is *not* tile.bin
                if i != "tile.bin":
                    self.recorder.record("missing_required_source", Path(i), paths[i])
                    self.recorder.logger.error("%s does not exist at: %s", i, paths[i])
                else:
                    self.recorder.record("missing_optional_source", Path(i), paths[i])
                    self.recorder.logger.warning("%s does not exist at: %s", i, paths[i])

        if missing_files:
            # We must allow for the one case that is only tile.bin is in missing_files,
            # in which case gcm_run.j will make tile.bin from tile.data
            if len(missing_files) == 1 and missing_files[0] == "tile.bin":
                self.recorder.logger.warning(
                    "tile.bin is missing, but tile.data exists. The model can still run with tile.data."
                )
            else:
                self.recorder.logger.error("One or more paths are broken. Please check the warnings above.")
                sys.exit(1)


def main():
    args = capture_arguments()
    recorder = ProvenanceRecorder()
    try:
        recorder.timestamp = args.timestamp.isoformat()
        catalog_manager = CatalogManager(Path(args.config))
        recorder.config = catalog_manager.config
        recorder.logger.debug("Configuration loaded from %s", Path(args.config).resolve())
        recorder.logger.debug("Requested timestamp: %s", recorder.timestamp)
        symlink_creator = SymlinkCreator(catalog_manager, args.timestamp.year, recorder)
        symlink_creator.make_symlinks()
        recorder.success = True
        recorder.logger.debug("linkbcs completed successfully")
    except (Exception, SystemExit) as error:
        recorder.error = str(error)
        recorder.logger.error("linkbcs failed: %s", error)
        raise
    finally:
        recorder.write_manifest()

if __name__ == "__main__":
    main()
