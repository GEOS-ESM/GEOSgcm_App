#import rc_parser
#import cpu_math
#import ioserver
from pathlib import Path
from gcmpy.batch_tools.batch_environ import load_env_vars, get_batch_resources
from gcmpy.run_tools.extdata import stage_extdata
from gcmpy.run_tools.emissions import stage_amip_emissions

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
    return args

def import_yaml(self, yaml_file: Path) -> dict:
    with open(yaml_file, 'r') as f:
        return yaml.safe_load(f)

def main():
    args = capture_arguements()
    gcm_dict = import_yaml(Path(args.config))
    
    '''
    rc_parser.update_end_date(Path("CAP.rc"), year, month)
    rc_parser.update_end_date(Path("CAP.rc"), nymdf, nhmsf)
    rc_parser.update_num_readers(Path("AGCM.rc"), num_readers)
    rc_parser.update_domain_start(Path("ww3_multi.nml"), nymdc, nhmsc)
    rc_parser.update_domain_end(Path("ww3_multi.nml"), nymde, nymde)
    '''
    gcmdict = {}
    nx = 1
    ny = 2 
    env_dict = load_env_vars()
    gcmdict.update(get_batch_resources(nx, ny, env_dict))
    print(f"env_dict = {env_dict}\ngcm_dict = {gcmdict}")
    '''
    if gcmdict["NCPUS"] != None:
        total_pes = ioserver.calculate_total_pes(gcmdict["NCPUS"], nx, ny, gcmdict["NUM_MODEL_NODES"], gcmdict["NCPUS_PER_NODE"], num_oserver_nodes)
    else:
        total_pes = nx * ny
    '''

    #run extdata.py
    stage_extdata(
        rc_dir=Path(gcm_dict["rc_dir"]),
        geosbin=gcm_dict["geosbin"],
        turn_off_wsub=@MP_TURN_OFF_WSUB_EXTDATA
    )

    #run emissions.py
    #if amip_emissions:
    stage_amip_emissions(
        rc_dir=Path(gcm_dict['exp_dir']) / "RC",
        scratch_dir=Path(gcm_dict['exp_dir']) / "scratch",
        agcm_lm=gcm_dict['agcm_lm'],
    )

if __name__ == "__main__":
    main()


