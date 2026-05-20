
from pathlib import Path

from gcmpy.utils.yaml_ops import read_templated_yaml
from gcmpy.utils.yaml_ops import read_yaml_file

script_dir = Path(__file__).parent.resolve()

def get_site_nodes() -> dict:
    """
    Read a YAML file to obtain the list of available
    types of node at various computing sites.
    For each type of node, the file provides the number
    of cores on the node.

    Results
    -------
    site_nodes_dict : dict
       A dictionary which main keys are the site names
       and the values are the available node types and
       number of cores on the node.
    """
    yaml_file = Path(script_dir) / f"site_nodes.yaml"
    site_nodes_dict = read_yaml_file(yaml_file)

    return site_nodes_dict

def get_batch_header(batch_system: str, tmpl_data: dict) -> dict:
    """
    Read a YAML file to obtain batch (SLURM, PBS) header
    settings for GEOS on various platforms.
    The YAML has templated variables that are filled
    using parameters passed in the dictionary tmpl_data.

    Parameters
    ----------
    batch_system : str
       The system we plan to use.
       It can take the values: 'slurm', 'pbs'
    tmpl_data : dict
       Contain parameters such as:
       total number of nodes, number of cores/node, etc.

    Results
    -------
    batch_header : dict
       A dictionary containing batch header settings.
    """

    # Templated YAML file.
    yaml_file = Path(script_dir) / f"{batch_system}_catalog.yaml"

    # Read the YAML file to obtain a dictionary
    batch_header = read_templated_yaml(yaml_file, tmpl_data)

    return batch_header

def get_computing_site_settings(site: str, tmpl_data: dict) -> dict:
    """
    """

    yaml_file = Path(script_dir) / f"settings_{site}.yaml"

    site_settings = read_templated_yaml(yaml_file, tmpl_data)

    return site_settings

    

"""
sites_list = ['NCCS', 'NAS']

site_nodes_dict = {
        'NCCS': dict(mil=40, cas=120),
        'NAS': dict(bro=24, sky=40, cas=40, rom=120, mil=120, tur=240)
        }
#site_nodes_dict = {
#        'NCCS': ['mil', 'cas'],
#        'NAS': ["rom", "mil", "sky", "cas", "bro", "tur"]
#        }
"""

"""
  As far as possible, define the following parameters:

     batch_cmd               - PBS Batch command
     batch_group             - PBS Syntax for GROUP
     batch_time              - PBS Syntax for walltime
     batch_jobname           - PBS Syntax for job name
     batch_outputname        - PBS Syntax for job output name
     batch_joinouterr        - PBS Syntax for joining output and error
     run_ft                  - Wallclock Time for gcm_forecast.j
     run_ft                  - Wallclock Time for gcm_run.j
     post_t                  - Wallclock Time for gcm_post.j
     plot_t                  - Wallclock Time for gcm_plot.j
     archive_t               - Wallclock Time for gcm_archive.j
     run_q                   - Batch queue name for gcm_run.j
     run_p                   - PE Configuration for gcm_run.j
     run_fp                  - PE Configuration for gcm_forecast.j
     regress_p               - PE Configuration for gcm_regress.j
     post_q                  - Batch queue name for gcm_post.j
     plot_q                  - Batch queue name for gcm_plot.j
     move_q                  - Batch queue name for gcm_moveplot.j
     archive_q               - Batch queue name for gcm_archive.j
     move_p                  - PE Configuration for gcm_moveplot.j
"""

"""
job_script_header = dict()
job_script_header['NAS'] = dict(
        batch_cmd = "qsub",
        batch_group = "PBS -W group_list=",
        batch_time = "PBS -l walltime=",
        batch_jobname = "PBS -N",
        batch_outputname = "PBS -o ",
        batch_joinouterr = "PBS -j oe -k oed",
        run_ft = "6:00:00",
        run_t = "8:00:00",
        post_t = "8:00:00",
        plot_t = "8:00:00",
        archive_t = "8:00:00",
        run_q = f"PBS -q normal",
        post_q = f"PBS -q normal",
        plot_q = f"PBS -q normal",
        move_q = f"PBS -q normal",
        archive_q = f"PBS -q normal",
        move_p = "PBS -l select=1:ncpus=1"
        )

job_script_header['NCCS'] = dict(
        batch_cmd  = "sbatch",
        batch_group = "SBATCH --account=",
        batch_time = "SBATCH --time=",
        batch_jobname = "SBATCH --job-name=",
        batch_outputname = "SBATCH --output=",
        batch_joinouterr = "DELETE",
        run_ft = "06:00:00",
        run_t  = "12:00:00",
        post_t = "8:00:00",
        plot_t = "12:00:00",
        archive_t = "2:00:00",
        move_q = "SBATCH --partition=datamove",
        archive_q = "SBATCH --partition=datamove",
        plot_p = f"SBATCH --nodes=4 --ntasks=4",
        archive_p = "SBATCH --ntasks=1",
        move_p = "SBATCH --ntasks=1"
        )

job_script_header['AWS'] = dict(
        batch_cmd        = "sbatch", 
        batch_group      = "#DELETE", 
        batch_time       = "SBATCH --time=", 
        batch_jobname    = "SBATCH --job-name=", 
        batch_outputname = "SBATCH --output=", 
        batch_joinouterr = "DELETE", 
        run_ft           = "06:00:00", 
        run_t            = "12:00:00", 
        post_t           = "8:00:00", 
        plot_t           = "12:00:00", 
        archive_t        = "1:00:00", 
        post_q           = "NULL", 
        plot_q           = "NULL", 
        move_q           = "NULL", 
        archive_q        = "NULL", 
        plot_p           = f"SBATCH --nodes=4 --ntasks=4", 
        archive_p        = "SBATCH --ntasks=1", 
        move_p           = "SBATCH --ntasks=1"
        )

job_script_header['Azure'] = job_script_header['AWS'] 

job_script_header['Other'] = dict(
        batch_cmd = "sbatch",
        batch_group = "SBATCH --account=",
        batch_time = "SBATCH --time=",
        batch_jobname = "SBATCH --job-name=",
        batch_outputname = "SBATCH --output=",
        batch_joinouterr = "DELETE",
        run_ft = "06:00:00",
        run_t  = "12:00:00",
        post_t = "8:00:00",
        plot_t = "12:00:00",
        archive_t = "1:00:00",
        run_q = "NULL",
        run_p = "NULL",
        run_fp = "NULL",
        regress_p = "NULL",
        post_q = "NULL",
        plot_q = "NULL",
        move_q = "NULL",
        archive_q = "NULL",
        post_p = "NULL",
        plot_p = "NULL",
        archive_p = "NULL",
        move_p = "NULL"
        )

"""




