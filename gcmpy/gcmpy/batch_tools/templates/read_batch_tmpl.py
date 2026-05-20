
from pathlib import Path

#from gcmpy.utils.yaml_ops import read_templated_yaml
#from gcmpy.utils.yaml_ops import read_yaml_file

script_dir = Path(__file__).parent.resolve()

def create_batch_header(task_type: str,
                        batch_system: str, 
                       tmpl_data: dict) -> str:
    """
    Read a templated file to obtain batch (SLURM, PBS) header
    settings for executing GEOS tasks on various platforms.
    The file has templated variables that are filled
    using parameters passed in the dictionary tmpl_data.

    Parameters
    ----------
    task_type : str
       The task we want to execute.
       It can be: "run", "plot", "post.
    batch_system : str
       The system we plan to use.
       It can take the values: 'slurm', 'pbs'
    tmpl_data : dict
       Contain parameters such as:
       total number of nodes, number of cores/node, etc.

    Returns
    -------
    batch_header : str
       The batch header for running GEOS
    """

    # Templated file.
    in_file = Path(script_dir) / f"gcm_{task_type}_{batch_system}.j.tmpl"

    # Read the file
    with open(in_file, 'r') as fid:
        content = fid.read()

    batch_header = content.format(**tmpl_data)

    return batch_header

if __name__ == "__main__":
    tmpl_data = dict(
            BATCH_TIME="12:00:00",
            NUM_NODES=2,
            NTASKS_PER_NODES=100,
            JOB_NAME="gcm_run",
            NODE_TYPE="cas",
            USER_ACCOUNT="siteam",
            OUTPUT_FILE="myfile.out",
            QUEUE_NAME="batch"
            )

    task_type = "run"
    batch_system = "slurm"
    batch_header = create_batch_header(task_type, batch_system, tmpl_data) 
    print(f"SLURM: \n {batch_header}")
    print()

    batch_system = "pbs"
    batch_header = create_batch_header(task_type, batch_system, tmpl_data) 
    print(f"PBS: \n {batch_header}")
    print()


