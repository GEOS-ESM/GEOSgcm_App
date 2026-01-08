import os, yaml, sys, subprocess, re, shutil
from copy import deepcopy
from utility import color, cpfile

# Helper to create a yaml copy of the experiment configurations
def yaml_receipt(expConfig):
    
    # we don't care about these anymore
    try:
        del expConfig['clone_experiment']
        del expConfig['original_exp_path']
    except KeyError:
        pass

    # create a deep copy of the dictionary so the original isn't affected
    yamlDict = deepcopy(expConfig)

    # if the question was never asked remove it from the dictionary before creating the yaml
    # delete if we want to give access to all questions in the yaml
    for key in list(yamlDict):
        if yamlDict[key] == "None":
            yamlDict.pop(key)
            

    # Create and write to YAML file
    file_path = os.path.join(yamlDict['exp_dir'], f"{yamlDict['experiment_id']}.yaml")
    with open(file_path, "w") as f:
        yaml.dump(yamlDict, f, default_flow_style=False, sort_keys=False)


# Helper to create an expConfig from yaml file input
def yaml_input_exp(yamlFile):
    expConfig = {}

    # copy yaml into dictionary 
    with open(yamlFile, 'r') as f:
        expConfig = yaml.safe_load(f)

    return expConfig


def clone_exp(expConfig):
   
    # we will peek inside HISTORY.rc to find the original exp id/desc
    history_path = f"{expConfig['original_exp_path']}/HISTORY.rc"

    # these are dictionaries containing only the old and new values 
    exp_path = {
        "old": expConfig['original_exp_path'],
        "new": expConfig['exp_dir']
    }
    exp_id = {
        "old": get_old_value(history_path, "EXPID:"),
        "new": expConfig['experiment_id']
    }
    exp_desc = {
        "old": get_old_value(history_path, "EXPDSC:"),
        "new": expConfig['experiment_description']
    }

    # find user's group root
    groups_output = subprocess.check_output(["groups"], text=True)
    expConfig['group_root'] = groups_output.strip().split()[0]

    # Find out if we are running the cube and/or OGCM
    ogcm = None
    seaice = None
    with open(f"{exp_path['old']}/AGCM.rc", "r") as f:
        for line in f:
            if "OCEAN_NAME" in line:
                ogcm = line.split(':', 1)[1].strip()
            if "SEAICE_NAME" in line:
                seaice = line.split(':', 1)[1].strip()
    
    # Construct the list of files to be copied over
    file_list = ["post/gcm_post.j",
                 "plot/gcm_plot.tmpl",
                 "plot/gcm_quickplot.csh",
                 "plot/gcm_moveplot.j",
                 "archive/gcm_archive.j",
                 "regress/gcm_regress.j",
                 "forecasts/gcm_forecast.tmpl",
                 "forecasts/gcm_forecast.setup",
                 "plot/plot.rc",
                 "post/post.rc",
                 "linkbcs",
                 "CAP.rc",
                 "AGCM.rc",
                 "HISTORY.rc",
                 "gcm_run.j",
                 "gcm_emip.setup",
                 "logging.yaml",
                 "fvcore_layout.rc",
                 "GEOSgcm.x"]
    
    if ogcm and ogcm != "MIT":
        file_list.extend(["input.nml",
                          "diag_table",
                          "__init__.py",
                          "plot/plotocn.j"])
    if ogcm == "MOM":
        file_list.append("field_table")
    elif ogcm == "MOM6":
        file_list.extend(["MOM_override",
                          "MOM_input",
                          "data_table"])
    if seaice == "CICE6":
        file_list.append("ice_in")

    # Create the new directory and copy over the files
    os.mkdir(exp_path['new'])
    sub_dirs = ['archive', 'forecasts', 'plot', 'post' , 'regress']
    for i in sub_dirs:
        os.makedirs(os.path.join(exp_path['new'], i), exist_ok=True)
    for file in file_list:
        cpfile(f"{exp_path['old']}/{file}", f"{exp_path['new']}/{file}", file)

    # Copy the RC dir over
    shutil.copytree(f"{exp_path['old']}/RC", f"{exp_path['new']}/RC", symlinks=True)
   
    # we do NOT want to attempt regex substitutions on a binary file
    file_list.remove("GEOSgcm.x")

    # performs regex subsitions on every file in file_list
    for file in file_list:
        with open(f"{exp_path['new']}/{file}", 'r') as f:
            lines = f.readlines()

        new_lines = [replace(line, exp_id, exp_desc, exp_path, expConfig['group_root']) for line in lines]

        with open(f"{exp_path['new']}/{file}", 'w') as f:
            f.writelines(new_lines)

    print("Cloning Complete!\n"
          "-----------------------\n\n"
          f"Original Experiment Directory: {color.RED}{exp_path['old']}{color.END}\n"
          "-----------------------\n\n"
          f"You must now copy your {color.GREEN}Initial Conditions{color.END} into:\n"
          "-----------------------\n"
          f"{color.RED}{exp_path['new']}{color.END}\n\n")
    

# helper function for regex substitutions
def replace(line, exp_id, exp_desc, exp_path, group_root):

    # basically a rulebook for what lines are allowed to have substitutions performed on them
    rules = [
        (rf"(^\s*EXPID:\s+){re.escape(exp_id['old'])}", rf"\1{exp_id['new']}"),
        (rf"(setenv\s+EXPID\s+){re.escape(exp_id['old'])}", rf"\1{exp_id['new']}"),
        (rf"(set\s+EXPID=){re.escape(exp_id['old'])}", rf"\1{exp_id['new']}"),
        (rf"(\s*(?:exp_id|cmpexp)=(?:\"|\')){re.escape(exp_id['old'])}", rf"\1{exp_id['new']}"),
        (rf"(^\s*(?:#PBS -N\s+|#SBATCH\s+--job-name=)){re.escape(exp_id['old'])}", rf"\1{exp_id['new']}"),
        (rf"(EXPDSC:\s*){re.escape(exp_desc['old'])}", rf"\1{exp_desc['new']}"),
        (rf"{exp_path['old']}", rf"{exp_path['new']}"),
        (rf"((?:group_list|#SBATCH\s+--account)=)(.*)", rf"\1{group_root}"),
    ]

    for pattern, repl in rules:
        line = re.sub(pattern, repl, line)
    return line

# helper function for finding original experiment id/desc
def get_old_value(filepath, old_key):
    with open(filepath) as f:
        for line in f:
            m = re.match(rf"^\s*{old_key}\s*(.*$)", line)
            if m:
                return m.group(1)
    return sys.exit(f"Could not find {old_key} in {filepath}")


