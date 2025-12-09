import os, yaml
from copy import deepcopy

# Helper to create a yaml copy of the experiment configurations
def yaml_receipt(expConfig):
    
    # we don't care about these anymore
    try:
        del expConfig['clone_experiment']
        del expConfig['yaml_clone_path']
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

