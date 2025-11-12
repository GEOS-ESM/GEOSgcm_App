import os, yaml
from copy import deepcopy

# Helper to create a yaml copy of the experiment configurations
def create_exp_yaml(expConfig):
    
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
        if yamlDict[key] == "null":
            yamlDict.pop(key)
            

    # Create and write to YAML file
    file_path = os.path.join(yamlDict['exp_dir'], f"{yamlDict['experiment_id']}.yaml")
    with open(file_path, "w") as f:
        yaml.dump(yamlDict, f, default_flow_style=False, sort_keys=False)


# Helper to create an expConfig from previous experiment
def clone_exp(expConfig, yamlFile):
    # copy yaml into dictionary 
    with open(yamlFile, 'r') as f:
        cloneDict = yaml.safe_load(f)

    # copy old exp answers into expConfig, but keeps the new exp name and description items
    expConfig = {**cloneDict, **expConfig}

    return expConfig

