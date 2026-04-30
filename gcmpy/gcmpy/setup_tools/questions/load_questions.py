
from pathlib import Path

from gcmpy.utils.color_ops import Color
from gcmpy.utils.yaml_ops import read_yaml_file
from gcmpy.utils.path_ops import get_current_dir

script_dir = Path(__file__).parent.resolve()

def load_yamls() -> dict:
    """
    Read a collection of YAML files and combine their
    their contents into a dictionary.
    Each YAML file contains settings for the questions
    to be asked during the setup process.
    """

    # List of question files 
    # (*MAKE SURE THEY ARE IN THE ORDER YOU WANT THEM TO BE ASKED*)
    #file_list = glob.glob("*.yaml")
    file_list = [
            "exp_setup.yaml",
            "atmospheric_model.yaml",
            "ocean_model.yaml",
            "land_model.yaml", 
            "gocart.yaml",    
            "directory_setup.yaml"
            ]

    all_yaml_questions = dict()

    for filename in file_list:
        filename = Path(script_dir) / filename
        yaml_questions = read_yaml_file(filename)
        all_yaml_questions.update(yaml_questions)

    return all_yaml_questions


