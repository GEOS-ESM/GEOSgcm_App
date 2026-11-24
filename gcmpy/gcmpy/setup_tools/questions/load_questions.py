
from pathlib import Path

from gcmpy.utils.color_ops import Color
from gcmpy.utils.yaml_ops import read_yaml_file
from gcmpy.utils.path_ops import get_current_dir
from gcmpy.components.base import discover_components, insert_question_blocks

script_dir = Path(__file__).parent.resolve()

def load_yamls() -> dict:
    """
    Read a collection of YAML files and combine their
    their contents into a dictionary.
    Each YAML file contains settings for the questions
    to be asked during the setup process.

    Components discovered under gcmpy.components (see
    gcmpy.components.base) contribute their own question blocks, spliced
    in after whatever question defines the expConfig key(s) they
    `requires`, or before directory_setup if they require nothing.
    """

    # Core question files, in the order they are asked.
    # (*MAKE SURE THEY ARE IN THE ORDER YOU WANT THEM TO BE ASKED*)
    core_file_list = [
            "exp_setup.yaml",
            "atmospheric_model.yaml",
            "ocean_model.yaml",
            "land_model.yaml",
            "directory_setup.yaml",
            ]

    core_blocks = [
            (filename, read_yaml_file(Path(script_dir) / filename))
            for filename in core_file_list
            ]

    components = discover_components()
    blocks = insert_question_blocks(core_blocks, components)

    all_yaml_questions = dict()
    for _, yaml_questions in blocks:
        all_yaml_questions.update(yaml_questions)

    return all_yaml_questions
