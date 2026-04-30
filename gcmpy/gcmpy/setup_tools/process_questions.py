import yaml, re, os, subprocess

from gcmpy.setup_tools.generate_question import Question
from gcmpy.utils.color_ops import Color
from gcmpy.setup_tools.setup_envs import envdict
from gcmpy.setup_tools.config.load_questions import load_yamls
from gcmpy.geos_settings.horizontal_resolution import (
        atmos_horizontal_res,
        mit_horizontal_res,
        heartbeat_dict,
        ocean_res_default_dict
        )
from gcmpy.batch_tools.computing_sites import site_nodes_dict

class Handle:
    '''
    This class deals with special cases where a question"s 
    properties need to be checked/dynamically changed at
    runtime based on certain conditions (e.g. input validation)
    '''

    @staticmethod
    def select_type(question_dict, i):
        if question_dict[i].q_type == "select" and question_dict[i].answer != None:
            # as of right now, we only want the first word of every select-type question.
            # If that changes it's probably best to delete this function.
            question_dict[i].answer = question_dict[i].answer.split(None, 1)[0]


    @staticmethod
    def io_server_default(question_dict, i):
        if i != 'io_server':
            return
        AM_horizontal_res = question_dict['AM_horizontal_res'].answer
        if AM_horizontal_res in atmos_horizontal_res:
            question_dict[i].default = True
        else:
            question_dict[i].default = False

    @staticmethod
    def AM_microphysics_default(question_dict, i):
        if i != 'AM_microphysics':
            return

        # The default ocean resolution is based on the atmospheric resolution
        match question_dict['AM_vertical_res'].answer:
            case '72':
                question_dict[i].choices = ['BACM_1M  --  3-phase 1-moment Bacmeister et al', \
                                           'GFDL_1M  --  6-phase 1-moment Geophysical Fluid Dynamics Laboratory', \
                                           'MGB2_2M  --  5 or 6-phase 2-moment Morrison & Gettleman']
            case _:
                question_dict[i].choices = ['GFDL_1M  --  6-phase 1-moment Geophysical Fluid Dynamics Laboratory', \
                                           'BACM_1M  --  3-phase 1-moment Bacmeister et al', \
                                           'MGB2_2M  --  5 or 6-phase 2-moment Morrison & Gettleman']


    @staticmethod
    def processor_choices(question_dict, i):
        if i != "processor":
            return

        my_site = envdict['site']
        if my_site in site_nodes_dict:
            question_dict[i].choices = list(site_nodes_dict[my_site].keys())
        else:
            exit(1)

    @staticmethod                           
    def group_id_choices(question_dict, i):  
        if i != "group_root":               
            return                          
            
        if envdict['site'] in site_nodes_dict:
            question_dict[i].choices = get_all_group_ids()
        else:
            exit(1)

    @staticmethod
    def OM_horizontal_res_default(question_dict, i):
        if i != 'OM_horizontal_res':
            return

        # The default ocean resolution is based on the atmospheric resolution
        horizontal_res = question_dict['AM_horizontal_res'].answer
        question_dict[i].choices = ocean_res_default_dict[horizontal_res]

    @staticmethod
    def LS_boundary_conditions_default(question_dict, i):
        if i != 'LS_boundary_conditions':
            return

        # The default ocean resolution is based on the atmospheric resolution
        match question_dict['AM_vertical_res'].answer:
            case '72':
                question_dict[i].choices = ['NL3', 'v12', 'ICA']
            case _:
                question_dict[i].choices = ['v12', 'NL3', 'ICA']

    @staticmethod
    def MIT_hres_choices(question_dict, i):
        if i != "OM_MIT_horizontal_res":
            return
        horizontal_res = question_dict["AM_horizontal_res"].answer
        if horizontal_res in ["c720", "c1440"]:
            question_dict[i].choices = mit_horizontal_res[horizontal_res]

    @staticmethod
    def MOM_hres_default(question_dict, i):
        if i != "OM_MOM_horizontal_res":
            return
        if question_dict["OM_name"].answer == "MOM6" and question_dict["AM_horizontal_res"].answer == "c12":
           question_dict[i].default = "72 36"

    @staticmethod
    def seaice_choices(question_dict, i):
        if i == 'OM_seaice_model' and question_dict['OM_name'] == 'MOM6':
            question_dict[i].choices = ['CICE6']


    @staticmethod
    def heartbeat_default(question_dict, i):
        if i != "heartbeat":
            return
        '''
        Default heartbeat is determined by atmospheric resolution.
        Of course, this just the recommended value. The user can
        enter whatever value they like
        '''
        heartbeat = ""
        horizontal_res = question_dict["AM_horizontal_res"].answer
        heartbeat = heartbeat_dict[horizontal_res]

        # Per W. Putman recommendation, set heartbeat to 450s anytime BACM_1M is selected
        if question_dict["AM_microphysics"].answer == "BACM_1M":
            heartbeat = 450

        # ((IMPORTANT: default must be type string due to some limitation in questionary))
        question_dict[i].default = str(heartbeat)


    @staticmethod
    def heartbeat_valid(question_dict, i):
        if i != "heartbeat":
            return
        # input validation using regex
        while not re.match(r"^\d+$", question_dict[i].answer):
            print(f"{Color.RED}please enter exactly 1 number!{Color.RESET}")
            question_dict[i].load_question(question_dict)

    @staticmethod
    def history_template_default(question_dict, i):
        if i != "history_template":
            return

        if question_dict['OM_name'].answer == 'MOM5':
            question_dict[i].default = 'HISTORY.AOGCM-MOM5.rc.tmpl'
        elif question_dict['OM_name'].answer == "MOM6":
            question_dict[i].default = 'HISTORY.AOGCM.rc.tmpl'
        elif question_dict['OM_name'].answer == 'MIT':
            question_dict[i].default = 'HISTORY.AOGCM_MITgcm.rc.tmpl'
        elif question_dict['OM_data_atmos'].answer == True:
            question_dict[i].default == 'HISTORY.DATAATM.rc.tmpl'
        else:
            question_dict[i].default = 'HISTORY.AGCM.rc.tmpl'

    @staticmethod
    def exp_dir_default(question_dict, i):
        if i != "exp_dir":
            return

        root = f"{os.environ.get('HOME')}/.EXPDIRroot"
        if os.path.exists(root):
            try:
                with open(root, "r") as file:
                    question_dict[i].default = f"{file.read().strip()}/{question_dict['experiment_id'].answer}"
            except Exception as e:
                print(f"An error occurred while reading {Color.BLUE}.EXPDIRroot{Color.RESET}: {str(e)}")
        elif envdict['site'] in site_nodes_dict:
            question_dict[i].default = f"/{'discover/' if envdict['site'] == 'NCCS' else ''}nobackup/{os.environ.get('LOGNAME')}/{question_dict['experiment_id'].answer}"
        else:
            question_dict[i].default = f"{os.environ.get('HOME')}/{question_dict['experiment_id']}"


    @staticmethod
    def exp_dir_valid(question_dict, i):
        if i != "exp_dir":
            return
        while os.path.basename(question_dict[i].answer) != question_dict['experiment_id'].answer:
            print(f"{Color.RED}This directory MUST point to the experiment ID: {Color.BLUE}{question_dict['experiment_id'].answer}{Color.RED}!{Color.RESET}")
            question_dict[i].load_question(question_dict)


    @staticmethod
    def group_root_default(question_dict, i):
        if i != "group_root":
            return
        groups_output = subprocess.check_output(["groups"], text=True)
        group_root = groups_output.strip().split()[0]
        question_dict[i].default = group_root

def get_all_group_ids() -> list:
    """
    Run the Linux groups command to create the list
    of all the user's group ids.
 
    Returns
    -------
    group_ids : list
      The list of all the user's group ids.
    """
    # Use the subprocess module to get all the possible group ids
    groups_output = subprocess.check_output(["groups"], text=True)
    groups_output = groups_output.strip().split()
 
    # Only select ids which second to fourth characters are integers
    # Some of the ids are purely characters and should not be considered.
    group_ids = [item for item in groups_output if item[1:4].isdigit()]
 
    return group_ids


def ask_questions():
    # actual driver for questionary questions
    question_dict = {}
    yaml_questions = load_yamls()

    # creates a dictionary of question:answer pairs
    for i in yaml_questions:
        temp = Question(i, yaml_questions[i]["type"],          \
                           yaml_questions[i]["prompt"],        \
                           yaml_questions[i]["choices"],       \
                           yaml_questions[i]["default_answer"],\
                           yaml_questions[i]["follows_up"])

        question_dict[i] = temp


        # if the question properties need to dynamically change at
        # runtime call Handle method BEFORE load_question()
        Handle.AM_microphysics_default(question_dict, i)
        Handle.io_server_default(question_dict, i)
        # Only ask processor question if on NAS or NCCS
        if i == "processor" and envdict['site'] not in site_nodes_dict:
            question_dict[i].answer = None
            continue
        else:
            Handle.processor_choices(question_dict, i)
        Handle.OM_horizontal_res_default(question_dict, i)
        Handle.MIT_hres_choices(question_dict, i)
        Handle.MOM_hres_default(question_dict, i)
        Handle.seaice_choices(question_dict, i)
        Handle.LS_boundary_conditions_default(question_dict, i)
        Handle.heartbeat_default(question_dict, i)
        Handle.history_template_default(question_dict, i)
        Handle.exp_dir_default(question_dict, i)
        Handle.group_id_choices(question_dict, i)
        #Handle.group_root_default(question_dict, i)

        # prompts the user with the question
        question_dict[i].load_question(question_dict)

        # input validation and other post processing goes here,
        # AFTER load_question() call
        # Handle.experiment_desc(question_dict, i)
        Handle.heartbeat_valid(question_dict, i)
        Handle.exp_dir_valid(question_dict, i)

        # strips the first word from every select type question
        Handle.select_type(question_dict, i)

        try:
            if question_dict['clone_experiment'].answer == True and i == 'original_exp_path':
                break
        except KeyError:
            pass


    # creates a dictionary that only contains user answers (for simplicity)
    expConfig = {key: question_dict.answer for key, question_dict in question_dict.items()}
    del question_dict

    return expConfig

