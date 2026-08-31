import yaml, re, os, subprocess, sys

from gcmpy.setup_tools.generate_question import Question
from gcmpy.utils.color_ops import Color
from gcmpy.setup_tools.setup_envs import envdict
from gcmpy.setup_tools.questions.load_questions import load_yamls
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
        AGCM_grid = question_dict['AGCM_grid'].answer
        if AGCM_grid in atmos_horizontal_res:
            question_dict[i].default = True
        else:
            question_dict[i].default = False

    @staticmethod
    def MOIST_version_default(question_dict, i):
        if i != 'MOIST_version':
            return

        if question_dict['AGCM_LM'].answer == '72':
            question_dict[i].choices = ['BACM_1M  --  3-phase 1-moment Bacmeister et al', \
                                        'GFDL_1M  --  6-phase 1-moment Geophysical Fluid Dynamics Laboratory', \
                                        'MGB2_2M  --  5 or 6-phase 2-moment Morrison & Gettleman']
        else:
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
    def DATAOCEAN_resolution_default(question_dict, i):
        if i != 'DATAOCEAN_resolution':
            return

        # The default ocean resolution is based on the atmospheric resolution
        horizontal_res = question_dict['AGCM_grid'].answer
        question_dict[i].choices = ocean_res_default_dict[horizontal_res]

    @staticmethod
    def LAND_bcs_default(question_dict, i):
        if i != 'LAND_bcs':
            return

        # top of the list is the default answer choice
        if question_dict['AGCM_LM'].answer == '72':
            question_dict[i].choices = ['NL3', 'v12', 'ICA']
        else:
            question_dict[i].choices = ['v12', 'NL3', 'ICA']

    @staticmethod
    def OCEAN_type_valid(question_dict, i):
        if i != 'OCEAN_type':
            return

        if question_dict['OCEAN_type'].answer == 'MOM6' and question_dict['AGCM_grid'].answer not in ["c12", "c90", "c180", "c360"]:
            sys.exit("""# For MOM6 we currently have only 4 allowed ocean resolutions (1/2, 1/4, 1/8 deg)
                        # based on the atmospheric resolution (c180 and c360). The allowed are:
                        #
                        # Atm Res  Atm NXxNY  Atm IMxJM  Ocean NXxNY  Ocean IMxJM  Ocean LM
                        # -------  ---------  ---------  -----------  -----------  --------
                        # c12      1x6        12x72      3x2          72x36        50
                        # c90      5x36       90x540     90x2         540x458      50
                        # c180     30x36      180x1080   36x30        1440x1080    75
                        # c180     24x36      180x1080   36x24        720x576      75
                        # c360     30x36      180x1080   36x30        1440x1080    75
                        # c180     20x36      180x1080   36x20        2880x2240    75
                        # See https://github.com/GEOS-ESM/GEOSgcm/wiki/Coupled-model-configurations-(GEOS-MOM6)
                        
                        Please select a valid Atmospheric resolution!""")
        elif question_dict['OCEAN_type'].answer == 'MIT' and question_dict['AGCM_grid'].answer != "c90":
            sys.exit("You MUST select c90 atmospheric resolution for MIT ocean!")
    
    @staticmethod
    def MOM6_resolution_choices(question_dict, i):
        if i != 'MOM6_resolution':
            return
        if question_dict['AGCM_grid'].answer == "c12":
            question_dict['MOM_resolution'].choices = ['o72']
        elif question_dict['AGCM_grid'].answer == "c90":
            question_dict['MOM_resolution'].choices = ['o540']
        elif question_dict['AGCM_grid'].answer == "c180":
            question_dict['MOM_resolution'].choices = ['o720 (1/2-deg,  720x576  Tripolar)', 
                                                       'o1440 (1/4-deg, 1440x1080  Tripolar)', 
                                                       'o2880 (1/8-deg, 2880x2240 Tripolar)']
        elif question_dict['AGCM_grid'].answer == "c360":
            question_dict['MOM_resolution'].choices = ['o1440 (1/4-deg, 1440x1080  Tripolar)']

        question_dict["MOM_resolution"].choices = choices 

    @staticmethod
    def seaice_choices(question_dict, i):
        if i == 'SEAICE_model' and question_dict['OCEAN_type'] == 'MOM6':
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
        horizontal_res = question_dict["AGCM_grid"].answer
        heartbeat = heartbeat_dict[horizontal_res]

        # Per W. Putman recommendation, set heartbeat to 450s anytime BACM_1M is selected
        if question_dict["MOIST_version"].answer == "BACM_1M":
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

        if question_dict['OCEAN_type'].answer == 'MOM5':
            question_dict[i].default = 'HISTORY.AOGCM-MOM5.rc.tmpl'
        elif question_dict['OCEAN_type'].answer == "MOM6":
            question_dict[i].default = 'HISTORY.AOGCM.rc.tmpl'
        elif question_dict['OCEAN_type'].answer == 'MIT':
            question_dict[i].default = 'HISTORY.AOGCM_MITgcm.rc.tmpl'
        elif question_dict['OCEAN_data_atmosphere'].answer == True:
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
        Handle.MOIST_version_default(question_dict, i)
        Handle.io_server_default(question_dict, i)
        # Only ask processor question if on NAS or NCCS
        if i == "processor" and envdict['site'] not in site_nodes_dict:
            question_dict[i].answer = None
            continue
        else:
            Handle.processor_choices(question_dict, i)
        Handle.DATAOCEAN_resolution_default(question_dict, i)
        Handle.MOM6_resolution_choices(question_dict, i)
        Handle.seaice_choices(question_dict, i)
        Handle.LAND_bcs_default(question_dict, i)
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
        Handle.OCEAN_type_valid(question_dict, i)
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

