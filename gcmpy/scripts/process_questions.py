import yaml, re, os, subprocess
from generate_question import question
from utility import envdict, pathdict, color, load_yamls

'''
This class handles special cases where a question"s properties need to be checked/dynamically
changed at runtime based on certain conditions (e.g. input validation)
'''
class handle:

    @staticmethod
    def select_type(questionDict, i):
        if questionDict[i].q_type == "select" and questionDict[i].answer != None:
            # as of right now, we only want the first word of every select-type question.
            # If that changes it's probably best to delete this function.
            questionDict[i].answer = questionDict[i].answer.split(None, 1)[0]


    @staticmethod
    def io_server_defualt(questionDict, i):
        if i != 'io_server':
            return
        match questionDict['AM_horizontal_res'].answer:
            case 'c180' | 'c360' | 'c720' | 'c1120' | 'c1440' | 'c2880' | 'c5760' | 'c270' | 'c540' | 'c1080' | 'c1536' | 'c2160':
                questionDict[i].default = True
            case _:
                questionDict[i].default = False


    @staticmethod
    def processor_choices(questionDict, i):
        if i != "processor":
            return

        if envdict['site'] == 'NCCS':
            questionDict[i].choices = ['mil', 'cas']
        elif envdict['site'] == 'NAS':
            print(color.GREEN + "NOTE Due to how FV3 is compiled by default, Sandy Bridge\n" + \
                                "and Ivy Bridge are not supported by current GEOS" + color.RESET)

            questionDict[i].choices = ["rom", "mil", "sky", "cas", "bro", "has"]
        else:
            exit(1)

    @staticmethod
    def OM_horizontal_res_default(questionDict, i):
        if i != 'OM_horizontal_res':
            return

        # The default ocean resolution is based on the atmospheric resolution
        match questionDict['AM_horizontal_res'].answer:
            case 'c12' | 'c24' | 'c48':
                questionDict[i].choices = ['o2 (1/4-deg, 1440x720  MERRA-2)',  \
                                           'o3 (1/8-deg, 2880x1440 OSTIA)',    \
                                           'o1 (1  -deg,  360x180  Reynolds, ends in 2022)']
            case _:
                questionDict[i].choices = ['CS (Cubed-Sphere OSTIA)', \
                                           'o2 (1/4-deg, 1440x720  MERRA-2)', \
                                           'o3 (1/8-deg, 2880x1440 OSTIA)',   \
                                           'o1 (1  -deg,  360x180  Reynolds, ends in 2022)']


    @staticmethod
    def MIT_hres_choices(questionDict, i):
        if i != "OM_MIT_horizontal_res":
            return
        if questionDict["AM_horizontal_res"].answer == "c720":
            questionDict[i].choices = ["llc1080 (1/12-deg, Lat-Lon-Cube)"]
        elif questionDict["AM_horizontal_res"].answer == "c1440":
            questionDict[i].choices = ["llc2160 (1/24-deg, Lat-Lon-Cube)"]

    @staticmethod
    def MOM_hres_default(questionDict, i):
        if i != "OM_MOM_horizontal_res":
            return
        if questionDict["OM_name"].answer == "MOM6" and questionDict["AM_horizontal_res"].answer == "c12":
           questionDict[i].default = "72 36"

    @staticmethod
    def seaice_choices(questionDict, i):
        if i == 'OM_seaice_model' and questionDict['OM_name'] == 'MOM6':
            questionDict[i].choices = ['CICE6']


    @staticmethod
    def heartbeat_default(questionDict, i):
        if i != "heartbeat":
            return
        '''
        Default heartbeat is determined by atmospheric resolution.
        Of course, this just the recommended value. The user can
        enter whatever value they like
        '''
        heartbeat = ""
        match questionDict["AM_horizontal_res"].answer:
            case 'c12':
                heartbeat = 3600
            case 'c24':
                heartbeat = 1800
            case 'c48':
                heartbeat = 1200
            case 'c90':
                heartbeat = 900
            case 'c180' | 'c270':
                heartbeat = 600
            case 'c360':
                heartbeat = 450
            case 'c720' | 'c1120' | 'c540':
                heartbeat = 300
            case 'c1440' | 'c1080':
                heartbeat = 150
            case 'c2880' | 'c5760' | 'c1536' | 'c2160' | 'c4320':
                heartbeat = 75


        # Per W. Putman recommendation, set heartbeat to 450s anytime BACM_1M is selected
        if questionDict["AM_microphysics"].answer == "BACM_1M":
            heartbeat = 450

        # ((IMPORTANT: default must be type string due to some limitation in questionary))
        questionDict[i].default = str(heartbeat)


    @staticmethod
    def heartbeat_valid(questionDict, i):
        if i != "heartbeat":
            return
        # input validation using regex
        while not re.match(r"^\d+$", questionDict[i].answer):
            print(f"{color.RED}please enter exactly 1 number!{color.RESET}")
            questionDict[i].load_question(questionDict)

    @staticmethod
    def history_template_default(questionDict, i):
        if i != "history_template":
            return

        if questionDict['OM_name'].answer == 'MOM5':
            questionDict[i].default = 'HISTORY.AOGCM-MOM5.rc.tmpl'
        elif questionDict['OM_name'].answer == "MOM6":
            questionDict[i].default = 'HISTORY.AOGCM.rc.tmpl'
        elif questionDict['OM_name'].answer == 'MIT':
            questionDict[i].default = 'HISTORY.AOGCM_MITgcm.rc.tmpl'
        elif questionDict['OM_data_atmos'].answer == True:
            questionDict[i].default == 'HISTORY.DATAATM.rc.tmpl'
        else:
            questionDict[i].default = 'HISTORY.AGCM.rc.tmpl'

    @staticmethod
    def exp_dir_default(questionDict, i):
        if i != "exp_dir":
            return

        root = f"{os.environ.get('HOME')}/.EXPDIRroot"
        if os.path.exists(root):
            try:
                with open(root, "r") as file:
                    questionDict[i].default = f"{file.read().strip()}/{questionDict['experiment_id'].answer}"
            except Exception as e:
                print(f"An error occurred while reading {color.BLUE}.EXPDIRroot{color.RESET}: {str(e)}")
        elif envdict['site'] in ['NAS','NCCS']:
            questionDict[i].default = f"/{'discover/' if envdict['site'] == 'NCCS' else ''}nobackup/{os.environ.get('LOGNAME')}/{questionDict['experiment_id'].answer}"
        else:
            questionDict[i].default = f"{os.environ.get('HOME')}/{questionDict['experiment_id']}"


    @staticmethod
    def exp_dir_valid(questionDict, i):
        if i != "exp_dir":
            return
        while os.path.basename(questionDict[i].answer) != questionDict['experiment_id'].answer:
            print(f"{color.RED}This directory MUST point to the experiment ID: {color.BLUE}{questionDict['experiment_id'].answer}{color.RED}!{color.RESET}")
            questionDict[i].load_question(questionDict)


    @staticmethod
    def group_root_default(questionDict, i):
        if i != "group_root":
            return
        groups_output = subprocess.check_output(["groups"], text=True)
        group_root = groups_output.strip().split()[0]
        questionDict[i].default = group_root




def ask_questions():
    # actual driver for questionary questions
    questionDict = {}
    yaml_questions = load_yamls()

    # creates a dictionary of question:answer pairs
    for i in yaml_questions:
        temp = question(i, yaml_questions[i]["type"],          \
                           yaml_questions[i]["prompt"],        \
                           yaml_questions[i]["choices"],       \
                           yaml_questions[i]["default_answer"],\
                           yaml_questions[i]["follows_up"])

        questionDict[i] = temp


        # if the question properties need to dynamically change at
        # runtime call handle function BEFORE load_question()
        handle.io_server_defualt(questionDict, i)
        handle.processor_choices(questionDict, i)
        handle.OM_horizontal_res_default(questionDict, i)
        handle.MIT_hres_choices(questionDict, i)
        handle.MOM_hres_default(questionDict, i)
        handle.seaice_choices(questionDict, i)
        handle.heartbeat_default(questionDict, i)
        handle.history_template_default(questionDict, i)
        handle.exp_dir_default(questionDict, i)
        handle.group_root_default(questionDict, i)

        # prompts the user with the question
        questionDict[i].load_question(questionDict)

        # input validation and other post processing goes here,
        # AFTER load_question() call
        # handle.experiment_desc(questionDict, i)
        handle.heartbeat_valid(questionDict, i)
        handle.exp_dir_valid(questionDict, i)

        # strips the first word from every select type question
        handle.select_type(questionDict, i)

        try:
            if questionDict['clone_experiment'].answer == True and i == 'exp_clone_path':
                break
        except KeyError:
            pass
    

    # creates a dictionary that only contains user answers (for simplicity)
    expConfig = {key: questionDict.answer for key, questionDict in questionDict.items()}
    del questionDict

    return expConfig

