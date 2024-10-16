import yaml, re, os
from generate_question import generateQuestion
from utility import envdict, pathdict, color

"""
This class handles special cases where a question"s properties need to be checked/dynamically 
changed at runtime based on certain conditions (e.g. input validation)
"""
class handle:

    @staticmethod
    def select_type(answerdict, i):
        if answerdict[i].q_type == "select" and answerdict[i].q_answer != None:
            # as of right now, we only want the first word of every select-type question. 
            # If that changes it's probably best to delete this function.
            answerdict[i].q_answer = answerdict[i].q_answer.split(None, 1)[0]

    @staticmethod
    def experiment_desc(answerdict, i):
        if i == "experiment_description" and answerdict[i].q_answer != None:
            while answerdict["experiment_description"].q_answer == answerdict["experiment_id"].q_answer:
                print(color.RED + "The experiment description must be different from the ID!")
                answerdict[i].load_question(answerdict)

    @staticmethod
    def processor_choices(answerdict, i):
        if i == "processor":
            if envdict["site"] == "NCCS":
                answerdict[i].q_choices = ["Skylake", "Cascade Lake"]
            elif envdict["site"] == "NAS":
                answerdict[i].q_choices = ["Skylake", "Haswell", "Broadwell", "Cascade Lake", "AMD Rome"]
            else:
                exit(1)

            print(color.GREEN + "NOTE Due to how FV3 is compiled by default, Sandy Bridge\n" + \
                                "and Ivy Bridge are not supported by current GEOS" + color.RESET)

    @staticmethod
    def MIT_hres_choices(answerdict, i):
        # This is a dumb case, but these are the only known ocean resolutions that work
        # with these atmosphere resolutions, so we will only give the users these choices
        if i == "OM_MIT_horizontal_res":
            if answerdict["AM_horizontal_res"].q_answer == "c720":
                answerdict[i].q_choices = ["llc1080 (1/12-deg, Lat-Lon-Cube)"]
            elif answerdict["AM_horizontal_res"].q_answer == "c1440":
                answerdict[i].q_choices = ["llc2160 (1/24-deg, Lat-Lon-Cube)"]

    @staticmethod
    def MOM_hres_default(answerdict, i):
        if i == "OM_MOM_horizontal_res" and \
           answerdict["OM_name"].q_answer == "MOM6" and \
           answerdict["AM_horizontal_res"].q_answer == "c12":
           answerdict[i].q_default = "72 36"

    @staticmethod 
    def OM_hres_valid(answerdict, i):
        if i == "OM_MOM_horizontal_res" and answerdict[i].q_answer != None:
            #input validation using regex
            while not re.match(r"^\d+\s\d+$", answerdict[i].q_answer):
                print(color.RED + "please enter exactly 2 numbers separated by a space! (int int)\n")
                answerdict[i].load_question(answerdict)

    @staticmethod
    def heartbeat_default(answerdict, i):
        if i == "heartbeat":
            '''
            Default heartbeat is determined by atmospheric resolution.
            Of course, this just the recommended value. The user can 
            enter whatever value they like
            '''
            heartbeat = ""
            match answerdict["AM_horizontal_res"].q_answer:
                case "c12" | "c24" | "c48" | "c90":
                    heartbeat = "1800"
                case "c180":
                    heartbeat = "900"
                case "c360":
                    heartbeat = "450"
                case "c720":
                    heartbeat = "225"
                case "c1440":
                    heartbeat = "75"
                case "c5760":
                    heartbeat = "30"
                case "c270" | "c540":
                    heartbeat = "600"
                case "c1080":
                    heartbeat = "150"
                case "c1536":
                    heartbeat = "75"
                case "c2160":
                    heartbeat = "60"


            # Per W. Putman recommendation, set heartbeat to 450s anytime BACM_1M is selected
            # ((IMPORTANT: default must be type string))
            if answerdict["AM_microphysics"].q_answer == "BACM_1M":
                heartbeat = "450"

            answerdict[i].q_default = heartbeat


    @staticmethod
    def heartbeat_valid(answerdict, i):
        if i == "heartbeat":
            # input validation using regex
            while not re.match(r"^\d+$", answerdict[i].q_answer):
                print(f"{color.RED}please enter exactly 1 number!{color.RESET}")
                answerdict[i].load_question(answerdict)

    @staticmethod
    def history_template_default(answerdict, i):
        if i == "history_template":

            match answerdict["OM_name"]:
                case "MOM5":
                    answerdict[i].q_default = f"{pathdict['etc']}/HISTORY.AOGCM-MOM5.rc.tmpl"
                case "MOM6":
                    answerdict[i].q_default = f"{pathdict['etc']}/HISTORY.AOGCM.rc.tmpl"
                case "MIT":
                    answerdict[i].q_default = f"{pathdict['etc']}/HISTORY.AOGCM_MITgcm.rc.tmpl"
                case _:            
                    answerdict[i].q_default = f"{pathdict['etc']}/HISTORY.AGCM.rc.tmpl"

    
    @staticmethod
    def history_template_valid(answerdict, i):
        if i == "history_template":
            while not os.path.exists(answerdict[i].q_answer):
                print(f"Error: Could not find {color.RED}{answerdict[i]}{color.RESET}")
                answerdict[i].load_question(answerdict)
    

    @staticmethod
    def exp_dir_default(answerdict, i):
        if i == "exp_dir":
            root = f"{os.environ.get('HOME')}/.EXPDIRroot"
            if os.path.exists(root):
                try:
                    with open(root, "r") as file:
                        answerdict[i].q_default = f"{file.read().strip()}/{answerdict['experiment_id'].q_answer}"
                except Exception as e:
                    print(f"An error occurred while reading {color.BLUE}.EXPDIRroot{color.RESET}: {str(e)}")
            elif envdict['site'] in ['NAS','NCCS']:
                answerdict[i].q_default = f"/{'discover/' if envdict['site'] == 'NCCS' else ''}nobackup/{os.environ.get('LOGNAME')}/{answerdict['experiment_id'].q_answer}"
            else:
                answerdict[i].q_default = f"{os.environ.get('HOME')}/{answerdict['experiment_id']}"
            

    @staticmethod
    def exp_dir_valid(answerdict, i):
        if i == "home_dir" or i == "exp_dir":
            while os.path.basename(answerdict[i].q_answer) != answerdict['experiment_id'].q_answer:
                print(f"{color.RED}This directory MUST point to the experiment ID: {color.BLUE}{answerdict['experiment_id'].q_answer}{color.RED}!{color.RESET}")
                answerdict[i].load_question(answerdict)


    @staticmethod 
    def group_root_default(answerdict, i):
        if i == "group_root":
            groups = subprocess.check_output('groups', shell=True).decode('utf-8').strip()
            answerdict[i].q_default = groups.split()[0]





# open yaml file and create dictionary from it's contents
def load_yamls():

    # list of question files (*MAKE SURE THEY ARE IN THE ORDER YOU WANT THEM TO BE ASKED*)
    file_list = ["../yaml/exp_setup.yaml",          \
                 "../yaml/atmospheric_model.yaml",  \
                 "../yaml/ocean_model.yaml",        \
                 "../yaml/land_model.yaml",         \
                 "../yaml/gocart.yaml",             \
                 "../yaml/directory_setup.yaml"]
    all_yaml_questions = {}

    for filename in file_list:
        try:
            with open(filename, "r") as file:
                yaml_questions = yaml.safe_load(file)
                all_yaml_questions.update(yaml_questions)
        except IOError:
            print(f"{color.RED}YAML file '{filename}' could not be located. Exiting.")
            exit(1)

    return all_yaml_questions

# actual driver for questionary questions
def process():
    answerdict = {}
    yaml_questions = load_yamls()

    # creates a dictionary of question:answer pairs
    for i in yaml_questions:
        temp = generateQuestion(i, yaml_questions[i]["type"],          \
                                   yaml_questions[i]["prompt"],        \
                                   yaml_questions[i]["choices"],       \
                                   yaml_questions[i]["default_answer"],\
                                   yaml_questions[i]["follows_up"])

        answerdict[i] = temp   


        # if the question properties need to dynamically change at
        # runtime call handle function BEFORE load_question()
        handle.processor_choices(answerdict,i)
        handle.MIT_hres_choices(answerdict, i)        
        handle.MOM_hres_default(answerdict, i)
        handle.heartbeat_default(answerdict, i)
        handle.history_template_default(answerdict, i)
        handle.exp_dir_default(answerdict, i)

        # prompts the user with the question
        answerdict[i].load_question(answerdict)   

        # input validation and other post processing goes here,
        # AFTER load_question() call
        handle.experiment_desc(answerdict, i)
        handle.OM_hres_valid(answerdict, i)
        handle.heartbeat_valid(answerdict, i)
        handle.history_template_valid(answerdict, i)
        handle.exp_dir_valid(answerdict, i)
        
        # strips the first word from every select type question
        handle.select_type(answerdict, i)

    return answerdict

