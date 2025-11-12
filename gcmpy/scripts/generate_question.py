import os
import questionary
from questionary import Validator, ValidationError

'''
This class will generate questionary questions based on a set of 
parameters specified in a YAML file. It is designed to be as modular
as possible, so keep it that way! (Handle specific cases in 
process_questions)
'''
class question:
    def __init__(self, name, q_type, prompt, choices, default, follows_up):
        self.name = name
        self.q_type = q_type
        self.prompt = prompt
        self.choices = choices                
        self.default = default
        self.follows_up = follows_up
        self.answer = None
        

    # Check if a question "follows_up" another and its conditions are met
    # to determine whether it should be asked or not
    def should_ask(self, questionDict):
        if self.follows_up:
            for prev_question, accepted_answers in self.follows_up:
                if questionDict[prev_question].answer in accepted_answers or ('any' in accepted_answers and questionDict[prev_question].answer != None) or ('was_not_asked' in accepted_answers):
                    return True
            return False
        return True


    # loads the questionary api based on yaml configurations
    def load_question(self, question):

        # check if the yaml entry should be skipped
        if not self.should_ask(question):
            return 

        # if should_ask() returns true, call correct questionary API
        elif self.q_type == 'text':
            answer = questionary.text(self.prompt, default=self.default, validate=qValidator.val_text).ask()
        elif self.q_type == 'confirm':
            answer = questionary.confirm(self.prompt, default=self.default).ask()
        elif self.q_type == 'select':
            answer = questionary.select(self.prompt, choices=self.choices).ask()
        elif self.q_type == 'path':
            answer = questionary.path(self.prompt, default=self.default).ask()

        self.answer = answer

# input validation used for questionary library
class qValidator():
    def val_text(input):
        if len(str(input)) == 0 or input.isspace():
            return "please answer the question!"
        else:
            return True
 
