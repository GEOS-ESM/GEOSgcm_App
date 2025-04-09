import os
import questionary
from questionary import Validator, ValidationError

'''
This class will generate questionary questions based on a set of 
parameters specified in a YAML file. It is designed to be as modular
as possible, so keep it that way! (Handle specific cases in 
process_questions)
'''
class generateQuestion:
    def __init__(self, q_name, q_type, q_prompt, q_choices, q_default, q_follows_up):
        self.q_name = q_name
        self.q_type = q_type
        self.q_prompt = q_prompt
        self.q_choices = q_choices                
        self.q_default = q_default
        self.q_follows_up = q_follows_up
        self.q_answer = None
        

    # Check if a question "follows_up" another and its conditions are met
    # to determine whether it should be asked or not
    def should_ask(self, answerdict):
        if self.q_follows_up:
            for prev_question, accepted_answers in self.q_follows_up:
                if answerdict[prev_question].q_answer in accepted_answers or ('any' in accepted_answers and answerdict[prev_question].q_answer != None) or ('was_not_asked' in accepted_answers):
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
            answer = questionary.text(self.q_prompt, default=self.q_default, validate=qValidator.val_text).ask()
        elif self.q_type == 'confirm':
            answer = questionary.confirm(self.q_prompt, default=self.q_default).ask()
        elif self.q_type == 'select':
            answer = questionary.select(self.q_prompt, choices=self.q_choices).ask()
        elif self.q_type == 'path':
            answer = questionary.path(self.q_prompt, default=self.q_default).ask()

        self.q_answer = answer

# input validation used for questionary library
class qValidator():
    def val_text(input):
        if len(str(input)) == 0 or input.isspace():
            return "please answer the question!"
        else:
            return True
 
