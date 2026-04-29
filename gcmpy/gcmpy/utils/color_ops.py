import os, platform, yaml, shutil

# pretty font
class Color:
    PURPLE      = '\033[95m'
    CYAN        = '\033[96m'
    DARKCYAN    = '\033[36m'
    BLUE        = '\033[94m'
    GREEN       = '\033[92m'
    YELLOW      = '\033[93m'
    RED         = '\033[91m'
    BOLD        = '\033[1m'
    UNDERLINE   = '\033[4m'
    END         = '\033[0m'
    RESET       = '\033[0m'

    # accepts any string and styles it
    def color_path(path):
        return Color.BLUE + Color.BOLD + path + Color.RESET

    def color_file(file):
        return Color.GREEN + Color.BOLD + file + Color.RESET

