
import sys
from pathlib import Path

#from gcmpy.utils.regex_ops import search_replace_in_file
#from gcmpy.utils.regex_ops import extract_line_starting_with
#from gcmpy.utils.regex_ops import grep_file

def get_collection_list(history_dict) -> list():
    """
    Use the dictionary containing the content of a HISTORY.rc
    file to determine the list of collections.

    Parameters
    ----------
    history_dict : dict
      A dictionary containing the content of the file.

    Returns
    -------
    collection_list : list
      The list of collections.
    """
    collections = history_dict["COLLECTIONS"]
    collection_list = list(collections.keys())

    return collection_list

def get_monthly_collection_list(history_dict) -> list():
    """
    Use the dictionary containing the content of a HISTORY.rc
    file to determine the list of collections with monthly
    records.

    Parameters
    ----------
    history_dict : dict
      A dictionary containing the content of the file.

    Returns
    -------
    monthly_colls : list
      The list of monthly collections.
    """
    monthly_colls = list()
    collections = history_dict["COLLECTIONS"]
    for coll in collections:
        if "monthly" in collections[coll].keys():
            monthly_colls.append(coll)

    return monthly_colls

def read_history_rc(history_rc_file: str) -> dict:
    """
    Read the HISTORY.rc file and convert its content into a dictionary.

      - Groups collection attributes into a nested 'COLLECTIONS' dictionary.
      - Ignores collections that are commented out (#) in the COLLECTIONS list.
      - Treats entire lines inside '::' blocks as single list items.

    Parameters
    ----------
    history_rc_file : str
      The path to the HISTORY.rc file.

    Returns
    -------
    history_dict : dict
      A dictionary containing the content of the file.
    """
    config = dict()
    current_key = None
    multiline_keys = set()
    ignored_collections = set()

    if not Path(history_rc_file).is_file():
        print(f" The file {history_rc_file} does not exist!")
        sys.exit(1)

    with open(history_rc_file, "r") as fid:
        hist_lines = fid.readlines()

    for raw_line in hist_lines:
        stripped_line = raw_line.strip()
        if not stripped_line:
            continue

        # --- Special Handling for the COLLECTIONS block ---
        if current_key == 'COLLECTIONS':
            if stripped_line == '::':
                current_key = None
                continue
            
            # If it's a commented-out collection, record it to be ignored
            if stripped_line.startswith('#'):
                col_name = stripped_line.strip('# \t\'"')
                if col_name:
                    ignored_collections.add(col_name)
                continue
            else:
                # Active collection
                val = stripped_line.split('#')[0].strip().strip('\'"')
                if val:
                    config['COLLECTIONS'].append(val)
                continue
        # -------------------------------------------------

        # Standard lines: strip out comments
        line = raw_line.split('#')[0].strip()
        if not line:
            continue

        # Check for the end of a standard multiline block
        if line == '::':
            if current_key:
                multiline_keys.add(current_key)
            current_key = None
            continue

        # Check for a new key-value pair
        if ':' in line and "::" not in line:
            k, v = line.split(':', 1)
            current_key = k.strip()
            v = v.strip()
            
            if current_key == 'COLLECTIONS':
                config[current_key] = list()
                # If the first collection is on the same line as 'COLLECTIONS:'
                if v:
                    val = v.split('#')[0].strip().strip('\'"')
                    if val:
                        config[current_key].append(val)
            else:
                config[current_key] = [v] if v else list()
        else:
            # If there is no colon, it is a continuation of the current multiline list
            if current_key is not None:
                config[current_key].append(line)

    # STEP 2: 
    #      Clean up values 
    #      (remove trailing commas, preserve internal commas)
    cleaned_config = dict()
    for k, v in config.items():
        # Collections is already clean from Step 1
        if k == 'COLLECTIONS':
            cleaned_config[k] = v
            continue

        # Removed the collections not registered in COLLECTIONS
        if any(sub in k for sub in ignored_collections):
            continue
            
        if k in multiline_keys or len(v) > 1:
            cleaned_list = []
            for item in v:
                # Strip leading/trailing spaces and the dangling comma at the end
                cleaned_item = item.strip().rstrip(",").strip()
                if cleaned_item:
                    cleaned_list.append(cleaned_item)
            cleaned_config[k] = cleaned_list
        else:
            # Single value
            if v:
                cleaned_item = v[0].strip().rstrip(",").strip()
                cleaned_config[k] = cleaned_item
            else:
                cleaned_config[k] = ""

    # STEP 3:
    #      Identify if there is a collecltion in the file
    #      that was not registered.

    unused_colls = [k.rsplit(".", 1)[0] for k in cleaned_config.keys() if "." in k and k.rsplit(".",1)[1].strip() in ["template", "fields"]]
    unused_colls_set = set(unused_colls)-set(config['COLLECTIONS'])
    for word in unused_colls_set:
        cleaned_config = {k: v for k, v in cleaned_config.items() if not k.startswith(f"{word}.")}


    # STEP 4: 
    #      Nest the individual collections and filter out ignored ones
    active_collections = cleaned_config.get('COLLECTIONS', [])
    nested_collections = {coll: {} for coll in active_collections}
    
    # Sort collection names by length to prevent partial matching (e.g. 'prog' vs 'prog.eta')
    sorted_active = sorted(active_collections, key=len, reverse=True)
    sorted_ignored = sorted(list(ignored_collections), key=len, reverse=True)

    history_dict = dict()
    
    for key, value in cleaned_config.items():
        if key == 'COLLECTIONS':
            continue
            
        # 4a. Check if this key belongs to an explicitly IGNORED collection
        is_ignored = False
        for ig_coll in sorted_ignored:
            if key.startswith(ig_coll + '.'):
                is_ignored = True
                break
        if is_ignored:
            continue  # Do not read this collection
            
        # 4b. Check if this key belongs to an ACTIVE collection
        matched_active = None
        for act_coll in sorted_active:
            if key.startswith(act_coll + '.'):
                matched_active = act_coll
                break
                
        if matched_active:
            # Move it into the nested dictionary
            attribute_name = key[len(matched_active) + 1:]
            nested_collections[matched_active][attribute_name] = value
        else:
            # It's a standard top-level key (like EXPID or GRID_LABELS)
            history_dict[key] = value

    # Assign the nested dictionary back to the main config
    history_dict['COLLECTIONS'] = nested_collections

    return history_dict

# --- Example Usage ---
if __name__ == "__main__":
    history_rc_file = "/gpfsm/dnb34/jkouatch/GEOS_CCM/GitRepos/gcm_v12/test_gcmpy/HISTORY.rc"
    history_dict = read_history_rc(history_rc_file)
    print()
    print(get_monthly_collection_list(history_dict))
    print()
    print(get_collection_list(history_dict))
    print()
    import json
    print(json.dumps(history_dict, indent=4))

    pass
