#!/usr/bin/env python3
import yaml
import argparse

def parse_args():
    p = argparse.ArgumentParser(description='create extdata master file')
    p.add_argument('rc_file',type=str,help='GEOS_ChemGridComp.rc file')
    return vars(p.parse_args())

if __name__ == '__main__':

   args = parse_args()
   rc_file = args['rc_file']

   f = open(rc_file,"r")
   temp_text = f.readlines()
   f.close()

   component_yaml_files = yaml.safe_load("""
   GOCART:
   - CFC_GridComp_ExtData.yaml
   - CH4_GridComp_ExtData.yaml
   - CO2_GridComp_ExtData.yaml
   - CO_GridComp_ExtData.yaml
   - O3_GridComp_ExtData.yaml
   - Rn_GridComp_ExtData.yaml
   GOCART2G:
   - CA2G_GridComp_ExtData.yaml
   - DU2G_GridComp_ExtData.yaml
   - NI2G_GridComp_ExtData.yaml
   - SS2G_GridComp_ExtData.yaml
   - SU2G_GridComp_ExtData.yaml
   GAAS:
   - GAAS_GridComp_ExtData.yaml
   STRATCHEM:
   - StratChem_ExtData.yaml
   GMICHEM:
   - GMI_ExtData.yaml
   GEOSCHEM:
   - GEOSCHEMchem_ExtData.yaml
   CARMA:
   - CARMAchem_GridComp_ExtData.yaml
   MAM:
   - MAM7_ExtData.yaml
   ACHEM:
   - ACHEM_ExtData.yaml
   TR:
   - TR_ExtData.yaml
   DNA:
   - DNA_ExtData.yaml
   RRG:
   - RRG_GridComp_ExtData.yaml
   HEMCO:
   - HEMCOgmi_ExtData.yaml
   - HEMCOgocart2g_ExtData.yaml
   Extras:
   - ChemEnv_ExtData.yaml
   - WSUB_ExtData.yaml
   - DataSea_ExtData.yaml
   - UMWM_ExtData.yaml
   - JRA55-DO_DataAtm_Forcings_ExtData.yaml
   """)
   components = {}
   for line in temp_text:
      if "ENABLE" in line:
          entry = line.split(":")
          if ".TRUE." in entry[1]:
             component_on = True
          if ".FALSE." in entry[1]:
             component_on = False
          key_name = entry[0]
          component_name = key_name.replace("ENABLE_","")
          component_name = component_name.lstrip()
          components[component_name] = component_on

   files_to_use = []
   for key in components:
       if key in component_yaml_files and components[key]:
          files = component_yaml_files[key]
          for single_file in files:
              files_to_use.append(single_file)

   extra_files = component_yaml_files["Extras"]
   for single_file in extra_files:
       files_to_use.append(single_file)

   f = open("extdata.yaml","w")
   f.write("subconfigs:\n")
   for single_file in files_to_use:
       f.write(" - "+single_file+"\n")
   f.close()

