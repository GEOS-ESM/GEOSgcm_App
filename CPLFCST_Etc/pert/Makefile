#LIB_NETCDF      = -L/discover/nobackup/projects/gmao/share/dao_ops/Baselibs/Baselibs/x86_64-unknown-linux-gnu/ifort/Linux/lib -lnetcdf -L/discover/nobackup/projects/gmao/share/dao_ops/Baselibs/Baselibs/x86_64-unknown-linux-gnu/ifort/Linux/lib -lhdf5_hl -lhdf5 -lz -lm -L/discover/nobackup/projects/gmao/share/dao_ops/Baselibs/Baselibs/x86_64-unknown-linux-gnu/ifort/Linux/lib -lmfhdf -ldf -lsz -ljpeg -lm -L/discover/nobackup/projects/gmao/share/dao_ops/Baselibs/Baselibs/x86_64-unknown-linux-gnu/ifort/Linux/lib -lmfhdf -ldf -L/discover/nobackup/projects/gmao/share/dao_ops/Baselibs/Baselibs/x86_64-unknown-linux-gnu/ifort/Linux/lib -lcurl -lrt -lm -lz -lm
#LIB_NETCDF      = -L/gpfsm/dnb52/projects/p10/dao_ops/Baselibs/v3.2.0_build1/x86_64-unknown-linux-gnu/ifort/Linux/lib -lnetcdf -L/discover/nobackup/projects/gmao/share/dao_ops/Baselibs/Baselibs/x86_64-unknown-linux-gnu/ifort/Linux/lib -lhdf5_hl -lhdf5 -lz -lm -L/discover/nobackup/projects/gmao/share/dao_ops/Baselibs/Baselibs/x86_64-unknown-linux-gnu/ifort/Linux/lib -lmfhdf -ldf -lsz -ljpeg -lm -L/discover/nobackup/projects/gmao/share/dao_ops/Baselibs/Baselibs/x86_64-unknown-linux-gnu/ifort/Linux/lib -lmfhdf -ldf -L/discover/nobackup/projects/gmao/share/dao_ops/Baselibs/Baselibs/x86_64-unknown-linux-gnu/ifort/Linux/lib -lcurl -lrt -lm -lz -lm
LIB_NETCDF      = -L/discover/nobackup/projects/gmao/share/dao_ops/Baselibs/v3.2.0_build3/x86_64-unknown-linux-gnu/ifort/Linux/lib -lnetcdf -L/discover/nobackup/projects/gmao/share/dao_ops/Baselibs/v3.2.0_build3/x86_64-unknown-linux-gnu/ifort/Linux/lib -lhdf5_hl -lhdf5 -lz -lm -L/discover/nobackup/projects/gmao/share/dao_ops/Baselibs/v3.2.0_build3/x86_64-unknown-linux-gnu/ifort/Linux/lib -lmfhdf -ldf -lsz -ljpeg -lm -L/discover/nobackup/projects/gmao/share/dao_ops/Baselibs/v3.2.0_build3/x86_64-unknown-linux-gnu/ifort/Linux/lib -lmfhdf -ldf -L/discover/nobackup/projects/gmao/share/dao_ops/Baselibs/v3.2.0_build3/x86_64-unknown-linux-gnu/ifort/Linux/lib -lcurl -lrt -lm -lz -lm
#INC_NETCDF      = /gpfsm/dnb52/projects/p10/dao_ops/Baselibs/v3.2.0_build1/x86_64-unknown-linux-gnu/ifort/Linux/include/netcdf
INC_NETCDF      = /discover/nobackup/projects/gmao/share/dao_ops/Baselibs/v3.2.0_build3/Linux/include/netcdf
F90_COMPILE	= ifort
F90FLAGS    =  -traceback -convert big_endian -O2
# -warn unused
# -g -real_size 32
.SUFFIXES: .o .f90 .txt 

#.f90.o:
#	$(F90_COMPILE) -c $<
#.f90:
#	$(F90_COMPILE) -o $* $< *.mod $(F90FLAGS) -I $(INC_NETCDF) $(LIB_NETCDF)
#	mv $* ../work/$*.x 

modules:
	$(F90_COMPILE) $(F90FLAGS) -c modules.f90

cal_norm_for2sided:
	$(F90_COMPILE) $(F90FLAGS) -c cal_norm_for2sided.f90 $(LIB_NETCDF) -I $(INC_NETCDF)

read2write:
	$(F90_COMPILE) $(F90FLAGS) ocn_read2write.f90 modules.f90 $(LIB_NETCDF) -I $(INC_NETCDF) -o ocn.bred.exe

ocn_bred:
	$(F90_COMPILE) $(F90FLAGS) ocn_read2write.f90 cal_norm_for2sided.f90 modules.f90 $(LIB_NETCDF) -I $(INC_NETCDF) -o ocn.bred.exe
atm_bred:
	$(F90_COMPILE) atm_read2write.F -o atm.bred.exe
perturb_ocean:
	$(F90_COMPILE) $(F90FLAGS) perturb_ocean.f90 modules.f90 $(LIB_NETCDF) -I $(INC_NETCDF) -o perturb_ocean.exe
perturb_aogcm:
	$(F90_COMPILE) $(F90FLAGS) perturb_aogcm.f90 modules.f90 $(LIB_NETCDF) -I $(INC_NETCDF) -o perturb_aogcm.exe
perturb_ocean4PENT:
	$(F90_COMPILE) $(F90FLAGS) perturb_ocean4PENT.f90 modules.f90 $(LIB_NETCDF) -I $(INC_NETCDF) -o perturb_ocean4PENT.exe
perturb_oceanlowlowave:
	$(F90_COMPILE) $(F90FLAGS) perturb_oceanlowlowave.f90 modules.f90 $(LIB_NETCDF) -I $(INC_NETCDF) -o perturb_oceanlowlowave.exe
perturb_oceanlowlowavemake:
	$(F90_COMPILE) $(F90FLAGS) perturb_oceanlowlowavemake.f90 modules.f90 $(LIB_NETCDF) -I $(INC_NETCDF) -o perturb_oceanlowlowavemake.exe
perturb_atmos:
	$(F90_COMPILE) perturb_atmoslohi.F -o perturb_atmos.exe
perturb_atmoslowlowave:
	$(F90_COMPILE) perturb_atmoslowlowave.F -o perturb_atmoslowlowave.exe
perturb_atmoslowlowavemake:
	$(F90_COMPILE) perturb_atmoslowlowavemake.F -o perturb_atmoslowlowavemake.exe
ocn_norm:
	$(F90_COMPILE) $(F90FLAGS) norm_test.f90 cal_norm_for2sided.f90 modules.f90 $(LIB_NETCDF) -I $(INC_NETCDF) -o ocn_norm
cal_norm_rst:
	$(F90_COMPILE) $(F90FLAGS) cal_norm_rst.f90 modules.f90 $(LIB_NETCDF) -I $(INC_NETCDF) -o cal_norm_rst.exe
perturb_atmichihi:
	$(F90_COMPILE) perturb_atmichihi.F -o perturb_atmichihi.exe
perturb_atmoshihi2:
	$(F90_COMPILE) perturb_atmoshihi2.F -o perturb_atmoshihi2.exe
perturb_atmoshihi4PENT:
	$(F90_COMPILE) perturb_atmoshihi4PENT.F -o perturb_atmoshihi4PENT.exe
perturb_oceanNew:
	$(F90_COMPILE) $(F90FLAGS) perturb_oceanNew.f90 modules.f90 $(LIB_NETCDF) -I $(INC_NETCDF) -o perturb_oceanNew.exe
