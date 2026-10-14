
set (files
  __init__.py
  cap_files_ops.py
  agcm_file_ops.py
  history_file_ops.py
  restart_file_ops.py
  scratchdir_ops.py
  rc_parser.py
  emissions.py
  extdata.py
  env_builder.py
  mpi_runner.py
  archiver.py
  linkbcs_ops.py
  stage_restarts.py
  run_resources.py
  scratch_setup.py
  history_dirs.py
  executable_setup.py
  gcmemip_setup.py
  restart_metadata.py
  segment_time.py
  ww3_ops.py
  heartbeat_sync.py
  mpi_env.py
  pre_run.py
  post_run.py
  )

install (
   FILES ${files}
   DESTINATION bin/gcmpy/gcmpy/run_tools
   )
