
set (files
  __init__.py
  batch_environ.py
  computing_sites.py
  interactive_env.py
  ioserver.py
  node_ops.py
  pbs_env.py
  slurm_env.py
)

install (
   FILES ${files}
   DESTINATION bin/gcmpy/gcmpy/batch_tools
   )
