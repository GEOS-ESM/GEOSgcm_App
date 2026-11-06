set (files
	README.md
	Quick-Start.md
	Creating-an-Experiment.md
	Running-the-Model.md
	Run-YAML-Reference.md
	New-Features-PR-935.md
	Developer-Guide.md
	Troubleshooting.md
	)

install (
   FILES ${files}
   DESTINATION bin/gcmpy/gcmpy_doc
   )
