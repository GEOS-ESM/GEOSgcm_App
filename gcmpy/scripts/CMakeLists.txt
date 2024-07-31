set (programs
  atmosphere.py
  env.py
  generate_question.py
  gocart.py
  land.py
  model.py
  ocean.py
  process_questions.py
  utility.py
  )

install (
   PROGRAMS ${programs}
   DESTINATION bin/gcmpy/scripts
   )

# processed files
set (setup_scripts)

foreach (file ${setup_scripts})
   configure_file(${file} ${file} @ONLY)
   install(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/${file} DESTINATION bin/gcmpy)
endforeach ()
