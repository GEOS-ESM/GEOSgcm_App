set (programs
  atmosphere.py
  generate_question.py
  gocart.py
  land.py
  gcm_setup.py
  ocean.py
  process_questions.py
  utility.py
  clone.py
  )

foreach (file ${programs})
   configure_file(${file} ${file} @ONLY)
   install(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/${file} DESTINATION bin/gcmpy/scripts)
endforeach ()
