$(info $(.FEATURES))

target: guile file
	@echo hello world
	$(guile (begin (display "i am target") (newline) #f))

guile:
	$(guile (begin (display "i am guile") (newline) "echo guile OK"))

file:
	$(guile (begin (display "i am file") (newline) #f))

zile:
	$(guile (mkdir "/bla-bla-bla"))
