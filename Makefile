all:
	mkdir /tmp/output || true
	sbt "run ./model.als"
