
examples: hw/gen/SpinexWithClock.v hw/gen/SpinexMinimal/SpinexMinimal.v

hw/gen/SpinexWithClock.v:
	./dockers/run-spinal-image.sh sbt "runMain spinalextras.lib.soc.spinex.SpinexWithClock"

hw/gen/SpinexMinimal/SpinexMinimal.v:
	./dockers/run-spinal-image.sh sbt "runMain spinalextras.mains.SpinexMinimal"