GarageDoor: GarageDoor.sgf GarageDoorActions.c GarageDoorApplication.c GarageDoorFSM.h GarageDoorFSM.c
	gcc -I. GarageDoorActions.c GarageDoorFSM.c GarageDoorApplication.c -o GarageDoor

GarageDoorFSM.h GarageDoorFSM.c:
	sisyphus -T../../templates -nGarageDoorFSM -d. -tC GarageDoor.sgf

clean:
	rm GarageDoorFSM.h GarageDoorFSM.c GarageDoor

.PHONY: clean
