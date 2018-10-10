#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <termios.h>
#include "GarageDoorFSM.h"


void blink_on(void)
{
    printf("blink_on()\n");
}

void blink_off(void)
{
    printf("blink_off()\n");
}

void start_timer(void)
{
    printf("start_timer()\n");
}


int main()
{
    GarageDoorFSM_t* sm;
    sm = GarageDoorFSM_New();
    GarageDoorFSM_Start(sm);
    int i = 0;
    while (1)
    {
        printf("Current state: %s\n", GarageDoorFSM_state_to_str(sm->current_state));
        scanf("%d", &i);
        if (i > GarageDoorFSM_NUM_EVENTS) break;
        GarageDoorFSM_event_t ev = (GarageDoorFSM_event_t) i;
        printf("Adding event '%s' to event queue\n", GarageDoorFSM_event_to_str(ev));
        GarageDoorFSM_AddEvent(sm, ev);
        GarageDoorFSM_Deduct(sm);
    }
    return 0;
}
