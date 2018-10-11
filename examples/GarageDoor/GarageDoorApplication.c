#include <stdio.h>
#include <stdlib.h>
#include "GarageDoorFSM.h"


int main()
{
    GarageDoorFSM_t* sm;
    sm = GarageDoorFSM_New();
    GarageDoorFSM_Start(sm);
    int i = 0;
    char* space = "  ";
    char* arrow = "->";
    GarageDoorFSM_event_t ev;
    GarageDoorFSM_state_t st;
    while (1)
    {
        char* pre;
        printf("\n\n");
        for(i=0; i<GarageDoorFSM_NUM_STATES; i++)
        {
            st = (GarageDoorFSM_state_t) i;
            pre = (st == sm->current_state) ? arrow : space;
            printf("%s%s\n", pre, GarageDoorFSM_state_to_str(st));
        }
        scanf("%d", &i);
        if (i > GarageDoorFSM_NUM_EVENTS) break;
        ev = (GarageDoorFSM_event_t) i;
        printf("Adding event '%s' to event queue\n", GarageDoorFSM_event_to_str(ev));
        GarageDoorFSM_AddEvent(sm, ev);
        GarageDoorFSM_Deduct(sm);
    }
    return 0;
}
