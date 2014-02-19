/**
 * Two threads executing chunks of work in a lock step - skeleton code
 * 
 * Course: Operating Systems and Multicore Programming - OSM
 * Lab assignment 2: Rendezvous locking.
 *
 * Author: Nikos Nikoleris <nikos.nikoleris@it.uu.se>
 *
 */

#include <stdio.h>     /* printf() */
#include <stdlib.h>    /* abort(), [s]rand() */
#include <unistd.h>    /* sleep() */
#include <semaphore.h> /* sem_...() */
#include <pthread.h>   /* pthread_...() */

#define LOOPS 5
#define NTHREADS 2
#define MAX_SLEEP_TIME 3


//Two semaphores to keep track of the progress of the diffrent threads.
sem_t sem_lock;
sem_t sem_lock2;

/* TODO: Make the two threads perform their iterations in a
 * predictable way. Both should perform iteration 1 before iteration 2
 * and then 2 before 3 etc. */

void *
threadA(void *param __attribute__((unused)))
{ 
    int i;
    /*
      locks sem_lock and increases sem_lock2, this ensures that after each iteration it wait for the other thread to
      complete.
     */
    for (i = 0; i < LOOPS; i++) {
        sem_wait(&sem_lock);
        //Critical section
	printf("threadA --> %d iteration\n", i);
        sleep(rand() % MAX_SLEEP_TIME);
        sem_post(&sem_lock2);
        
    } 
    
    pthread_exit(0);
}


void *
threadB(void *param  __attribute__((unused)))
{ 
    int i;
   
    /*
      locks sem_lock2 and increases sem_lock, this ensures that after each iteration it wait for the other thread to
      complete.
     */ 
    for (i = 0; i < LOOPS; i++) {
        sem_wait(&sem_lock2);
        //Critical section
	printf("threadB --> %d iteration\n", i);
        sleep(rand() % MAX_SLEEP_TIME);
        sem_post(&sem_lock);
    
    } 
    
    pthread_exit(0);
}

int 
main()
{
    pthread_t tidA, tidB;
    //initate 
    sem_init(&sem_lock,0,1);
    sem_init(&sem_lock2,0,1);

    srand(time(NULL));
    pthread_setconcurrency(3);

    if (pthread_create(&tidA, NULL, threadA, NULL) || 
	pthread_create(&tidB, NULL, threadB, NULL)) {
	perror("pthread_create");
	abort();
    }
    if (pthread_join(tidA, NULL) != 0 || 
        pthread_join(tidB, NULL) != 0) {
	perror("pthread_join");
	abort();
    }


    sem_destroy(&sem_lock);
    sem_destroy(&sem_lock2);

    return 0;
}

/*
 * Local Variables:
 * mode: c
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * c-file-style: "stroustrup"
 * End:
 */
