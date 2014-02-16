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


int count = 0;
sem_t sem_binary;
sem_t sem_barrier;
sem_t sem_barrier2;

/* TODO: Make the two threads perform their iterations in a
 * predictable way. Both should perform iteration 1 before iteration 2
 * and then 2 before 3 etc. */

void *
threadA(void *param __attribute__((unused)))
{ 
    int i;
    
    for (i = 0; i < LOOPS; i++) {

        sem_wait(&sem_binary);    //mutex lock,  	 
	count++; //shared data, counting every thread for the next statment
        if(count == NTHREADS){ //if every thread has done this part and are waiting at #1
            sem_wait(&sem_barrier2); //block the second barrier
            sem_post(&sem_barrier); //open the first barrier for one thread
        }
        sem_post(&sem_binary);  //unlock mutex	

        //----------FIRST BARRIER-----------------
        sem_wait(&sem_barrier); //#1
        sem_post(&sem_barrier); //let the next thread pass, this is called a turnstile
        //--------------------------------------

        //Critical section
	printf("threadA --> %d iteration\n", i);
        //sleep(rand() % MAX_SLEEP_TIME);

        sem_wait(&sem_binary); //mutex lock     	 
	count--; //decrease shared data
        if(count == 0){ //if every thread has done this part and are waiting at #2 
            sem_wait(&sem_barrier); //block the first barrier
            sem_post(&sem_barrier2); //open the second barrier for one thread
        }
        sem_post(&sem_binary);//unlock mutex  	

        //-----------SECOND BARRIER---------------
        sem_wait(&sem_barrier2); //#2
        sem_post(&sem_barrier2); //Let the next thread throught the barrier
        //--------------------------------------
        
    } 
    
    pthread_exit(0);
}


void *
threadB(void *param  __attribute__((unused)))
{ 
    int i;
    
    for (i = 0; i < LOOPS; i++) {

        sem_wait(&sem_binary);    //mutex lock,  	 
	count++; //shared data, counting every thread for the next statment
        if(count == NTHREADS){ //if every thread has done this part and are waiting at #1
            sem_wait(&sem_barrier2); //block the second barrier
            sem_post(&sem_barrier); //open the first barrier for one thread
        }
        sem_post(&sem_binary);  //unlock mutex	

        //----------FIRST BARRIER-----------------
        sem_wait(&sem_barrier); //#1
        sem_post(&sem_barrier); //let the next thread pass, this is called a turnstile

        //Critical section
	printf("threadB --> %d iteration\n", i);
        //sleep(rand() % MAX_SLEEP_TIME);

        sem_wait(&sem_binary); //mutex lock     	 
	count--; //decrease shared data
        if(count == 0){ //if every thread has done this part and are waiting at #2 
            sem_wait(&sem_barrier); //block the first barrier
            sem_post(&sem_barrier2); //open the second barrier for one thread
        }
        sem_post(&sem_binary);//unlock mutex  	

        //-----------SECOND BARRIER---------------
        sem_wait(&sem_barrier2); //#2
        sem_post(&sem_barrier2); //Let the next thread throught the barrier

    
    } 
    
    pthread_exit(0);
}

int 
main()
{
    pthread_t tidA, tidB;
    //pthread_t threads[NTHREADS]; //Test n number of threads
    //init the first semaphore at 1, used as a mutex lock.
    sem_init(&sem_binary,0,1);

    //init the second semaphore at 1, used as the first barrier.
    sem_init(&sem_barrier,0,1);
    sem_wait(&sem_barrier); // fill the barrier so that it's full.

    //init the third semaphore at 1, used as the second barrier.
    sem_init(&sem_barrier2,0,1);
    

    srand(time(NULL));
    pthread_setconcurrency(3);
/*
  //-------------Test for n number of threads--------------------
  int i;
    for(i = 0; i < NTHREADS; i++) {
        if(pthread_create(&(threads[i]), NULL, threadA, NULL)){
        perror("pthread_create");
	abort();
        }
    }

    for(i = 0; i < NTHREADS; i++) {
        if(pthread_join(threads[i], NULL) != 0){
        perror("pthread_join");
	abort();
        }
    }
*/



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

    //Destroy all the semaphores
    sem_destroy(&sem_binary);
    sem_destroy(&sem_barrier);
    sem_destroy(&sem_barrier2);

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
