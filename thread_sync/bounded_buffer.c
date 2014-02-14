/**
 * multiple threads accessing the same bounded buffer - skeleton code
 *
 * Course: Operating Systems and Multicore Programmin - OSM
 * Lab assignment 2: Thread-safe bounded buffer
 *
 * Author: Nikos Nikoleris <nikos.nikoleris@it.uu.se>
 *
 */

#include <stdio.h>     /* printf(), fprintf */
#include <stdlib.h>    /* [s]rand() */
#include <unistd.h>    /* sleep() */
#include <pthread.h>   /* pthread_... */
#include <semaphore.h> /* sem_... */

#define BUFFER_SIZE 5

#define PRODUCERS 2
#define CONSUMERS 4

#define ITERATIONS 20
#define PRODUCER_ITERATIONS (ITERATIONS / PRODUCERS)
#define CONSUMER_ITERATIONS (ITERATIONS / CONSUMERS)

typedef struct {
    int value[BUFFER_SIZE];
    int next_in, next_out;
} buffer_t;


buffer_t buffer;

pthread_mutex_t mutex_lock = PTHREAD_MUTEX_INITIALIZER;
sem_t sem_lock_full;
sem_t sem_lock_empty;

pthread_t consumer_tid[CONSUMERS], producer_tid[PRODUCERS];

/* *
 * insert_item - thread safe(?) function to insert items to the bounded buffer
 * @param item the value to be inserted
 * @return 0 in case of sucess -1 otherwise
 */
int
insert_item(int item)
{
    /* TODO: Check and wait if the buffer is full. Ensure exclusive
     * access to the buffer and use the existing code to remove an item.
     */
    //mindre lediga platser
    sem_wait(&sem_lock_empty);
    //ta mutex låset
    pthread_mutex_lock(&mutex_lock);
    
   
    buffer.value[buffer.next_in] = item;
    buffer.next_in = (buffer.next_in + 1) % BUFFER_SIZE;
    //Lämna låset
    pthread_mutex_unlock(&mutex_lock);
    sem_post(&sem_lock_full);    

    return 0;
}

/**
 * remove_item - thread safe(?) function to remove items to the bounded buffer
 * @param item the address of the variable that the removed value will be written
 * @return 0 in case of sucess -1 otherwise
 */
int
remove_item(int *item)
{
    /* TODO: Check and wait if the buffer is empty. Ensure exclusive
     * access to the buffer and use the existing code to remove an item.
     */
    //fler lediga platser
    sem_wait(&sem_lock_full);
    //ta mutex låset
    pthread_mutex_lock(&mutex_lock);
    *item = buffer.value[buffer.next_out];
    buffer.value[buffer.next_out] = -1;
    buffer.next_out = (buffer.next_out + 1) % BUFFER_SIZE;
    //Lämna låset
    pthread_mutex_unlock(&mutex_lock);
    //get tillbaka empty låset
    sem_post(&sem_lock_empty);
    return 0;
}

/**
 * producer - will iterate PRODUCER_ITERATION times and call the corresponding
 * function to insert an integer to the bounded buffer
 * @param param an integer id of the producer used to distinguish between the
 * multiple producer threads
 * @return nothing
 */
void *
producer(void *param)
{
    int item, i;
    long int id = (long int)param;

    printf("producer started\n");
    i = PRODUCER_ITERATIONS;
    while (i--) {
	sleep(rand() % 3);

	item = rand() % 10000;
	if (insert_item(item))
	    fprintf(stderr, "Error while inserting to buffer\n");
	else
	    printf("producer %ld: inserted %d\n", id, item);
    }

    pthread_exit(0);
}

/**
 * consumer - will iterate CONSUMER_ITERATION times and call the corresponding
 * function to remove an integer from the bounded buffer
 * @param param an integer id of the producer used to distinguish between the
 * multiple consumer threads
 * @return nothing
 */
void *
consumer(void *param)
{
    int item, i;
    long int id = (long int)param;

    printf("consumer started\n");
    i = CONSUMER_ITERATIONS;
    while (i--) {
	sleep(rand() % 3);

	if (remove_item(&item))
	    fprintf(stderr, "Error while removing from buffer\n");
	else
	    printf("consumer %ld: removed %d\n", id, item);
    }

    pthread_exit(0);
}

int
main()
{
    long int i;

    srand(time(NULL));
    sem_init(&sem_lock_empty,0,BUFFER_SIZE);
    sem_init(&sem_lock_full,0,BUFFER_SIZE);
    int n;
    for(n = 0; n < BUFFER_SIZE; n++) sem_wait(&sem_lock_full);

    /* Create the consumer threads */
    for (i = 0; i < CONSUMERS; i++)
	if (pthread_create(&consumer_tid[i], NULL, consumer, (void *)i) != 0) {
	    perror("pthread_create");
	    abort();
	}
    /* Create the producer threads */
    for (i = 0; i < PRODUCERS; i++)
	if (pthread_create(&producer_tid[i], NULL, producer, (void *)i) != 0) {
	    perror("pthread_create");
	    abort();
	}

    /* Wait for them to complete */
    for (i = 0; i < CONSUMERS; i++)
	if (pthread_join(consumer_tid[i], NULL) != 0) {
	    perror("pthread_join");
	    abort();
	}
    for (i = 0; i < PRODUCERS; i++)
	if (pthread_join(producer_tid[i], NULL) != 0) {
	    perror("pthread_join");
	    abort();
	}


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
