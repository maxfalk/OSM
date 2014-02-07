/**
 * Game of luck: Implementation of the Gamemaster
 *
 * Course: Operating Systems and Multicore Programming - OSM lab
 * assignment 1: game of luck.
 *
 * Author: Nikos Nikoleris <nikos.nikoleris@it.uu.se>
 *
 */

#include <stdio.h> /* I/O functions: printf() ... */
#include <stdlib.h> /* rand(), srand() */
#include <unistd.h> /* read(), write() calls */
#include <assert.h> /* assert() */
#include <time.h>   /* time() */
#include <signal.h> /* kill(), raise() and SIG???? */

#include <sys/types.h> /* pid */
#include <sys/wait.h> /* waitpid() */

#include "common.h"

int main(int argc, char *argv[])
{
	int i, seed;
	int status;
	int score[NUM_PLAYERS];
	int return_pid;
	int pid[NUM_PLAYERS];
	int filedes_player[NUM_PLAYERS][2];
	int filedes_master[NUM_PLAYERS][2];
       
	/* TODO: initialize the communication with the players */

	for (i = 0; i < NUM_PLAYERS; i++) {

	  //Pipe skapas mellan parent och dess child
	  //pipe medlar mellan två processer
	  pipe(filedes_player[i]);
	  pipe(filedes_master[i]);

	}

       

	for (i = 0; i < NUM_PLAYERS; i++) {
	  /* TODO: spawn the processes that simulate the players */
	  pid[i] = fork();
	  /* TODO: Use the following variables in the exec system call. Using the
	   * function sprintf and the arg1 variable you can pass the id parameter
	   * to the children
	   */
	  char arg0[] = "./shooter";
	  char arg1[] = {(char) (((int)'0')+i)}; // This only works for numbers 0-9
	  char *args[] = {arg0, arg1, NULL};
	  int q = 0;   
	  switch(pid[i]){
	  case -1:
	    perror("Error could not fork.\n");
	    return 1;
	  case 0:

	    /*
	      	      |<--xx-READ-|          |--xx-READ->|
		      |           |--[PIPE]--|           |
		      |-xx-WRITE->|          |<----WRITE-|	    
	      PARENT--|                                  |--CHILD
	      	      |<--xx-READ-|          |-----READ->|
		      |           |--[PIPE]--|           |
		      |--WRITE--->|          |<-xx-WRITE-|


	    */
	   
	    for (; q < NUM_PLAYERS; q++) {
	      if (q != i) {
		close(filedes_player[q][0]);
 		close(filedes_master[q][1]);
		close(filedes_player[q][1]);
		close(filedes_master[q][0]);

	      }
	    }
	    close(filedes_player[i][0]);
	    close(filedes_master[i][1]);
	    
//Sätter ny standard in/out innan vi exekverar vårt program för att kunna läsa och skriva i pipsen
	    dup2(filedes_master[i][0],STDIN_FILENO);
	    dup2(filedes_player[i][1],STDOUT_FILENO);
	   
      
	    //shooter(i,filedes_master[i][0],filedes_player[i][1]);
	    execv(arg0,args);
	    break;
	  default:
	    close(filedes_master[i][0]);
	    close(filedes_player[i][1]);

	    //Kopplar om filedescriptorn så vi skriver till vår pipe instället
	    dup2(filedes_master[i][1],STDIN_FILENO);
	    dup2(filedes_player[i][0],STDOUT_FILENO);
      
	  }
	}

	seed = time(NULL);
	for (i = 0; i < NUM_PLAYERS; i++) {
	  seed++;
	  /* TODO: send the seed to the players */
	  write(filedes_master[i][1],&seed, 4);
	
	}


	

	/* TODO: get the dice results from the players, find the winner */
	for (i = 0; i < NUM_PLAYERS; i++) {
	 
	  read(filedes_player[i][0],&(score[i]),4);
	}
	
	int winner_score = 0;
	for (i = 0; i < NUM_PLAYERS; i++) {
	  if(winner_score < score[i]){
	    winner_score = score[i];
	    winner = i;
	  
	  }
	}

	fprintf(stderr,"master: player %d WINS\n", winner);

	/* TODO: signal the winner */
	kill(pid[winner],SIGUSR1);
	/* TODO: signal all players the end of game */
	for (i = 0; i < NUM_PLAYERS; i++) {
	  if(i != winner){
	    kill(pid[i],SIGUSR2);
	  }
	  
	}

	fprintf(stderr,"master: the game ends\n");

	/* TODO: cleanup resources and exit with success */
	for (i = 0; i < NUM_PLAYERS; i++) {
	  return_pid = wait(&status);
	  waitstat(return_pid,status);
	}



	return 0;
}
