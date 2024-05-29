/* my_mini_shell.c
 * ----------------------------------------------------------
 *  CS350
 *  A1 Linux Userspace Programming Assignment
 *
 *  Purpose:  - Use Linux programming environment system calls.
 *            - Review process creation and management
 * ----------------------------------------------------------
 */

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/fcntl.h>


#define BUFSIZE 64
#define SEPARATOR " \t\r\n\a"

bool exist_in = false;
bool exist_out = false;
bool exist_pipe = false;


int in_mark = 0;
int out_mark = 0;
int pipe_count = 0;



char *readline(){
  char* tokens = NULL;
  size_t buffer = 0;
  int retcode = getline(&tokens, &buffer, stdin);
  if(retcode == 1 && feof(stdin)){
    exit(0);
  }
  return tokens;
}

char** getargs(char* line){
  int buffer = BUFSIZE;
  int index = 0;
  char ** tokens = malloc(buffer * sizeof(char*));
  char * token;
  token = strtok(line, SEPARATOR);
  
  while(token != NULL){ 

    tokens[index] = token;
    index = index + 1;

    if(index >= buffer){
      buffer += BUFSIZE;
      tokens = realloc(tokens, buffer * sizeof(char*));
    }
    token = strtok(NULL, SEPARATOR);
  }
  tokens[index] = NULL;
  return tokens;

}




void execute(char ** args){
  if(strcmp(args[0], "exit") == 0){
    exit(0);
  }

  pid_t childpid = fork();
  
  if(childpid == 0){
    
    for(int i = 0; args[i] != '\0'; i++){
      if(strcmp(args[i], "<") == 0){
        args[i] =  NULL;
        in_mark = i + 1;
        exist_in = true;
      }
      
      else if(strcmp(args[i], ">") == 0){
        args[i] = NULL;
        out_mark = i + 1;
        exist_out = true;
      } 
    }
      
    
    
    if(exist_in){
        int fd0;
        fd0 = open(args[in_mark], O_RDONLY);
        dup2(fd0, STDIN_FILENO);
        close(fd0);
    }
    if(exist_out){
        int fd1;
        fd1 = creat(args[out_mark], 0644);
        dup2(fd1, STDOUT_FILENO);
        close(fd1);
    }
    execvp(args[0], args);
  } else if(childpid > 0){
    wait(NULL);
  }
  
  return;
}

void loop(){
  char *line;
  char * args;
  while(true){
    printf("$ ");
    line = readline();
    
    args = getargs(line);
    execute(args);
    free(line);
    free(args);
  }
}

int main( int argc, char ** argv )
{
    
    loop();
    
    return 0;
}
