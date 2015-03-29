# waiter

Continuously run a server and rebuild it on changes for compiled
languages.

## to-do

* pass cli arguments to watcher rather than read in `startServer`
* pass build command as argument
* pass server binary as argument
* create executable that boostraps .gitignore with pid file
* create option to declare a custom location of pid file
* trap ctrl-c to remove pid file, maybe?
