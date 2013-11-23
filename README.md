iGrach
======

Games App Store

Prerequisites
-------------

* Erlang R16: sudo apt-get install erlang
* Rebar: https://github.com/rebar/rebar
* iNotify Tools: sudo apt-get install inotify-tools

Install
-------

    $ make
    $ make start
    $ make attach
    
If something goes wrong you need to boot directly into console (bypass start and attach)

    $ make console
    
And debug the output.

Database Initialization
-----------------------

Then in Erlang console you should init the DB.

    > kvs:join().
    > kvs:initialize().
    > kvs:init_db().
    > 

Node Tools for Designers
------------------------

For generating CSS scripts from LESS and LESS developing you should install node tools:

    $ sudo apt-get install python-software-properties
    $ sudo add-apt-repository ppa:richarvey/nodejs
    $ sudo apt-get update
    $ sudo apt-get install nodejs
    $ sudo apt-get install npm
    $ sudo npm install uglify-js -g

Credits
-------

* Stan Podolski
* Max Podolski
* Andrii Zadorozhnii

OM A HUM
