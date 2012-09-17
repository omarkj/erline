Erline
======
Erline is a simple API to create and (soon) manage pipelines in Erlang

Reason
------
At [Kóði](http://github.com/KodiEhf/) we noticed we were writing a lot of pipeline code over and over again and `erline` is a way for us to manage pipelines is a simpler and easier manner.
`erline` makes it easy to create code that should run sequentially or concurrently and takes advantage of the how easy it is to create and destroy processes in Erlang.

`erline` also makes it easy to create pipeline dynamically.

Status
------
`erline` is still relatively young and probably has a bunch of bugs yet to be discovered. We are not using it in production yet, but that should change in the next two weeks.

Terminology
-----------
A pipeline in `erline` contains the following things

* Type
 * sequential
 * concurrent
* Actions
 * Module
 * Function
 * Pipelines

Getting Started
---------------
`erline` does nothing by default, to get started a pipeline needs to be created and started.

Below is a very simple example that runs a pipeline with two actions, these actions are run sequentially with the output from the first one being the input into the next one.

``` erlang
ok = application:start(erline).
Line = erline:create(sequential, [fun(X) -> X * 10 end,
                                  fun(X) -> X * 10 end], []).
1500 = erline:sync(Line, 15).
```

Features
--------
`erline` can currently create two types of pipelines; sequential and concurrent. Many pipelines can be started together and the output of the first pipeline will then be a the input in the next one.
Each pipeline has an arbitrary number of actions, these actions can be Modules, Functions or other pipelines.

To-do
-----
`erline` is still under active development and these are the features I'm working on now

* Better error handling
* Warm pipelines for pooling
* Monitoring API
* Some supervisor refactoring

I am also looking into how actions and pipelines could be distributed to other nodes, but don't hold your breath waiting for that.