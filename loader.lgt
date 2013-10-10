info([
	    author 	is 'Daniel Lyons',
	    date 	is 2013/10/10,
	    comment is
	    	'This is PERT in Logtalk.']).

:- use_module(library(dcg/basics)).

:- initialization(logtalk_load([
				      '$HOME/logtalk/examples/roots/loader',
				      library(metap),
				      library(meta),
				      library(types_loader),
				      library(metapredicates_loader)])).

:- op(500, xfx, depends_on).
:- set_logtalk_flag(dynamic_declarations, allow).

:- initialization(logtalk_load([
				      timeProvider,
				      timeReceiver,
				      dependencyProvider,
				      dependencyReceiver,
				      labeller,
				      pert])).
