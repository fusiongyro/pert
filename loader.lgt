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
