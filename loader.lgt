:- use_module(library(dcg/basics)).

:- initialization(logtalk_load([
				      '$HOME/logtalk/examples/roots/loader',
				      library(all_loader)])).

:- op(500, xfx, depends_on).
:- set_logtalk_flag(dynamic_declarations, allow).

:- initialization(logtalk_load([
				      timeCalculator,
				      activity,
				      expected,
				      gaussian,
				      gamma,
				      beta,
				      timeProvider,
				      timeReceiver,
				      dependencyProvider,
				      dependencyReceiver,
				      dependencyTracking,
				      labeller,
				      graphviz,
				      pert,
				      project,
				      csvParser,
				      main])).
