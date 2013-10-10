:- protocol(labeller).

:- info([
       comment is 'Provides labeling information about a graph.',
       author is 'Daniel Lyons',
       date is 2013/10/10]).

:- public(node_label/2).
:- mode(node_label(+node, ?label), zero_or_one).
:- info(node_label/2, [comment is 'Unify Label with a label for Node']).

:- public(node_attrs/2).
:- mode(node_attrs(+node, ?list), zero_or_one).
:- info(node_attrs/2, [comment is 'Unify List with label attributes for Node']).

:- public(edge_attrs/2).
:- mode(edge_attrs(+edge, ?list), zero_or_one).
:- info(edge_attrs/2, [comment is 'Unify List with edge attributes for Edge']).

:- end_protocol.
