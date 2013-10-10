:- protocol(labeller).

:- public(node_label/2).
:- mode(node_label(+atom, ?atom), zero_or_one).

:- public(node_attrs/2).
:- mode(node_attrs(+atom, ?list), zero_or_one).

:- public(edge_attrs/2).
:- mode(edge_attrs(+atom, ?list), zero_or_one).

:- end_protocol.
